<?php
/**
 * This file is responsible for handing off search or stats requests to the erlang 
 * backend. Usage: generate request options with build_request_options() if you want to,
 * otherwise just prepare an associative array with options. 
 * Make the request with make_request().
 *
 * We use cURL for the actual request.
 * We pass some options and details that we store as "user habit data",
 * see further comments below and in documentation on the format.
 *
 * NOTE: Make sure this file does not have ANY text/characters outside the php tags!
 * 
 * (On refactoring, it could be a good idea to make this a bit more object-oriented-ish,
 * for example stop having the functions static, and let the request options be set as an
 * instance variable of a "request" object, etc. For now I just wanted to wrap related 
 * functions into a class.)  
 */

// This file uses an external library to parse the user agent string into subparts.
require_once ("lib/UserAgentParser.php");
require_once ("conf/config.php");
require_once ("include/server_manager.php");


class request {

	/**
	 * Builds an associative array of all the options that need to be passed
	 * to the erlang server to make a request.
	 *
	 * $request_type: "search" or "update" - use update from ajax or other subsequent requests.
	 * $services: array of strings, ex: ["twitter"]. supply empty list for all services.
	 * $content_type: array of strings, ex ["images", "videos"]. empty list for all.
	 * $language specified as a two character string
	 *
	 * Unspecified fields are set to null by default and then omitted in the
	 * resulting associative array
	 */
	public static function build_request_options($request_type = "search", $services = null,
			$content_type = null, $language = null) {
		
		// Set up the parameter array
		$options = array (
			'request_type' => $request_type,
			'services' => $services,
			'content_type' => $content_type,
			'language' => $language 
		);
		// Filter out any null values so they're not even sent
		$options = array_filter($options);
		
		return $options;
	}
	
	
	/**
	 * Makes a request to the erlang backend and returns the result (as JSON).
	 * $search: the search term
	 * $options: associative array with options. Use build_request_options for convenience.
	 */
	public static function make_request($term, $options) {
		global $config;
		
		// Filter out any null values in the options
		$options = array_filter ($options);
		
		// Put together the request body in the [options, user habit data] format
		$habit_data = request::get_user_habit_data ($term);
		$request_body = json_encode ([$options, $habit_data]);
		
		$backend_timeout = $config ['backend_timeout'];
		if ($options ['request_type'] == "stats") {
			// An exception - higher timeout for stats request
			$backend_timeout = 60;
		}
		
		// The server_manager class keeps track of which servers are available
		$server_manager = new server_manager();
		$server_address = $server_manager->get_pref_server_addr();
		// Assume invalid reply from server
		$reply = false;
		do {
			$reply = request::query_server($server_address, $term, $request_body, $backend_timeout);
			
			// If timeout or error occured, try the next backend server
			// \"no_alloc\" means the miner_server was too busy, this is within " because
			// it's encoded by jsx before it's sent out
			if (!$reply || $reply == "\"no_alloc\"") {
				$server_manager->preferred_server_down();
				$server_address = $server_manager->get_pref_server_addr();
			}		
	
			// Repeat as long as content is invalid and another server was proposed by
			// server_manager
		} while (!$reply && $server_address != false);
		
		return $reply;
	}
	
	
	/**
	 * Makes a concrete request to the specified server - returns false on error or timeout
	 */
	private static function query_server($address, $term, $request_body, $backend_timeout) {
		global $config;
		
		// Error check: die if cURL isn't available
		if (! function_exists ('curl_init')) {
			die ("Sorry cURL is not installed!");
		}
		$ch = curl_init ();
		
		// cURL should return the info instead of printing it
		curl_setopt ($ch, CURLOPT_RETURNTRANSFER, TRUE);
		
		// cURL should timeout after configured time
		curl_setopt ($ch, CURLOPT_TIMEOUT, $backend_timeout);
		// cURL should POST the JSON in $request_data
		curl_setopt ($ch, CURLOPT_POST, 1);
		curl_setopt ($ch, CURLOPT_POSTFIELDS, $request_body);
		
		// Set the URL, including urlencoded (helps if there are special characters) search term
		curl_setopt ($ch, CURLOPT_URL, $address . "/" . urlencode(request::clean_searchterm($term)));

		// Execute the request
		$output = curl_exec ($ch);
		
		// Return false on timeout or other error (errno 0 means no error)
		if (curl_errno ($ch) != 0) {
			$output = false;
		}
		curl_close ($ch);
		
		return $output;
	}
	
	
	/**
	 * Put together the user habit data associative array that will later be part
	 * of the [options, user habit data] JSON object sent to the erlang backend.	 
	 */
	private static function get_user_habit_data($term) {
		// Parse the user agent string so we get the components we need
		$user_agent_strings = parse_user_agent ();
		$browser = $user_agent_strings ['browser'];
		$browser_version = $user_agent_strings ['version'];
		$platform = $user_agent_strings ['platform'];
		
		$user_habit_data = array (
			'term' => $term,
			'timestamp' => time (),
			'session_id' => session_id (),
			'ip_address' => $_SERVER ['REMOTE_ADDR'],
			'browser' => $browser,
			'browser_version' => $browser_version,
			'platform' => $platform 
		)
		;
		return $user_habit_data;
	}
	
	
	/**
	 * Remove special characters from string using regex.
	 */
	private static function clean_searchterm($string) {
		return preg_replace ('/[^\p{Common}\p{Latin}1-9\-\_\+ ]/', '', $string); // Removes special chars.
	}
}
?>
