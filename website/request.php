<?php

/*
 * TODO: remember last server that replied in time: implement when we call next_server_index
 * TODO: then read from this when we call get_server_index, and comment these lines
 * TODO: consider splitting into one file that helps building options/data,
 * and one for more low-level connection stuff
 *
 * This file is responsible for handing off the search to the erlang backend.
 * Usage: generate request options with build_request_options() if you want to.
 * Make the request with request().
 *
 * We use cURL for the actual request.
 * We pass some options and details that we store as "user habit data",
 * see further comments below and in documentation on the format.
 *
 * NOTE: Make sure this file does not have ANY text/characters outside the php tags!
 */

// This file uses an external library to parse the user agent string into subparts.
require_once ("lib/UserAgentParser.php");
require_once ("config.php");


/**
 * This class maintains an ordered list of backend servers.
 * When we first ask for the preferred server, it gives us the topmost entry
 * in the backend_servers array in the config. However, if requests to this one
 * times out or has problems, the using code can call preferred_server_down() 
 * to tell this module to adjust the order of the list of backend servers.
 * @author jerker
 *
 */
class server_manager {
	var $server_array = [];
	var $servers_tried = 1;
	var $server_filename = "servers.order";
	
	/**
	 * Constructor. 
	 */
	function __construct() {
		global $config;
		// Read the file into array if posisble. It is already ordered.
		$this->server_array = $this->load_file();

		// If there was trouble with the file, load_file has returned false.
		if (!$this->server_array) {
			
			$this->server_array = [];
			
			// Let's create the key-value array based on the configuration
			foreach ($config['backend_servers'] as $currentserver) {
				$this->server_array[$currentserver] = 0;
				error_log("Adding server " . $currentserver);
			}
			// ...and save it
			$this->save_file();			
		}
	}

	
	/**
	 * Return the preferred server, if the list contains at least one entry
	 * and the using code hasn't gone through all the available servers.
	 */
	public function get_preferred_server() {
		global $config;
		
		$preferred_server = false;
		if (sizeof($this->server_array) > 0 && 
				$this->servers_tried <= sizeof($config['backend_servers'])) {
			$preferred_server = $this->get_topmost_address();
		}

		error_log("Preferred server:  " . serialize($preferred_server));
		
		return $preferred_server;
	}
	
	/**
	 * Call this if there are problems with the preferred server.
	 */
	public function preferred_server_down() {

		error_log("NOTE: Preferred server down called!");
		
		// Keep track of how many servers we've tried so far (during the 
		// lifetime of this object)
		$this->servers_tried++;	

		// Put the current timestamp as the value for the current server
		$this->server_array[$this->get_topmost_address()] = time();
		
		// Sort the array in ascending order - servers with 0 known instances of
		// being down or downtime long ago (likely up again) will be further up
		asort($this->server_array);
		
		// Write to the servers.order file
		$this->save_file();	
	}
	
	private function get_topmost_address() {		
		$topmost_address = "";

		error_log("Current server array: " . serialize($this->server_array));
		if (sizeof($this->server_array) > 0 && is_array($this->server_array)) {
			// "Reset" the array to ensure we get the first entry 
			reset($this->server_array);			
			// Then take the first key of the array
			$topmost_address = key($this->server_array);
		}

		return $topmost_address;		
	}
	


			
	
	/**
	 * Read from the servers.order file and see if it exists and contains all
	 * the servers. If so, return the key-value array. If not, return false.
	 */
	private function load_file() {
		$result = false;
		if (file_exists($this->server_filename)) {
			// Read the file where the array is stored in it's serialized form.
			// Unserialize to get it as an array again 
			$array_result = unserialize(file_get_contents($this->server_filename));
			
			// Only return the loaded array if all entries (keys) match the config
			if ($this->has_configured_servers($array_result)) {
				$result = $array_result;				
			}
		}
		return $result;
	}
	
	
	/**
	 * Make sure all servers from the config (and ONLY those) is present in
	 * the array given as the argument 
	 */
	private function has_configured_servers($proposed_server_array) {
		global $config;
		
		if (is_array($proposed_server_array)) {
			// The server addresses are keys in the server array		
			$proposed_servers = array_keys($proposed_server_array);
			
			// Check equality between arrays. === returns true if the arrays
			// have the same key-value pairs
			return ($config['backend_servers'] === $proposed_servers);
		} else {
			return false;
		}
	}
	
	
	/**
	 * Save the current order and timestamps.
	 */
	private function save_file() {

		error_log("Saving... ");
		if (sizeof($this->server_array > 0)) {
			// Serialize and save to file
			file_put_contents($this->server_filename, serialize($this->server_array));
		}
	}		
}

/*
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
function build_request_options($request_type = "search", $services = null, $content_type = null, $language = null) {
	
	// Set up the parameter array
	$options = array (
		'request_type' => $request_type,
		'services' => $services,
		'content_type' => $content_type,
		'language' => $language 
	);
	// Filter out any null values so they're not even sent
	$options = array_filter ( $options );
	
	return $options;
}

/*
 * Makes a request to the erlang backend and returns the result (as JSON).
 * $search: the search term
 * $options: associative array with options. Use build_request_options for convenience.
 */
function request($term, $options) {
	global $config;
	
	// Filter out any null values in the options
	$options = array_filter ( $options );
	
	// Put together the request body in the [options, user habit data] format
	$habit_data = _get_user_habit_data ($term);
	$request_body = json_encode ( [ 
		$options,
		$habit_data 
	] );
	
	$backend_timeout = $config ['backend_timeout'];
	if ($options ['request_type'] == "stats") {
		// An exception - higher timeout for stats request
		$backend_timeout = 60;
	}
	
	$server_manager = new server_manager();
	// On problems, retry with a new server as long as the server manager proposes more servers
	$server_address = $server_manager->get_preferred_server();
	$output = false;
	$limit = 0;
	do {
		$limit++;
		$output = _query_server ($server_address, $term, $request_body, $backend_timeout);
		
		// If timeout or error occured, try the next backend server
		if (!$output) {
			$server_manager->preferred_server_down();
			$server_address = $server_manager->get_preferred_server();
		}		
		error_log("server adress: " . $server_address);
		
		// Repeat as long as content is invalid and there are more
		// servers to try
	} while (!$output && $server_address != false && $limit < 4);
	
	return $output;
}


/*
 * Makes a concrete request to the specified server - returns false on error or timeout
 */
function _query_server($address, $term, $request_body, $backend_timeout) {
	global $config;
	
	// Error check: die if cURL isn't available
	if (! function_exists ( 'curl_init' )) {
		die ( "Sorry cURL is not installed!" );
	}
	$ch = curl_init ();
	
	// cURL should return the info instead of printing it
	curl_setopt ( $ch, CURLOPT_RETURNTRANSFER, TRUE );
	
	// cURL should timeout after configured time
	curl_setopt ( $ch, CURLOPT_TIMEOUT, $backend_timeout );
	// cURL should POST the JSON in $request_data
	curl_setopt ( $ch, CURLOPT_POST, 1 );
	curl_setopt ( $ch, CURLOPT_POSTFIELDS, $request_body );
	
	// Set the URL, including urlencoded (helps if there are special characters) search term
	curl_setopt ( $ch, CURLOPT_URL, $address . "/" . urlencode ( _clean_searchterm ( $term ) ) );
	
	// Execute the request
	$output = curl_exec ( $ch );
	
	// Return false on timeout or other error (errno 0 means no error)
	if (curl_errno ( $ch ) != 0) {
		$output = false;
	}
	curl_close ( $ch );
	
	return $output;
}

/*
 * Put together the user habit data associative array that will later be part
 * of the [options, user habit data] JSON object sent to the erlang backend
 */
function _get_user_habit_data($term) {
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

/*
 * Remove special characters from string using regex.
 */
function _clean_searchterm($string) {
	return preg_replace ( '/[^A-Öa-ö1-9\-\_\+ ]/', '', $string ); // Removes special chars.
}
?>
