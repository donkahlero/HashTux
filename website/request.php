<?php 

/*
 * TODO: Fix indentation in this file
 * TODO: remember last server that replied in time: implement when we call next_server_index
 * TODO: then read from this when we call get_server_index, and comment these lines
 * TODO: consider splitting into one file that helps building options/data,
 *		and one for more low-level connection stuff
 *
 * This file is responsible for handing off the search to the erlang backend.
 * Usage: generate request options with build_request_options() if you want to.
 * Make the request with request().
 * 
 * We use cURL for the actual request. 
 * We pass some options and details that we store as "user habit data",
 * see further comments below and in documentation on the format.
 */
 

// This file uses an external library to parse the user agent string into subparts.
require_once("lib/UserAgentParser.php"); 
require_once("config.php");


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
function build_request_options($request_type = "search", $services = null,
		$content_type = null, $language = null) {

	// Set up the parameter array
	$options = array(
			'request_type' => $request_type,
			'services' => $services,
			'content_type' => $content_type,
			'language' => $language,
	);
	// Filter out any null values so they're not even sent
	$options = array_filter($options);
      
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
	$options = array_filter($options);
	
	// Put together the request body in the [options, user habit data] format
	$habit_data = _get_user_habit_data();
	$request_body = json_encode([$options, $habit_data]);

	// Try the first server first, then cycle through the array if timeout or errors occur
	$server_index = _get_server_index();
	$attempts = 0;
	do {
		$output = _query_server($config['backend_servers'][$server_index], $term, $request_body);
		$attempts++;

		// If timeout or error occured, try the next backend server
		if (!$output) {
			$server_index = _next_server_index($server_index);
		}	

		// If necessary, retry until we've tried all servers
	} while (!$output && $attempts < sizeof($config['backend_servers']));

	return $output;
}


function _get_server_index() {
	return 0;
}

function _next_server_index($index) {
	global $config;
	$index++;
	if ($index >= sizeof($config['backend_servers'])) {
		$index = 0;
	}
	return $index;
}


/*
 * Makes a concrete request to the specified server - returns false on error or timeout
 */
function _query_server($address, $term, $request_body) {
	global $config;

	// Error check: die if cURL isn't available
	if (!function_exists('curl_init')){
   	die("Sorry cURL is not installed!");
	}
	$ch = curl_init();

	// cURL should return the info instead of printing it
  curl_setopt ($ch, CURLOPT_RETURNTRANSFER, TRUE);
  
	// cURL should timeout after configured time
	curl_setopt($ch, CURLOPT_TIMEOUT, $config['backend_timeout']);
  // cURL should POST the JSON in $request_data
  curl_setopt($ch, CURLOPT_POST, 1);
  curl_setopt($ch, CURLOPT_POSTFIELDS, $request_body);
    
	// Set the URL, including urlencoded (helps if there are special characters) search term
  curl_setopt($ch, CURLOPT_URL, $address . "/" . urlencode(_clean_searchterm($term)));
    
	// Execute the request
	$output = curl_exec($ch);  

	// Return false on timeout or other error
	if (curl_errno($ch)) {
		$output = false;
	}
	curl_close($ch);

	return $output;
}


/*
 * Put together the user habit data associative array that will later be part
 * of the [options, user habit data] JSON object sent to the erlang backend
 */
function _get_user_habit_data() {
	// Parse the user agent string so we get the components we need
	$user_agent_strings = parse_user_agent();
	$browser = $user_agent_strings['browser'];
	$browser_version = $user_agent_strings['version'];
	$platform = $user_agent_strings['platform'];
	
	$user_habit_data = array(
			'timestamp' => time(),
			'session_id' => session_id(),
			'ip_address' => $_SERVER['REMOTE_ADDR'],
			'browser' => $browser,
			'browser_version' => $browser_version,
			'platform' => $platform,
	
	);
	return $user_habit_data;
}


/*
 * Remove special characters from string using regex. 
 */
function _clean_searchterm($string) {
   return preg_replace('/[^A-Öa-ö1-9\-\_\+ ]/', '', $string); // Removes special chars.
}
?>	
