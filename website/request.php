<?php 
/*
 * This file is responsible for handing off the search to the erlang backend.
 * Usage: generate request data with build_request_data(). Make request with request().
 * 
 * We use cURL for the actual request. 
 * We pass some options and details that we store as "user habit data",
 * see further comments below and in documentation on the format.
 */
 

// This file uses an external library to parse the user agent string into subparts.
require_once 'lib/UserAgentParser.php'; 


/*
 * Makes a request to the erlang backend and returns the result (as JSON).
 * $search: the search term
 * $request_data: options and user habit data. Use build_request_data() to generate.	  
 */
function request($search, $request_data) { 
	
	// Start curl if the module is installed, otherwise stop execution
	if (!function_exists('curl_init')){
    	die('Sorry cURL is not installed!');
    }
    $ch = curl_init();

	// Tell cURL to return the info instead of printing it
    curl_setopt ($ch, CURLOPT_RETURNTRANSFER, TRUE);
    
    // Tell cURL we want to POST the JSON in $request_data
    curl_setopt($ch, CURLOPT_POST, 1);
    curl_setopt($ch, CURLOPT_POSTFIELDS, $request_data);
    
	// Set the URL, including urlencoded (helps if there are special characters) search term
    curl_setopt ($ch, CURLOPT_URL, "http://localhost:8080/" . urlencode($search));
    
	// Execute the request
	$output = curl_exec($ch);  
    curl_close($ch);
	return $output;
}


/*
 * Builds the JSON representation of all the options and data that needs to be passed
 * to the erlang server to make a request. 
 * 
 * $request_type: "search" or "update" - use update from ajax or other subsequent requests.
 * $services: array of strings, ex: ["twitter"]. supply empty list for all services.
 * $content_type: array of strings, ex ["images", "videos"]. empty list for all.
 * $language specified as a two character string
 *
 * Format of the resulting object will be [OPTIONS, USER_HABIT_DATA] where
 * OPTIONS and USER_HABIT_DATA each are lists of key-value tuples.
 *
 */
function build_request_data($request_type = "search", $services = [],
		$content_type = [], $language = "en") {

	// Parse the user agent string so we get the components we need
	$user_agent_strings = parse_user_agent();
	$browser = $user_agent_strings['browser'];
	$browser_version = $user_agent_strings['version'];
	$platform = $user_agent_strings['platform'];

	// Set up the parameter array
	$options = array(
			'request_type' => $request_type,
			'services' => $services,
			'content_type' => $content_type,
			'language' => $language,
	);
	$user_habit_data = array(
			'timestamp' => time(),
			'session_id' => session_id(),
			'ip_address' => $_SERVER['REMOTE_ADDR'],
			'browser' => $browser,
			'browser_version' => $browser_version,
			'platform' => $platform,
				
	);

	$options = distinguish_tuples($options);
	$user_habit_data = distinguish_tuples($user_habit_data);

	return json_encode([$options, $user_habit_data]);
}


/*
 * Converts a PHP associative key-value array into an array with key-value
 * objects that is as close as possible to Erlangs "list of tuples".
 *
 * Where an associative array in PHP would be encoded in JSON as
 * {"key1" : "value1", "key2" : "value2"}
 *
 * we instead get
 *
 * [{"key1" : "value1"}, {"key2" : "value2"}]
 *
 * if we encode the output of this method.
 *
 * (Because PHP doesn't really have tuples, the easiest way to convert it reasonably
 * is to use an associative array in PHP... FOR EACH key-value pair! Since we need 
 * one ass. array for each, i just use a two-dimensional one where the first isn't
 * associative (just uses numbers), hence we end up with "a list of tuples"
 * if we convert this outer array to json.)
 * http://php.net/manual/en/function.json-encode.php
 */
function distinguish_tuples($array) {
	$tuples = [];
	foreach ($array as $key => $value) {
		// Add each value with the corresponding key as the only element
		// for that index in the leftmost array []
		$tuples[sizeof($tuples)][$key] = $value;
	}
	return $tuples;
}
?>	
