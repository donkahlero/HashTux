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
 * $options: associative array with options. Use build_request_options for convenience.  
 */
function request($term, $options) {
	// Filter out any null values in the options
	$options = array_filter($options);
	
	// Put together the request body in the [options, user habit data] format
	$habit_data = get_user_habit_data();
	$request_body = json_encode([$options, $habit_data]);
	
	// Start curl if the module is installed, otherwise stop execution
	if (!function_exists('curl_init')){
    	die('Sorry cURL is not installed!');
    }
    $ch = curl_init();

	// Tell cURL to return the info instead of printing it
    curl_setopt ($ch, CURLOPT_RETURNTRANSFER, TRUE);
    
    // Tell cURL we want to POST the JSON in $request_data
    curl_setopt($ch, CURLOPT_POST, 1);
    curl_setopt($ch, CURLOPT_POSTFIELDS, $request_body);
    
	// Set the URL, including urlencoded (helps if there are special characters) search term
    curl_setopt ($ch, CURLOPT_URL, "http://localhost:8080/" . urlencode(clean($term)));
    
	// Execute the request
	$output = curl_exec($ch);  
    curl_close($ch);
	return $output;
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
 * Put together the user habit data associative array that will later be part
 * of the [options, user habit data] JSON object sent to the erlang backend
 */
function get_user_habit_data() {
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
function clean($string) {
   return preg_replace('/[^A-Öa-ö1-9\-\_\+ ]/', '', $string); // Removes special chars.
}
?>	
