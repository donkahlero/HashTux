<?php 
/*
 * This file is responsible for handing off the search to the erlang backend.
 * It uses cURL for the actual request. It also passes some options and details
 * that we store as "user habit data".
 * 
 * This file uses an external library to parse the user agent string into subparts.
 */
require_once 'lib/UserAgentParser.php'; 


/*
 * $search: the search term
 * $services: array of strings, ex: ["twitter"]. supply empty list for all services.
 * $language specified as a two character string
 * $content_type: array of strings, ex ["images", "videos"]. empty list for all.
 * $request_type: "search" or "update" - use update from ajax or other subsequent requests.
 * 
 */
function curl_request($search, $request_type = "search", $language = "en",
		$services = [], $content_type = []) { 
	// Parse the user agent string into an array of separate values
	$user_agent_strings = parse_user_agent(); 
	
	// Start curl if the module is installed, otherwise stop execution
	if (!function_exists('curl_init')){
    	die('Sorry cURL is not installed!');
    }
    $ch = curl_init();

	// Tell cURL to return the info instead of printing it
    curl_setopt ($ch, CURLOPT_RETURNTRANSFER, TRUE);
	
	// Set up the parameter array
	$params = array('timestamp' => time(),
			'session_id' => session_id(), 
			'ip_address' => $_SERVER['REMOTE_ADDR'],			
			'browser' => $user_agent_strings['browser'],
			'browser_version' => $user_agent_strings['version'],
			'platform' => $user_agent_strings['platform'],
			'request_type' => $request_type,
            'services' => json_encode($services),
			'content_type' => json_encode($content_type)		
	);
  
	// Set the URL, including urlencoded (helps if there are special characters) search term
    curl_setopt ($ch, CURLOPT_URL, "http://localhost:8080/" 
			. urlencode($search) 
			. "?" 
			. http_build_query($params));
    
	// Execute the request
	$output = curl_exec($ch);  
    curl_close($ch);
	return $output;
}




?>	
