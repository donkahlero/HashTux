<?php 

function curl_request($search) { 
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
			'language' => $_SERVER['HTTP_ACCEPT_LANGUAGE'],
			'user_agent' => $_SERVER['HTTP_USER_AGENT']);
  
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
