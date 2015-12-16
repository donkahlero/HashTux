<?php
	// Uses TwitterOauth library and a major part of this file is borrowed from the example
	// at https://github.com/ricardoper/TwitterOAuth/blob/v1/Example_app-only.php
	// Just wanted a quick way to get the popular searches on twitter : )) // J
	require_once "lib/TwitterOAuth/TwitterOAuth.php";
	require_once "lib/TwitterOAuth/Exception/TwitterException.php";	
	require_once "conf/config.php";

	use TwitterOauth\TwitterOAuth;
	date_default_timezone_set('UTC');

	// Instantiate TwitterOAuth class with API tokens
	$connection = new TwitterOAuth($config['twitter_api_key']);
	// Get an application-only token
	$bearer_token = $connection->getBearerToken();

	$params = array(
		 'id' => 1, // Means the whole world
		 'exclude' => true // Means exclude the # sign
	);
	$response = $connection->get('trends/place', $params);
	
	// json_encode in this context is not used to TURN IT INTO JSON, it already
	// is, but rather to encode any special characters in a way that they're JSON-compliant.
	echo json_encode($response);
?>
