<?php
	/**
	 * Fetches Twitter trending searches. 
	 * Uses TwitterOauth library and a major part of this file is borrowed from the example
	 * at https://github.com/ricardoper/TwitterOAuth/blob/v1/Example_app-only.php
	 * 
	 * I just wanted a quick way to get the popular searches from Twitter, without having
	 * to mess with introducing a completely new task for the backend server  : )) // J
	 * 
	 * Main structure:
	 * 	- If there is no recent cache file, try reading from Twitter
	 *  - If this fails or if there was a recent cache file, 
	 *  	read from cache file provided it exists
	 */
	 
	require_once "lib/TwitterOAuth/TwitterOAuth.php";
	require_once "lib/TwitterOAuth/Exception/TwitterException.php";	
	require_once "conf/config.php";

	use TwitterOauth\TwitterOAuth;
	date_default_timezone_set('UTC');

	$cache_filename = "state/twitter_trends";
	$trends_json = "";

	// If there is no recent cache file, try reading from twitter 
	// (By recent we currently mean modification time < half an hour ago)
	if (!(file_exists($cache_filename) && filemtime($cache_filename) > time() - 1800)) {

		// Read from Twitter and write to the cache file		
		try {
			// Instantiate TwitterOAuth class with API tokens
			$connection = new TwitterOAuth($config['twitter_api_key']);
			// Get an application-only token
			$bearer_token = $connection->getBearerToken();
		
			$params = array(
					'id' => 1, // Means the whole world
					'exclude' => true // Means exclude the # sign
			);
		
			// Ask for the trends
			$trends_json = $connection->get('trends/place', $params);
				
			// json_encode in this context is not used to TURN IT INTO JSON, it already
			// is, but rather to encode any special characters so they won't cause any
			// trouble when being output
			$trends_json = json_encode($trends_json);
				
			// Write to the cache file
			file_put_contents($cache_filename, $trends_json);
			
		} catch (Exception $e) {
			// Fetch failed - fallback to file
			$trends_json = "";
		}				
	} 	
 
	if ($trends_json == "") {
		// Either recent cache file is available, OR the Twitter fetch failed and
		// we should fallback to file anyway.
		if (file_exists($cache_filename)) {
			$trends_json = file_get_contents($cache_filename);
		}
	}

	echo $trends_json;
?>
