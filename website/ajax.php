<?php
	/**
	 * Used for making AJAX calls to the Erlang backend. By querying this file
	 * and therefore using the request method defined in request.php instead of
	 * directly connecting to the backend, we can append some details
	 * to the request (such as user habit data.
	 * 
	 * So obviously we just ouput the response (JSON from Erlang backend) and
	 * nothing else from this file.
	 * NOTE: Make sure this file does not have ANY text/characters outside the php tags!
	 */

	/* Start a new session or resume if the client has a cookie ;) */
	session_start();
	require_once ("include/request.php");

	/* See if request type is specified, otherwise assume update */
	if (isset($_GET['request_type'])) {
		$request_type = $_GET['request_type'];
	} else {
		$request_type = "update";
	}
	
	/* Only handle if the search term is specified */
	if (isset($_GET['search']) && $_GET['search'] != "") {
		$search = $_GET['search'];
		$options = request::build_request_options($request_type);
		echo request::make_request($search, $options);
	}
?>
