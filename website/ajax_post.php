<?php
	/*
	 * See ajax.php for more details. This is a specialised version of that
	 * file that allows a more general query (not only search queries).
	 * Any options supplied as json will be sent straight through to the
	 * Erlang backend (provided it's well-formatted enough to be parsed to
	 * PHP here).
	 * 
	 * Using this requires providing the request type parameter in the options
	 * object!
	 *
	 * NOTE: Make sure this file does not have ANY text/characters outside the php tags!
	 */
	
	/* Start a new session or resume if the client has a cookie ;) */
	session_start();
	require_once ("request.php");
        
	/* Here we take an options object for the search as the POST body. */
	$options_json = file_get_contents('php://input');
        
	/* We see if this is proper JSON */
	$options = json_decode($options_json, true);
	
	/* Only handle if the search term is specified */
	if (isset($_GET['search']) && $_GET['search'] != "") {          
		$search = $_GET['search'];
		echo request($search, $options);
	} 
?>
