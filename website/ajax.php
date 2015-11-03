<?php
	/* Start a new session or resume if the client has a cookie ;) */
	session_start();

	require_once("curl_request.php");

	echo curl_request($_GET['search'];
?>
