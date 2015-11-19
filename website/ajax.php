<?php
        /* Start a new session or resume if the client has a cookie ;) */
        session_start();
        require_once ("request.php");
        
        /* Only handle if the search term is specified */
        if (isset($_GET['search']) && $_GET['search'] != "") {          
			$search = $_GET['search'];
			$request_data = build_request_data();
			$output = request($search, $request_data);
        } 
?>