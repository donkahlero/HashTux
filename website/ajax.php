<?php
        /* Start a new session or resume if the client has a cookie ;) */
        session_start();
        require_once ("request.php");
        
        /* See if request type is specified, otherwise assume update */
        if (isset($_GET['request_type'])) {
            $request_type = $_GET['request_type'];
        } else {
            $request_type = "update";
        } 
        
        /* Only handle if the search term is specified */
        if (isset($_GET['search']) && $_GET['search'] != "") {          
            $search = $_GET['search'];
            $request_data = build_request_data($request_type);
            echo request($search, $request_data);
        } 
?>