<?php
       /* Start a new session or resume if the client has a cookie ;) */
        session_start();
        require_once ("request.php");
        
        /* Here we take an options object for the search as the POST body. */
        $options_json = file_get_contents('php://input');
        
        /* We see if this is proper JSON */
        $options = json_decode($options_json, true);
        
        /* See if request type is specified, otherwise assume update *//*
        if (isset($_GET['request_type'])) {
            $request_type = $_GET['request_type'];
        } else {
            $request_type = "update";
        } */
        
        /* Only handle if the search term is specified */
        if (isset($_GET['search']) && $_GET['search'] != "") {          
            $search = $_GET['search'];
            $request_data = build_custom_request($options);
            echo request($search, $request_data);
        } 
?>