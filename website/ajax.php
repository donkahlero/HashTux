<?php
        /* Start a new session or resume if the client has a cookie ;) */
        session_start();

        require_once("curl_request.php");
         
        /* Only handle if the search term is specified */
        if (isset($_GET['search']) && $_GET['search'] != "") {
            echo curl_request($_GET['search']);
        } 
?>