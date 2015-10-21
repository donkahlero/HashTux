<html lang="en">
    <head>
        
        <title>HashTux</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        
        <link href="css/bootstrap.css" rel="stylesheet">
        
        <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
        <script src="js/bootstrap.min.js"></script>
          
    </head>
    
    <body>
 <?php 
 
    $search = $_GET['search'];
    if (!function_exists('curl_init')){
    	die('Sorry cURL is not installed!');
    }
   
    $ch = curl_init();
    curl_setopt ($ch, CURLOPT_RETURNTRANSFER, TRUE);
    curl_setopt ($ch, CURLOPT_URL,"http://localhost:8080/" . urlencode($search));
     
    $output = curl_exec($ch);
    $output = str_replace("\n", "<br />", $output);
      
     
    curl_close($ch);
   ?>     
   
    <div class="container">
    
  		<div class="jumbotron">
  		
    		<h1 align="center">You searched for:</h1>
    		<p align="center"> #<?php echo $search?> </p>
    		<h2 align="center">Result from the Erlang backend:</h2>
    		<br />
   			<p align="center"> <?php echo $output?> </p>
   					 
   	
  		</div>
    </div>
              

             
 
    </body>
</html>

