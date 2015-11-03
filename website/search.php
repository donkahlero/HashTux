<?php
	/* Start a new session or resume if the client has a cookie ;) */
	session_start();
?>
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
	/* 
   if (!function_exists('curl_init')){
    	die('Sorry cURL is not installed!');
    }
   
    $ch = curl_init();
    curl_setopt ($ch, CURLOPT_RETURNTRANSFER, TRUE);

    curl_setopt ($ch, CURLOPT_URL,"http://localhost:8080/" . urlencode($search));
	curl_close($ch);
	  $output = curl_exec($ch);
   
	*/
    
	require_once("curl_request.php");
    $search = $_GET['search'];
 
	$output = curl_request($search);
	 
   	$output = str_replace("\n", "<br />", $output);
      

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

    <script type='text/javascript'>
    
​  ​
function json_parsing(search)
{
	var url = 'http://localhost:8080/ajax.php?search=' + encodeURI(search);
	var response = "";
    var form_data = {
        username: username,
        resource_link: resource_link,
        text: text
    };
    $.ajax({
        type: "json", 
        url:url, 
        data: form_data,
        success: function(response)
        {

            console.log(response);
            
            var json_obj = $.parseJSON(response);//parse JSON
            
            var output="<ul>";
            for (var i in json_obj) 
            {
                output+="<li>" + json_obj[i].username + ",  " + json_obj[i].resource_link + ", " + json_obj[i].text "</li>";
            }
            output+="</ul>";
            
            $('span').html(output);
        },
        dataType: "json"//set to JSON    
    })    
}            
    </script>
    
    </body>
</html>

