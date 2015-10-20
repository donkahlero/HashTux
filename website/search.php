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
        
        <p align="center">You searched for:</p>

        
        <?php 
    $search = $_GET['search'];
    if (!function_exists('curl_init')){
    	die('Sorry cURL is not installed!');
    }
   
    $ch = curl_init();
    curl_setopt ($ch, CURLOPT_URL,"http://localhost:8080/" . $search);
     
    $output = curl_exec($ch);
     
    echo($output);
     
    curl_close($ch);
    
   
   ?>
    </body>
</html>

