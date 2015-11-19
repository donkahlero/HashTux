<?php
	/* Start a new session or resume if the client has a cookie ;) */
	session_start();
?>
<html lang="en">
  <head>   
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    
    <title>HashTux</title>
    
    <link href="css/bootstrap.css" rel="stylesheet">
    
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
    <script src="js/bootstrap.min.js"></script>  
    
    <script>
    function checkInput()
    {
        var input = document.forms["searchform"]["search"].value;
        
        if(input == null || input == "")
        {
            $('#input-error').fadeTo(2000, 500).slideUp(500, function() {
                $('#input-error').alert('close');
            });
            return false;
        }
        else
        {  
            return true;
        }
    }
    </script>
    
  </head>
  
  <body>
      
    <div class="header">
        <h1 align="center">#hashtux</h1>
    </div>
        
    <div class="container">

          <div class="row">
              
              <div class="col-md-4"></div>
              
              <div class="col-md-4 col-fill">
                  
                    <div class="search">
                        
                        <p align="center">Please search for a hashtag!</p>
                        
                        <div class="input-group">
                            <span class="input-group-addon">#</span>
                            <form action="search.php" method="get" id="searchform" onsubmit="
                                    if (checkInput() == true) 
                                        {					
                                        window.location.replace(document.getElementById('search').value);
                                        } return false; ">
                                    
                                <input type="text" class="form-control" id="search" name="search" />
                            </form>
                        </div>
                        
                        <div class="alert-warning fixalert" id="input-error">
                            You did not enter a hashtag, please try again!
                        </div>
                        
                    </div>
              
              	</div>
              	<div class="col-md-4" style="color: #bbbbbb; margin-top: 50px;">
				</div>
          </div>

    </div>
    
  </body>
</html>



    
    
    
