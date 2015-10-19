<html lang="en">
  <head>   
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    
    <title>HashTux</title>
    
    <link href="css/bootstrap.css" rel="stylesheet">  
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
                            <form action="search.php" method="get">
                                <input type="text" class="form-control" name="search" />
                            </form>
                        </div>
                    </div>
              
              </div>
              
              <div class="col-md-4"></div>
              
          </div>

    </div>
    
  </body>
</html>

<?php

    if(isset($_GET['search']))
    {
        $search = $_GET['search'];
        header("location: search.php search = $search");
    }