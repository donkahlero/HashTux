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
    <link href="css/hashtux.css" rel="stylesheet">
    
    <link href="images/favicon.ico" rel="shortcut icon">
    
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
    <script src="js/bootstrap.min.js"></script>
    <script src="js/frontpagegrid.js"></script>
    
    <script>
        
    var searchterm = "hashtux";
        
    var items = [];         // An array to store all items fetched
    var displayed = [];     // An array to temporarily store the currently displayed items

    var gridWidth = 4;      // The width of the grid (in num of tiles)
    var gridHeight = 3;     // The Height of the grid (in num of tiles)
    var totalItems = gridWidth * gridHeight;    // total number of tiles
    
    function item(type, service, url, text, username, userlink, frozen, tile) {
        this.type = type;               // The content type
        this.service = service;         // The service the content is from
        this.url = url;                 // URL (img/video)
        this.text = text;               // Text content (tweet)
        this.username = username;       // The username of content provider
        this.userlink = userlink;       // The link to the profile/channel page, depending on the service
        this.frozen = frozen;           // A boolean to check if the content is frozen (frozen will not refresh)
        this.tile = tile;               // Corresponding to the tile ID if the content is being displayed
    }
    
    window.onload = function() {
        initialize();       // Run the initialize function
    };
    
    function initialize() {

        $.ajax({
            url: "/ajax.php?search=" + searchterm,
            type: "get",
            data: JSON.stringify({request_type:"search", service:["instagram"], content_type:["image"]}),

            success: function (myString) {
                parse_to_items(myString);   // Parse the JSON to items
                initDisplayed();            // Run the initDisplayed function
                initGrid();                 // Initialize the grid
            }
        });
    }
    
    function parse_to_items(json) {

                var jsonobj = $.parseJSON(json);    // Parse the JSON object

                // A for loop to go through all of the objects within the JSON
                // and extract them into item objects.

                for(var i in jsonobj) {

                    // Construct an item object.

                    var incItem = new item(
                                jsonobj[i].content_type, jsonobj[i].service,
                                jsonobj[i].resource_link_high, jsonobj[i].text,
                                jsonobj[i].username, jsonobj[i].profile_link, false, "");

                    // Goes through the displayed array to check whether any of
                    // the items in it is the same as the incoming item. If so, disregard
                    // the incoming item.

                    items.push(incItem);    // Add the new item to the end of items

                }

//                debug += "\n\n";
//                
//                for(k = 0; k < items.length; k++)
//                {
//                    debug += items[k].service + " ";
//                }
//                
//                alert(debug);
            }
        
    function checkInput()
    {
        var input = document.forms["searchform"]["search"].value;
        
        if(input === null || input === "")
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
  
  <body style="background-color: #000">
      
    <div class="container con-fill header mainpagegrid" id="grid"></div>
      
    <div class="container">

        <div class="row">
              
            <div class="col-md-4"></div>

            <div class="col-md-4 col-fill">
                  
                <div class="logo">
                    <div class="text-center">
                        <img src="images/logotext.png" height="30%">
                    </div>
                </div>

                <div class="search">

                    <p class="greytext" align="center">Please search for a hashtag!</p>

                    <form action="search.php" method="get" id="searchform" onsubmit="
                            if (checkInput() == true) 
                                {					
                                window.location.replace(document.getElementById('search').value);
                                } return false; ">

                        <input type="text" class="searchfield" id="search" name="search" style="width: 100%;"/>
                    </form>

                    <div class="alert-warning fixalert" id="input-error">
                        You did not enter a hashtag, please try again!
                    </div>

                </div>

            </div>
              	
            <div class="col-md-4" style="color: #bbbbbb; margin-top: 50px;"></div>
          
        </div>

    </div>
    
  </body>
</html>



    
    
    
