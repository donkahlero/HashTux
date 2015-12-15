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
    <script src="js/general.js"></script>    

    <script>
        
    var searchterm = "hashtux";
    
    var donationBtnDisplayed = false; 
        
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
    	loadTrendingHT();
		};
    
		// Fetch and render trending terms info from HashTux
		function loadTrendingHT() {
        $.ajax({
            url: "/ajax_post.php?search=search_term_week",
            type: "post",
            data: JSON.stringify({request_type:"stats"}),

            success: function (trendingJSON) {
                data = JSON.parse(trendingJSON); 
								result = "";

								// Sort search terms by their popularity
								data = data.sort(compare);
								
								// Concatenate together some HTML with 5 links to the most popular searches
								for (i = 0; i < data.length && i < 5; i++) {
									result += "<a href='" + data[i].key + "'>" + data[i].key + "</a><br />";	
								}
								
								$("#trending").html(result);
            }
       });
		}
	
		// Compare trending terms from HashTux (by value, which is search occurences/popularity)
		function compare(item1, item2) {
			// All other browsers than Firefox need a 1, 0 or -1 as return value here
			if (item1.value < item2.value) {
				return 1;
			} 
			return -1;	
		}
	
	

    function initialize() {

        $.ajax({
            url: "/ajax_post.php?search=" + searchterm,
            type: "post",
            data: JSON.stringify({request_type:"update", service:["instagram"], content_type:["image"]}),

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
        
    function checkInput(input)
    {
        input = strip_illegal_characters(input);
        
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
    
    function showDonationBtn()
    {
        if(!donationBtnDisplayed)
        {
            $('#donationbtn').fadeIn(500);
            donationBtnDisplayed = true;
        }
        else
        {
            $('#donationbtn').fadeOut(500);
            donationBtnDisplayed = false;
        }
    }
    
    </script>
    
  </head>
  
  <body style="background-color: #000">
      
    <div class="container con-fill header mainpagegrid" id="grid"></div>
      
    <div class="container con-fill-hor">

        <div class="row" style="margin: 0;">

            <div class="col-xs-12 col-fill">
                
                <div class="text-center">
                  
                    <div class="logo">
                        <img src="images/logotext.png" height="30%">
                    </div>

                    <div class="search">

                        <p class="greytext" align="center">Please search for a keyword!</p>

                        <form action="search.php" method="get" id="searchform" onsubmit="
															searchterm = strip_illegal_characters(document.getElementById('search').value);
                                if (checkInput(searchterm) == true) 
                                    {					
                                    	window.location.href = searchterm;
                                    } return false; ">

                            <input type="text" class="searchfield" id="search" name="search" style="width: 50%;"/>
                        </form>

                        <div class="alert-warning fixalert" id="input-error">
                            You did not enter a hashtag, please try again!
                        </div>

                    </div>


		            <div class="container con-fill header optionsbackground" id="commercial_use" onclick="hideCommercialUseInfo()">
		
		                <div class="panel optionspanel commercial_use_panel" id="commercial_panel">
		
		                    <h4 align="center">Commercial Use</h4>
		
		                    <hr />
							<p>
								HashTux is free for personal use. For commercial use, please purchase either a <i>Lifetime Access License</i>, or a <i>1 Year License</i>.
							</p>
		
							<form action="https://www.paypal.com/cgi-bin/webscr" method="post" target="_top">
							<input type="hidden" name="cmd" value="_s-xclick">
							<input type="hidden" name="hosted_button_id" value="V6UA6S9LKLTKL">
							
							<div class="commercial_use_section">
								<input type="hidden" name="on0" value="License:">License:
								<select name="os0">
									<option value="Lifetime License">Lifetime License €29,99 EUR</option>
									<option value="1 Year License">1 Year License €9,99 EUR</option>
								</select> 
							</div>
							<div class="commercial_use_section">
								<input type="hidden" name="currency_code" value="EUR">
								<input type="image" src="https://www.paypalobjects.com/en_US/GB/i/btn/btn_buynowCC_LG.gif" border="0" name="submit" alt="PayPal – The safer, easier way to pay online.">
								<img alt="" border="0" src="https://www.paypalobjects.com/sv_SE/i/scr/pixel.gif" width="1" height="1">
							</div>
							</form>
		
		                    <button type="button" class="btn btn-default savebutton"
		                            data-toggle="tooltip" data-placement="top" title="Save all options (Grid will be refreshed)"
		                            id="save" onclick="hideCommercialUseInfo()">Close</button>		
		                </div>
		
		            </div>

		
						<div style="text-align: center; color: white; margin-top: 170px;">
							<b>Popular searches:</b><br />
							<div id="trending">Loading...</a>
						</div>




                    <div class="footer">

                        <div class="donationbtn" id="donationbtn">
                                <form action="https://www.paypal.com/cgi-bin/webscr" method="post" target="_top">
                                <input type="hidden" name="cmd" value="_s-xclick">
                                <input type="hidden" name="hosted_button_id" value="EUPGXJH6CA6BL">
                                <input type="image" src="https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif" border="0" name="submit" alt="PayPal – The safer, easier way to pay online.">
                                <img alt="" border="0" src="https://www.paypalobjects.com/sv_SE/i/scr/pixel.gif" width="1" height="1">
                                </form>
                        </div>

                        <p class="greytext" align="center">
    HashTux is free for personal use. For commercial use, <button type="button" class="btn btn-link linkbtn" onclick="showCommercialUseInfo();">click here to read more</button>.
                            <br>
                            If you like the product and would like to support us, you can throw a 
                            <button type="button" class="btn btn-link linkbtn" onclick="showDonationBtn()">donation</button> our way. Every little bit helps!
                            <br>
                            <br>
                            © 2015 HashTux
                        </p>

                    </div>
                </div>
            </div>
          
        </div>

    </div>
    <!-- Served by <?php echo gethostname(); ?> -->
  </body>
</html>



    
    
    
