<!DOCTYPE html>

<?php
    $search = $_GET['search'];
?>

<html>
    <head>
        
        <title>HashTux</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        
        <link href="css/bootstrap.css" rel="stylesheet">
        <link href="css/hashtux.css" rel="stylesheet">
        
        <link href="images/favicon.ico" rel="shortcut icon">
        
        <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
        <script src="js/bootstrap.min.js"></script>
        <script src="js/jquery.tweet-linkify.js"></script>
        
        <script src="js/grid.js"></script>
        <script src="js/refresh.js"></script>
        <script src="js/freeze.js"></script>
        <script src="js/options.js"></script>
        <script src="js/general.js"></script>
        
        
        <script type="text/javascript">
            $(document).ready(function() {
                // When the user has used forward/backward buttons in browser, check the
                // request path and reinitialize (fetch data and render items).
                window.onpopstate = function(event) {
                      searchterm = window.location.pathname.substring(1);

                                        // Download and display new items for the new search term
                                        reinitialize();
                };
            });

            var searchterm = "<?php echo $search; ?>";
            var options = {request_type: "update"};
            
            var heartbeats = true;
            
            var refreshTimer;
            
            var screenFrozen = false;
            
            var items = [];         // An array to store all items fetched
            var displayed = [];     // An array to temporarily store the currently displayed items
           
            var gridWidth = 4;      // The width of the grid (in num of tiles)
            var gridHeight = 3;     // The Height of the grid (in num of tiles)
            var totalItems = gridWidth * gridHeight;    // total number of tiles
            
            // A constructor that given a certain number of arguments creates item
            // object which in this case is a representation of the JSON objects
            // retreived from the backend.
            
            function item(type, service, url, text, username, displayname, profilepic, postdate, userlink, frozen, tile) {
                this.type = type;                   // The content type
                this.service = service;             // The service the content is from
                this.url = url;                     // URL (img/video)
                this.text = text;                   // Text content (twitter)
                this.username = username;           // The username of content poster
                this.displayname = displayname;     // The full display name of the poster (twitter)
                this.profilepic = profilepic;       // The porfile picture of the poster (twitter)
                this.postdate = postdate;           // The date the post was made (twitter)
                this.userlink = userlink;           // The link to the profile/channel page, depending on the service
                this.frozen = frozen;               // A boolean to check if the content is frozen (frozen will not refresh)
                this.tile = tile;                   // Corresponding to the tile ID if the content is being displayed
            }
            
            /**
             * A function that handles the initial request for data from the backend.
             * This request is done with ajax and our internal request type is set to search.
             */
            
            function initialize() {
                
                loading();
                
                $.ajax({
                    url: "/ajax.php?search=" + searchterm + "&request_type=search",
                    type: "get",
                    
                    success: function (myString) {
                        parse_to_items(myString);   // Parse the JSON to items
                        initDisplayed();            // Run the initDisplayed function
                        initGrid();                 // Initialize the grid
                        stopLoading();
                    }
                });
            }
            
            function reinitialize() {
            
		hideNoResults();
                loading();
            
                displayed = [];
                items = [];
                $('#grid').html('');
                
                $('#searchlabel').html("#" + searchterm);
                
                options.request_type = "search";
									
                $.ajax({
                    url: "/ajax_post.php?search=" + searchterm,
                    type: "post",
                    data: JSON.stringify(options),
                    
                    success: function (myString) {
//                        $('#debug').html("<p>" + myString + "</p>");
                        parse_to_items(myString);   // Parse the JSON to items
                        initDisplayed();            // Run the initDisplayed function
                        initGrid();                 // Initialize the grid
                        stopLoading();
                    }
                });
                
                options.request_type = "update";
            }
            
            function newSearch() {
                
               
                var newTerm = strip_illegal_characters($('#searchField').val());
                
//                alert("New Search: " + newTerm);
                
                if(newTerm === "")
                {
                    $('#invalidterm').fadeTo(3000, 500).slideUp(500, function() {
                        $('#invalidterm').alert('close');
                    });
                }
                
                else
                {
                    searchterm = newTerm;
            		
                    // Download and display new items for the new search term
                    reinitialize();

                    // Update the browser URL and history to reflect the new search term.
                    // If the user then uses back/forward buttons, window.onpopstate will be called.
                    history.pushState({state: searchterm}, null, searchterm);
                }
            }
            
            // Sends a heartbeat message to the http server to let the miners
            // know that the user is still on the website and that they should keep mining.
            
            function heartbeat() {
                $.ajax({
                    url: "/ajax.php?search=" + searchterm + "&request_type=heartbeat",
                });
            }
            
            // A function for any future searches. Uses ajax.php to fetch the
            // JSON object from the http server.
            
            function fetch() {
                $.ajax({
                    url: "/ajax_post.php?search=" + searchterm,
                    type: "post",                    
                    data: JSON.stringify(options),
                    
                    success: function (myString) {
//                        alert(JSON.stringify(myString));
                        parse_to_items(myString);
                    }
                });
            }
            
            // A function that runs on a 30 second interval and sends a heartbeat
            // or fetch message every other time it runs. If a heartbeat was sent
            // last, it sends a fetch request and vice versa.
            
            function heartbeatFetchHandler() {
                
                if(heartbeats === true)
                {
                    heartbeat();
                    heartbeats = false;
                }
                
                else
                {
                    fetch();
                    heartbeats = true;
                }
            }
            
            // A function to extract all the information from the JSON object and
            // convert it to an amount of item objects that we store in the items
            // array.

            function parse_to_items(json) {
                
                if(json === "[]")
                {
                    showNoResults();    // Show the "No Results" message
                }
                
                else
                {
                    hideNoResults();    // Hide the "No Results" message
                }
                
                var jsonobj = $.parseJSON(json);    // Parse the JSON object
                
                // A for loop to go through all of the objects within the JSON
                // and extract them into item objects.
                
                for(var i in jsonobj) {
                    
                    // Construct an item object.
                    
                    var incItem = new item(
                                jsonobj[i].content_type, jsonobj[i].service,
                                jsonobj[i].resource_link_high, jsonobj[i].text,
                                jsonobj[i].username, jsonobj[i].free_text_name,
                                jsonobj[i].profile_image_url, jsonobj[i].date_string,
                                jsonobj[i].profile_link, false, "");
                                
                    var ignore = false;     // A boolean to keep track of whether to insert the item or not

                    
                    // Goes through the displayed array to check whether any of
                    // the items in it is the same as the incoming item. If so, disregard
                    // the incoming item.
                    
                    for(j = 0; j < displayed.length; j++)
                    {
                        if(incItem.url === displayed[j].url || incItem.text === displayed[j].text)
                        {
                            ignore = true;      // Set ignore to true
                            break;              // Break the loop and continue with the next item
                        }
                    }
                    
                    // Goes through the items array and checks whether any of the
                    // items are the same as the incoming one. If so, disregard the item.
                    
                    for(k = 0; k < items.length; k++)
                    {
                        if(incItem.url === items[k].url || incItem.text === items[k].text)
                        {
                            ignore = true;      // Set ignore to true
                            break;              // Break the loop and continue with the next item
                        }
                    }
                    
                    // As long as ignore is false, do the following
                    
                    if(ignore === false)
                    {
                        
                        // If the length of the item array reaches 50 elements
                        // push an old item out before inserting a new one
                        
                        if(items.length >= 50)
                        {
                            items.splice(0, 1);     // Remove the head of items
                            items.push(incItem);    // Add the new item to the end of items
                        }
                        
                        else
                        {
                            items.push(incItem);    // Add the new item to the end of items
                        }
                    }
                }
            }
            
          /**
           * A function that runs as soon as the users window loads.
           */
            
            window.onload = function() {
                
                initialize();       // Run the initialize function
                
                refreshTimer = setInterval(refresh, 5000);      // Initialize the refresh function on a timer.
                setInterval(heartbeatFetchHandler, 30000);      // Initialize the heartbeatFetchHandler function on a timer.
                
                hoverListener();    // Run the hoverListener function
                timeScrollListener();
            };
            
            // Initialize all bootstrap tooltips on the website
            
            $(function () {
                $('[data-toggle="tooltip"]').tooltip()
            });
            
            // Shows the textfield used to make a new search when you click the search button
            
            function showField() {
                $('#searchField').fadeIn(500);      // Fade in the text field
                $('#searchBtn').hide();             // Hide the search button
                
                $('#searchField').click(function(e) {
                    e.stopPropagation();        // Ignore hover events
                });
            }
            
            // A function to show the menu
            
            function showMenu() {
                $('#menuBtn').fadeOut(200);
                $('#actionsBtn').fadeOut(200);
                $('#optionsMenu').fadeIn(500);
                $('#topbackdrop').fadeIn(500);
            }
            
            // A function to hide the menu
            
            function hideMenu() {
                $('#searchField').hide();
                $('#optionsMenu').fadeOut(500);
                $('#topbackdrop').fadeOut(500);
                $('#searchBtn').show();
                $('#menuBtn').fadeIn(200);
                $('#actionsBtn').fadeIn(200);
            }
            
            // A function to show the actions menu
            
            function showActionsMenu() {
                $('#actionsBtn').fadeOut(200);
                $('#menuBtn').fadeOut(200);
                $('#actionsMenu').fadeIn(500);
                $('#topbackdrop').fadeIn(500);
            }
            
            // A function to hide the actions menu
            
            function hideActionsMenu() {
                $('#actionsBtn').fadeIn(200);
                $('#menuBtn').fadeIn(200);
                $('#actionsMenu').fadeOut(500);
            }
            
            // A function to show the loading circle
            
            function loading() {
                $('#progress').show();
            }
            
            // A function to hide the loading circle
            
            function stopLoading() {
                $('#progress').hide();
            }
            
            // A function to show the "No Results" message
            
            function showNoResults() {
                $('#no-results').show();
            }
            
            // A function to hide the "No Results" message
            
            function hideNoResults() {
                $('#no-results').hide();
            }
            
            // A function that listens to whether the "ENTER" key is pressed, runs
            // when you press a key when focusing on the searchfield.
            
            function runScript(e) {
                if (e.keyCode === 13) {
                    newSearch();    // Run a new search
                }
            }
            
            function hoverListener() {
                
                $('body').mouseover(function(e){
                    var x = e.pageX - this.offsetLeft;
                    var y = e.pageY - this.offsetTop;
                    
                    var topLimit = ($(document).height()/3);
                    
                    if(y > topLimit)
                    {
                        hideMenu();
                        hideActionsMenu();
                    }
                });
            }
            
            function timeScrollListener() {
                
                $('#timeScrollValue').text("NOW");
                
                $('#timeScroll').change(function() {
                    
                    var halfDay = 60 * 60 * 12;
                    
                    var now = new Date();
                    var startOfDay = new Date(now.getFullYear(), now.getMonth(), now.getDate());
                    var timestamp = startOfDay / 1000;
                
                    switch($('#timeScroll').val())
                    {
                        case "8":
                            $('#timeScrollValue').text("RECENT");
                            delete options.history_timestamp;
                            break;
                            
                        case "7":
                            $('#timeScrollValue').text("TODAY");
                            options.history_timestamp = timestamp;
                            break;
                            
                        case "6":
                            $('#timeScrollValue').text("YESTERDAY");
                            options.history_timestamp = timestamp - halfDay;
                            break;
                            
                        case "5":
                            $('#timeScrollValue').text("2 DAYS AGO");
                            options.history_timestamp = timestamp - (halfDay * 3);
                            break;
                            
                        case "4":
                            $('#timeScrollValue').text("3 DAYS AGO");
                            options.history_timestamp = timestamp - (halfDay * 5);
                            break;
                            
                        case "3":
                            $('#timeScrollValue').text("4 DAYS AGO");
                            options.history_timestamp = timestamp - (halfDay * 7);
                            break;
                            
                        case "2":
                            $('#timeScrollValue').text("5 DAYS AGO");
                            options.history_timestamp = timestamp - (halfDay * 9);
                            break;
                            
                        case "1":
                            $('#timeScrollValue').text("1 WEEK AGO");
                            options.history_timestamp = timestamp - (halfDay * 11);
                            break;
                    }
                    
                    reinitialize();
                
                });
            }
	</script>
	          
    </head>
    
    <body style="background-color: #3d3d3d">
 
        <div class="container con-fill">

            <div class="container con-fill header" id="grid"></div>

            <div class="container con-fill-hor">
                
                <div class="topbackdrop" id="topbackdrop"></div>
                
                <button type="submit" class="menubtn" id="menuBtn" onmouseover="showMenu()">
                    <img src="images/menuicon.png" width="50px" height="50px"/>
                </button>
                
                <button type="submit" class="actionsbtn" id="actionsBtn" onmouseover="showActionsMenu()">
                    <img src="images/menuiconbottom.png" width="50px" height="50px"/>
                </button>

                <div class="row topbar" id="optionsMenu">
                    <div class="col-sm-8">
                        <ol class="breadcrumb searchlabel">
                            <li id="searchlabel"><script>document.write("#" + searchterm);</script></li>
                        </ol>
                    </div>
                    <div class="col-sm-4">
                        <button type="submit" class="iconbtn" id="optionsBtn"
                                data-toggle="tooltip" data-placement="bottom" 
                                title="Click here to open the options menu" onclick="showOptions()">
                            <img src="images/options.png" width="30px" height="30px"/>
                        </button>
                        
                        <input type="text" class="searchfield searchfieldgrid" 
                               id="searchField" onkeypress="runScript(event)">
                            
                        <button type="submit" class="iconbtn" id="searchBtn"
                                data-toggle="tooltip" data-placement="bottom" 
                                title="Click here to enter a new search term" onmouseover="showField()">
                            <img src="images/search.png" width="30px" height="30px"/>
                        </button>
                        <div class="centered" id="player">
                            <img id="pause" src="http://codropspz.tympanus.netdna-cdn.com/codrops/wp-content/uploads/2010/01/pause1-150x150.png" width="100" height="100" style="display:none;"/>
                            <img id="play" src="http://codropspz.tympanus.netdna-cdn.com/codrops/wp-content/uploads/2010/01/play1-150x150.png" width="100" height="100" style="display:none;"/>
                             
                       </div>
                        
                    </div>
                </div>
                
                <div class="row topbar" id="actionsMenu">
                    <div class="col-sm-3">
                        <ol class="breadcrumb searchlabel">
                            <li id="searchlabel"><script>document.write("#" + searchterm);</script></li>
                        </ol>
                    </div>
                    <div class="col-sm-6">
                        <p class="timescrollvalue" id="timeScrollValue">
                        </p>
                        <input type="range" min="1" max="8" step="1" value="8" class="timescroll" id="timeScroll">
                    </div>
                    <div class="col-sm-3">
                        <button type="submit" class="iconbtn freezebtn" id="freezeBtn"
                                data-toggle="tooltip" data-placement="top" 
                                title="Click here to freeze the screen" onclick="screenFreeze()">
                            <img src="images/freeze.png" width="40px" height="40px"/>
                        </button>
                    </div>
                </div>
            </div>
                
            <div class="alert-warning gridalert" id="aborted">
                Your changes were aborted!
            </div>
            
            <div class="alert-success gridalert" id="saved">
                Changes saved!
            </div>
            
            <div class="alert-warning gridalert" id="invalidterm">
                You did not enter a hashtag, please try again!
            </div>
            
            <div class="center-container">
                <div class="spinner" id="progress"></div>
                <div class="no-results" id="no-results">NO RESULTS</div>
            </div>

            <div class="container con-fill header optionsbackground" id="options" onclick="hideOptions()">

                <div class="panel optionspanel" id="optionsPanel">

                    <hr />

                    <h4 align="center">GRID OPTIONS</h4>

                    <hr />

                    <div class="optiontext" align="center">Tile Size</div> 

                    <div class="text-center">
                        <div class="btn-group options">
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Small tile size (4x6 grid)"
                                    id="size-sm" onclick="changeSize('size-sm')">SMALL</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Medium tile size (3x4 grid)"
                                    id="size-md" onclick="changeSize('size-md')">MEDIUM</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Large tile size (2x3 grid)"
                                    id="size-lg" onclick="changeSize('size-lg')">LARGE</button>
                        </div>
                    </div>

                    <div class="optiontext" align="center">Refresh Rate</div>

                    <div class="text-center">
                        <div class="btn-group options">
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Slow update speed (1 tile every 10 seconds)"
                                    id="ref-slow" onclick="changeRefRate('ref-slow')">SLOW</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Medium update speed (1 tile every 5 seconds)"
                                    id="ref-md" onclick="changeRefRate('ref-md')">MEDIUM</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Fast update speed (1 tile every 2 seconds)"
                                    id="ref-fast" onclick="changeRefRate('ref-fast')">FAST</button>
                        </div>
                    </div>

                    <hr />

                    <h4 align="center">CONTENT OPTIONS</h4>

                    <hr />

                    <div class="optiontext" align="center">Media Type</div>
                    <div class="optionexpltext" align="center">Select the type of media you would like displayed in the grid.</div>

                    <div class="text-center">
                        <div class="btn-group options">
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Stop any images from showing up in your grid"
                                    id="type-img" onclick="changeType('type-img')">Images</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Stop any videos from showing up in your grid"
                                    id="type-vid" onclick="changeType('type-vid')">Videos</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Stop any pure text content from showing up in your grid"
                                    id="type-txt" onclick="changeType('type-txt')">Text</button>
                        </div>
                    </div>

                    <div class="optiontext" align="center">Services</div>
                    <div class="optionexpltext" align="center">Select from which services you would like your content retrieved from.</div>

                    <div class="text-center">
                        <div class="btn-group options">
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Stop any Twitter content from show up in your grid"
                                    id="serv-twitter" onclick="changeService('serv-twitter')">Twitter</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Stop any Instagram content from show up in your grid"
                                    id="serv-instagram" onclick="changeService('serv-instagram')">Instagram</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Stop any Youtube content from show up in your grid"
                                    id="serv-youtube" onclick="changeService('serv-youtube')">YouTube</button>
                        </div>
                    </div>

                    <div class="optiontext" align="center">Languages</div>

                    <div class="text-center">
                        <div class="btn-group-vertical options">
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="left" title="Only english content will appear in the grid"
                                    id="en" onclick="changeLanguage('en')">English</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="left" title="Only spanish content will appear in the grid"
                                    id="es" onclick="changeLanguage('es')">Español (Spanish)</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="left" title="Only french content will appear in the grid"
                                    id="fr" onclick="changeLanguage('fr')">Français (French)</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="left" title="Only german content will appear in the grid"
                                    id="de" onclick="changeLanguage('de')">Deutsch (German)</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="left" title="Only swedish content will appear in the grid"
                                    id="sv" onclick="changeLanguage('sv')">Svenska (Swedish)</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="left" title="Only bulgarian content will appear in the grid"
                                    id="bg" onclick="changeLanguage('bg')">български език (Bulgarian)</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="left" title="Only italian content will appear in the grid"
                                    id="it" onclick="changeLanguage('it')">Italiano (Italian)</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="left" title="Only amharic content will appear in the grid"
                                    id="am" onclick="changeLanguage('am')">አማርኛ (Amharic)</button>
                        </div>
                    </div>

                    <button type="button" class="btn btn-default savebutton"
                            data-toggle="tooltip" data-placement="top" title="Save all options (Grid will be refreshed)"
                            id="save" onclick="saveOptions()">Save & Exit</button>

                </div>

            </div>

        </div>
	    
        <!-- Served by <?php echo gethostname(); ?> -->
        
    </body>
</html>
