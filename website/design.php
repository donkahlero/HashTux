<html lang="en">
    <head>
        
        <title>HashTux</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        
        <link href="css/bootstrap.css" rel="stylesheet">
        
        <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
        <script src="js/bootstrap.min.js"></script>
        
        <script type='text/javascript'>

        
            function item(type, content, frozen, displayed) {
                this.type = type;
                this.content = content;
                this.frozen = frozen;
                this.displayed = displayed;
            }
            
            /*
             * Images!
             * 
             * http://i.imgur.com/JfKwovX.jpg
             * http://i.imgur.com/z9DY7ev.jpg
             * http://i.imgur.com/bHkmaqm.jpg
             * http://i.imgur.com/DfjlogK.jpg
             * http://i.imgur.com/JgNFQlc.jpg
             * http://i.imgur.com/0OYdzge.jpg
             * http://i.imgur.com/iBnXFAA.jpg
             * http://i.imgur.com/HiqYpKC.jpg
             * http://i.imgur.com/aFvyTTp.jpg
             * http://i.imgur.com/fDutob0.jpg
             * http://i.imgur.com/xx7hFLa.jpg
             * http://i.imgur.com/rRK1c1O.jpg
             * 
             */
            
            var items = [new item("tweet", "This is a motherfucking tweet! Is it not beautiful?! I think yes.", false, false),
                         new item("img", "http://i.imgur.com/z9DY7ev.jpg", false, false),
                         new item("img", "http://i.imgur.com/bHkmaqm.jpg", false, false),
                         new item("tweet", "This is a motherfucking tweet! Is it not beautiful?! I think yes.", false, false),
                         new item("img", "http://i.imgur.com/JgNFQlc.jpg", false, false),
                         new item("img", "http://i.imgur.com/0OYdzge.jpg", false, false),
                         new item("tweet", "This is a motherfucking tweet! Is it not beautiful?! I think yes.", false, false),
                         new item("img", "http://i.imgur.com/HiqYpKC.jpg", false, false),
                         new item("tweet", "This is a motherfucking tweet! Is it not beautiful?! I think yes.", false, false),
                         new item("img", "http://i.imgur.com/fDutob0.jpg", false, false),
                         new item("img", "http://i.imgur.com/xx7hFLa.jpg", false, false),
                         new item("img", "http://i.imgur.com/rRK1c1O.jpg", false, false)];
            
            window.onload = function() {
                
                var $grid = document.getElementById('grid');
                var $row = "<div class='row-grid-4' id='row";
                var $rowmid = "'>";
                var $rowend = "</div>";
                var $colstart = "<div class='col-xs-3 col-fill imageitem' style='background-image:url(";
                var $colmid = ");' id='col";
                var $colend = "'></div>";
                var $count = 0;
                
                var $tweetcolstart = "<div class='col-xs-3 col-fill twitteritem' id='col";
                var $tweetcolmid = "'>";
                var $tweetcolend = "</div>";
                
                for(i = 0; i < 3; i++) {
                    
                    var $cols = "";
                    
                    for(j = 0; j < 4; j++) {
                        
                        if(items[$count].type === "img") {                        
                            $cols = $cols + $colstart + items[$count].content + $colmid + $count + $colend;
                            items[$count].displayed = true;
                            $count++;
                        }
                        else if(items[$count].type === "tweet")
                        {
                            $cols = $cols + $tweetcolstart + $count + $tweetcolmid +
                                "<div class='twittertext'><p>" +
                                items[$count].content +
                                "</p><p class='username'>@hashtux</p></div>" + 
                                $tweetcolend;
                            items[$count].displayed = true;
                            $count++;
                        }
                    }
                    
                    $grid.innerHTML = $grid.innerHTML + $row + i + $rowmid + $cols + $rowend;
                }
                
            };
            
           var	reff = setInterval(refresh, 10000);
            
            function refresh() {
            
                for(i = 0; i < 5; i++) {
                    var newitem = items[Math.floor((Math.random() * 12))];

                    var $randomcol = "col" + Math.floor((Math.random() * 12));

                    if(newitem.type === "img") 
                    {
                        $(document.getElementById($randomcol)).animate({height: "0", opacity: "0"}, 0);

                        document.getElementById($randomcol).innerHTML = '';
                        document.getElementById($randomcol).className = 'col-xs-3 col-fill imageitem';
                        document.getElementById($randomcol).style.backgroundImage = 'url(' + newitem.content + ')';

                        $(document.getElementById($randomcol)).animate({height: "100%", opacity: "1"}, 1000);
                    }
                    else if (newitem.type === "tweet")
                    {
                        $(document.getElementById($randomcol)).animate({height: "0", opacity: "0"}, 0);

                        document.getElementById($randomcol).className = "col-xs-3 col-fill twitteritem";
                        document.getElementById($randomcol).style.backgroundImage = '';
                        document.getElementById($randomcol).innerHTML =
                                "<div class='twittertext'><p>" +
                                newitem.content +
                                "</p><p class='username'>@hashtux</p></div>";

                        $(document.getElementById($randomcol)).animate({height: "100%", opacity: "1"}, 1000);
                    }
                    
                }
            }
                 
            function freeze(){
                
            	for(i = 0; i < items.length; i++) 
                {
                    	  
             	 if(items[i].displayed === true){
                    items[i].frozen = !items[i].frozen; 
             		alert('frozen ' + items[i].frozen);
             				}
                 		}	
                     }
            
            function unfreeze(){
               
        	    for(i = 0; i < items.length; i++) 
                {
             	  
             	 if(items[i].displayed === false){
                    items[i].frozen = !items[i].frozen; 
             		alert('unfrozen '+ items[i].frozen);
             	 			}
                 		}	
                     }
            
            function reload(){

               	$("#col0").animate({padding:"-50%"},"50");
               	refresh();
                   }
           
           $(document).ready(function(){
               $("#searchBtn").click(function(){
                   $("#col0").animate({padding:"50%"},"100");
                   
               });
           });
	           
            function showField() {
                $(document.getElementById('sField')).fadeIn(500);
                $(document.getElementById('searchBtn')).hide();
                
                $(document.getElementById('sField')).click(function() {
                    event.stopPropagation();
                });
                
                event.stopPropagation();
            }
            
            function showOptions() {
                $(document.getElementById('optionsMenu')).fadeIn(500);
            }
            
            function hideOptions() {
                $(document.getElementById('sField')).hide();
                $(document.getElementById('optionsMenu')).fadeOut(500);
                $(document.getElementById('searchBtn')).show();
            }
            
            function hideSearchField() {
                $(document.getElementById('sField')).hide();
                $(document.getElementById('searchBtn')).fadeIn(500);
            }
            
        </script>    
        
    </head>
    
    <body style="background-color: #3d3d3d">
        
        <div  class="container con-fill">
            
            <div class="container con-fill header" id="grid">
            </div>
        
            <div class="container con-fill-hor">
                
                <div class="tophoverarea" onmouseover="showOptions()" onclick="hideSearchField()"></div>
                <div class="midhoverarea" onmouseover="hideOptions()"></div>
                <div class="bothoverarea" onmouseover=""></div>
                
                <div class="row topbar" id="optionsMenu">
                    <div class="col-md-8">
                        <ol class="breadcrumb" style="background:none; margin: 0; padding: 0;">
                            <li style="color: #ebebeb;">#jerk</li>
                            <li style="font-weight: bold; color: #ebebeb;">#hashtux</li>
                        </ol>
                    </div>
                    <div class="col-md-4">
                        <button type="button" class="btn btn-default btn-md" id="optionsBtn"
                                style="float:right;" onclick="freeze()">
                            Freeze
                        </button>
                        <div class="input-group" style="display: none; float:right; width:inherit; margin-right: 15px;" id="sField">
                            <span class="input-group-addon">#</span>
                            <input type="text" class="form-control" name="sField">
                        </div>
                        <button type="button" class="btn btn-default btn-md" id="searchBtn"
                                style="float:right; margin-right: 15px;" onclick="unfreeze()">
                            zoom
                        </button>
                        <button type="button" class="btn btn-default btn-md" id="reload"
                                style="float:right; margin-right: 15px;" onclick="reload()">
                            refresh
                        </button>
                    </div>
                </div>
            </div>
            
        </div>
        
    </body>
</html>

