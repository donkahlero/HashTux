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
                var $row = "<div class='row-grid-4'>";
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
                    
                    $grid.innerHTML = $grid.innerHTML + $row + $cols + $rowend;
                }                
                
            };
            
            function refresh() {
                var url = items[Math.floor((Math.random() * 11))];
                
                var $randomcol = "col" + Math.floor((Math.random() * 11));
                
                $(document.getElementById($randomcol)).fadeOut(600);
                document.getElementById($randomcol).style.backgroundImage = 'url(' + url + ')';
                $(document.getElementById($randomcol)).fadeIn(300);
            }
            
            function showField() {
                $(document.getElementById('sField')).fadeIn(500);
                $(document.getElementById('searchBtn')).hide();
                
                $(document.getElementById('sField')).click(function() {
                    event.stopPropagation();
                });
                
                event.stopPropagation();
            }
            
            $('html').click(function() {
                $(document.getElementById('sField')).hide();
                $(document.getElementById('searchBtn')).fadeIn(500);
                refresh();
            });
            
        </script>    
        
    </head>
    
    <body style="background-color: #FFF">
        
        <div class="container con-fill">
            
            <div class="container con-fill header" id="grid">
            </div>
        
            <div class="container con-fill-hor">
                <div class="row">
                    <div class="col-md-4"></div>
                    <div class="col-md-4"></div>
                    <div class="col-md-4">
                        <div class="input-group" style="display: none; margin-top: 15px;" id="sField">
                            <span class="input-group-addon">#</span>
                            <input type="text" class="form-control" name="sField">
                        </div>
                        <button type="button" class="btn btn-default btn-md" id="searchBtn"
                                style="float:right; margin-top: 15px;" onclick="showField()">
                            Search
                        </button>
                    </div>
                </div>
            </div>
            
        </div>
        
    </body>
</html>

