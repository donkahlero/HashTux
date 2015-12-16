/**
 * This Javascript file handles the generating of the grid.
 */

/**
 * A function that fills the displayed array with the number of items that
 * are needed to fill up the grid. If there is not enough items, it gets filled
 * with all the availible items.
 */

function initDisplayed()
{                
    if(displayed.length > 0)
    {
        displayed.length = 0;
    }

    for(k = 0; k < totalItems; k++) {

        if(items.length < 1)
            break;

        var index = Math.floor((Math.random() * items.length));

        displayed.push(items[index]);
        items.splice(index, 1);
    }
}

function initGrid() 
{
    var grid = $('#grid');
    var count = 0;

    for(i = 0; i < gridHeight; i++) {

        var cols = "";

        for(j = 0; j < gridWidth; j++) {

            if(count >= displayed.length) 
            {

                cols = cols + 
                    "<div class='col-xs-" +
                    (12/gridWidth) + 
                    " col-fill fixitem' style='background-image:url();' id='tile" +
                    count +
                    "'></div>";

                count++;
            }

            else if(displayed[count].service === "instagram") 
            {

                if(displayed[count].type === "image")
                {


                    cols = cols + 
                        "<div class='col-xs-" +
                        (12/gridWidth) + 
                        " col-fill fixitem' style='background-image:url(" +
                        displayed[count].url +
                        ");' id='tile" + count + "'>" +
                        "<p class='usernameimage'><a class='greytext' href='" +
                        displayed[count].userlink + "' target='_blank'>@" +
                        displayed[count].username + "</a></p>" +
                        "<button type='submit' class='itemfreezebtn' id='tile" + count +
                        "freeze' onclick='tileFreeze(" + count + ")'>" +
                        "<img src='images/freeze.png' width='30px' height='30px'>" + "</button>" +
                        "</div>";

                    displayed[count].tile = "tile" + count;
                    count++;

                }

                else if(displayed[count].type === "video")
                {
                    cols = cols +
                        "<div class='col-xs-" + 
                        (12/gridWidth) + 
                        " col-fill' style='background-image:url('');' id='tile"+
                        count + "'>" +
                        "<video width='100%' height='100%' autoplay loop muted controls src='" +
                        displayed[count].url + 
                        "' style='object-fit: fill; top: 0; position: absolute;'></video>" +
                        "<p class='usernameimage'><a class='greytext' href='" +
                        displayed[count].userlink + "' target='_blank'>@" +
                        displayed[count].username +
                        "</a></p>" +
                        "<button type='submit' class='itemfreezebtn' id='tile" + count +
                        "freeze' onclick='tileFreeze(" + count + ")'>" +
                        "<img src='images/freeze.png' width='30px' height='30px'>" + "</button>" +
                        "</div>";

                    displayed[count].tile = "tile" + count;
                    count++;
                }

            }

            else if(displayed[count].service === "twitter")
            {

                if(displayed[count].type === "text")
                { 
                    cols = cols +
                        "<div class='col-xs-" +
                        (12/gridWidth) + 
                        " col-fill fixitem' id='tile" +
                        count + "' style='background-image:url(images/TwitterLogo_blue.png);'>" +
                        "<div class='twittertext'>" +
                        "<div class='twittertop'>" + 
                        "<img src='" + displayed[count].profilepic + "' width='20px' height='20px'> | " + 
                        "<a class='twitterlink' href='" + displayed[count].userlink + "' target='_blank'>" + 
                        displayed[count].displayname + " (@" + displayed[count].username + ")" + "</a>" +
                        "</div>" + "<p class='tweet'>" + displayed[count].text + "</p>" +
                        "<div class='smalltext'><a href='" + displayed[count].userlink + "' target='_blank'>" +
                        displayed[count].postdate + "</a><br />" + 
                        "<a href='https://twitter.com/intent/like?tweet_id=463440424141459456'>like</a> | " +
                        "<a href='https://twitter.com/intent/retweet?tweet_id=463440424141459456'>retweet</a> | " +
                        "<a href='https://twitter.com/intent/tweet?in_reply_to=463440424141459456'>reply</a>" +"</div></div>" +
                        "<button type='submit' class='itemfreezebtn' id='tile" + count +
                        "freeze' onclick='tileFreeze(" + count + ")'>" +
                        "<img src='images/freeze.png' width='30px' height='30px'>" + "</button>" +
                        "</div>";

                    displayed[count].tile = "tile" + count;
                    count++;
                }

                else if(displayed[count].type === "image")
                {
                    cols = cols + 
                        "<div class='col-xs-" +
                        (12/gridWidth) + 
                        " col-fill fixitem' style='background-image:url(" +
                        displayed[count].url +
                        ");' id='tile" +
                        count + "'>" +
                        "<div class='twitterimagetext'>" +
                        "<div class='twittertop'>" + 
                        "<img src='" + displayed[count].profilepic + "' width='20px' height='20px'> | " + 
                        "<a class='twitterlink' href='" + displayed[count].userlink + "'target='_blank'>" + 
                        displayed[count].displayname + " (@" + displayed[count].username + ")" + "</a>" +
                        "</div>" + "<p class='tweet'>" + displayed[count].text + "</p>" + 
                        "<div class='smalltext'><a href='" + displayed[count].userlink + "' target='_blank'>" +
                        displayed[count].postdate + "</a><br />" + 
                        "<a href='https://twitter.com/intent/like?tweet_id=463440424141459456'>like</a> | " +
                        "<a href='https://twitter.com/intent/retweet?tweet_id=463440424141459456'>retweet</a> | " +
                        "<a href='https://twitter.com/intent/tweet?in_reply_to=463440424141459456'>reply</a>" +"</div></div>" +
                        "<button type='submit' class='itemfreezebtn' id='tile" + count +
                        "freeze' onclick='tileFreeze(" + count + ")'>" +
                        "<img src='images/freeze.png' width='30px' height='30px'>" + "</button>" +
                        "<div class='twitterlogo'><img src='images/TwitterLogo_blue.png' width='40px' height='40px'></div>" +
                        "</div>";

                    displayed[count].tile = "tile" + count;
                    count++;
                }
            }
            
            else if(displayed[count].service === "youtube")
            {
                cols = cols +
                    "<div class='col-xs-" + 
                    (12/gridWidth) + 
                    " col-fill' style='background-image:url('');' id='tile"+
                    count + "'>" +
                    "<iframe width='100%' height='100%' frameborder='0' src='" +
                    displayed[count].url + "?autoplay=1&loop=1" +
                    "' style='object-fit: fill; top: 0; position: absolute;'></iframe>" +
                    "<p class='usernameimage'><a class='greytext' href='" +
                    displayed[count].userlink + "' target='_blank'>@" +
                    displayed[count].username +
                    "</a></p>" +
                    "<button type='submit' class='itemfreezebtn' id='tile" + count +
                    "freeze' onclick='tileFreeze(" + count + ")'>" +
                    "<img src='images/freeze.png' width='30px' height='30px'>" + "</button>" +
                    "</div>";

                displayed[count].tile = "tile" + count;
                count++;
            }
        }

        grid.html(grid.html() +
            "<div class='row-grid-" +
            (12/gridHeight) + "' id='row" +
            i + "'>" + cols + "</div>");
    }
    
    $('p.tweet').tweetLinkify();
    
}