function refresh() 
{

    if(items.length > 0 && !screenFrozen)
    {

        var newIndex = Math.floor((Math.random() * items.length));
        var newItem = items[newIndex];
        var randTileNum = Math.floor((Math.random() * totalItems));
        var randTile = "#tile" + randTileNum;
        var currentItem = null;
        var currentIndex;

            for(i = 0; i < displayed.length; i++)
            {
                if(displayed[i].tile === "tile" + randTileNum)
                {
                    currentItem = displayed[i];
                    currentIndex = i;
                }
            }

            if(currentItem === null || !currentItem.frozen)
            {
                if(newItem.service === "instagram")
                {
                    if(newItem.type === "image") 
                    {
                        $(randTile).animate({height: "0", opacity: "0"}, 0);

                        $(randTile).attr('class', 'col-xs-' + (12/gridWidth) + ' col-fill fixitem');
                        $(randTile).css('background-image', 'url(' + newItem.url + ')');
                        $(randTile).html("<p class='usernameimage'><a class='greytext' href='" +
                                newItem.userlink + "' target='_blank'>@" +
                                newItem.username + "</a></p>" +
                                "<button type='submit' class='itemfreezebtn' id='tile" + randTileNum +
                                "freeze' onclick='tileFreeze(" + "tile" + randTileNum + ")'>" +
                                "<img src='images/freeze.png' width='30px' height='30px'>" + "</button>");

                        $(randTile).animate({height: "100%", opacity: "1"}, 1000);
                    }

                    else if(newItem.type === "video")
                    {
                        $(randTile).animate({height: "0", opacity: "0"}, 0);

                        $(randTile).attr('class', 'col-xs-' + (12/gridWidth) + ' col-fill');
                        $(randTile).css('background-image', '');
                        $(randTile).html(
                                "<p class='usernameimage'><a class='greytext' href='" +
                                newItem.userlink + "' target='_blank'>@" +
                                newItem.username + "</a></p>" +
                                "<video width='100%' height='100%' autoplay loop muted controls src='" +
                                newItem.url + 
                                "'style='object-fit: fill; top: 0; position: absolute;'></video>" +
                                "<button type='submit' class='itemfreezebtn' id='tile" + randTileNum +
                                "freeze' onclick='tileFreeze(" + "tile" + randTileNum + ")'>" +
                                "<img src='images/freeze.png' width='30px' height='30px'>" + "</button>");

                        $(randTile).animate({height: "100%", opacity: "1"}, 1000);
                    }
                }

                else if(newItem.service === "twitter")
                {
                    if(newItem.type === "text")
                    {
                        $(randTile).animate({height: "0", opacity: "0"}, 0);

                        $(randTile).attr('class', 'col-xs-' + (12/gridWidth) + ' col-fill fixitem');
                        $(randTile).css('background-image', 'url(images/TwitterLogo_blue.png)');
                        $(randTile).html(
                                "<div class='twittertext'>" + 
                                "<div class='twittertop'>" + 
                                "<img src='" + newItem.profilepic + "' width='20px' height='20px'> | " + "'>" + 
                                "<a class='twitterlink' href='" + newItem.userlink +
                                newItem.displayname + " (@" + newItem.username + ")" + "</a>" +
                                "</div><p class='tweet'>" + newItem.text + "</p></div>" +
                                "<button type='submit' class='itemfreezebtn' id='tile" + randTileNum +
                                "freeze' onclick='tileFreeze(" + "tile" + randTileNum + ")'>" +
                                "<img src='images/freeze.png' width='30px' height='30px'>" + "</button>");

                        $(randTile).animate({height: "100%", opacity: "1"}, 1000);
                    }

                    else if(newItem.type === "image")
                    {
                        $(randTile).animate({height: "0", opacity: "0"}, 0);

                        $(randTile).attr('class', 'col-xs-' + (12/gridWidth) + ' col-fill fixitem');
                        $(randTile).css('background-image', 'url(' + newItem.url + ')');
                        $(randTile).html(
                                "<div class='twitterimagetext'>" + 
                                "<div class='twittertop'>" + 
                                "<img src='" + newItem.profilepic + "' width='20px' height='20px'> | " + "'>" + 
                                "<a class='twitterlink' href='" + newItem.userlink +
                                newItem.displayname + " (@" + newItem.username + ")" + "</a>" +
                                "</div><p class='tweet'>" + newItem.text + "</p></div>" +
                                "<button type='submit' class='itemfreezebtn' id='tile" + randTileNum +
                                "freeze' onclick='tileFreeze(" + "tile" + randTileNum + ")'>" +
                                "<img src='images/freeze.png' width='30px' height='30px'>" + "</button>" +
                                "<div class='twitterlogo'><img src='images/TwitterLogo_blue.png' width='40px' height='40px'></div>");

                        $(randTile).animate({height: "100%", opacity: "1"}, 1000);
                    }
                }
                
                else if(newItem.service === "youtube")
                {
                    $(randTile).animate({height: "0", opacity: "0"}, 0);

                    $(randTile).attr('class', 'col-xs-' + (12/gridWidth) + ' col-fill');
                    $(randTile).css('background-image', '');
                    $(randTile).html(
                            "<iframe width='100%' height='100%' frameborder='0' src='" +
                            newItem.url + "" +
                            "' style='object-fit: fill; top: 0; position: absolute;'></iframe>" +
                            "<p class='usernameimage'><a class='greytext' href='" +
                            newItem.userlink + "' target='_blank'>@" +
                            newItem.username + "</a></p>" +
                            "<button type='submit' class='itemfreezebtn' id='tile" + randTileNum +
                            "freeze' onclick='tileFreeze(" + "tile" + randTileNum + ")'>" +
                            "<img src='images/freeze.png' width='30px' height='30px'>" + "</button>");

                    $(randTile).animate({height: "100%", opacity: "1"}, 1000);
                }

                if(currentItem !== null)
                {
                    currentItem.tile = "";
                    items.push(currentItem);
                    displayed.splice(currentIndex, 1);
                }

                newItem.tile = "tile" + randTileNum;
                displayed.push(newItem);
                items.splice(newIndex, 1);
            }

            else
            {
                refresh();
            }

    }
}