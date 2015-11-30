function refresh() 
{

    if(items.length > 0 && !screenFrozen)
    {

        var newIndex = Math.floor((Math.random() * items.length));
        var newItem = items[newIndex];
        var randTileNum = Math.floor((Math.random() * totalItems));
        var randTile = "#tile" + randTileNum;
        var currentItem;
        var currentIndex;

            for(i = 0; i < displayed.length; i++)
            {
                if(displayed[i].tile === "tile" + randTileNum)
                {
                    currentItem = displayed[i];
                    currentIndex = i;
                }
            }

            if(!currentItem.frozen)
            {
                if(newItem.service === "instagram")
                {
                    if(newItem.type === "image") 
                    {
                        $(randTile).animate({height: "0", opacity: "0"}, 0);

                        $(randTile).attr('class', 'col-xs-' + (12/gridWidth) + ' col-fill fixitem');
                        $(randTile).attr('onclick', 'tileFreeze(tile' + randTileNum + ')');
                        $(randTile).css('background-image', 'url(' + newItem.url + ')');
                        $(randTile).html("<p class='usernameimage'><a class='greytext' href='" +
                                newItem.userlink + "' target='_blank'>@" +
                                newItem.username + "</a></p>");

                        $(randTile).animate({height: "100%", opacity: "1"}, 1000);
                    }

                    else if(newItem.type === "video")
                    {
                        $(randTile).animate({height: "0", opacity: "0"}, 0);

                        $(randTile).attr('class', 'col-xs-' + (12/gridWidth) + ' col-fill');
                        $(randTile).attr('onclick', 'tileFreeze(tile' + randTileNum + ')');
                        $(randTile).css('background-image', '');
                        $(randTile).html(
                                "<p class='usernameimage'><a class='greytext' href='" +
                                newItem.userlink + "' target='_blank'>@" +
                                newItem.username + "</a></p>" +
                                "<video width='100%' height='100%' autoplay loop muted controls src='" +
                                newItem.url + 
                                "'style='object-fit: fill; top: 0; position: absolute;'></video>");

                        $(randTile).animate({height: "100%", opacity: "1"}, 1000);
                    }
                }

                else if(newItem.service === "twitter")
                {
                    if(newItem.type === "text")
                    {
                        $(randTile).animate({height: "0", opacity: "0"}, 0);

                        $(randTile).attr('class', 'col-xs-' + (12/gridWidth) + ' col-fill fixitem');
                        $(randTile).attr('onclick', 'tileFreeze(tile' + randTileNum + ')');
                        $(randTile).css('background-image', '');
                        $(randTile).html(
                                "<div class='twittertext'><p>" +
                                newItem.text +
                                "</p><p class='usernametweet'><a class='greytext' href='" +
                                newItem.userlink + "' target='_blank'>@" +
                                newItem.username + "</a></p>" +
                                "</div>");

                        $(randTile).animate({height: "100%", opacity: "1"}, 1000);
                    }

                    else if(newItem.type === "image")
                    {
                        $(randTile).animate({height: "0", opacity: "0"}, 0);

                        $(randTile).attr('class', 'col-xs-' + (12/gridWidth) + ' col-fill fixitem');
                        $(randTile).attr('onclick', 'tileFreeze(tile' + randTileNum + ')');
                        $(randTile).css('background-image', 'url(' + newItem.url + ')');
                        $(randTile).html(
                                "<div class='twitterimagetext'><p>" +
                                newItem.text +
                                "</p><p class='usernametweet'><a class='greytext' href='" +
                                newItem.userlink + "' target='_blank'>@" +
                                newItem.username + "</a></p>" +
                                "</div>");

                        $(randTile).animate({height: "100%", opacity: "1"}, 1000);
                    }
                }
                
                else if(newItem.service === "youtube")
                {
                    $(randTile).animate({height: "0", opacity: "0"}, 0);

                    $(randTile).attr('class', 'col-xs-' + (12/gridWidth) + ' col-fill');
                    $(randTile).attr('onclick', 'tileFreeze(tile' + randTileNum + ')');
                    $(randTile).css('background-image', '');
                    $(randTile).html(
                            "<video width='100%' height='100%' autoplay loop muted controls src='" +
                            newItem.url + 
                            "'style='object-fit: fill; top: 0; position: absolute;'></video>" +
                            "<p class='usernameimage'><a class='greytext' href='" +
                            newItem.userlink + "' target='_blank'>@" +
                            newItem.username + "</a></p>");

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