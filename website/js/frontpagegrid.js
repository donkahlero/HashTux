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

    for(k = 0; k < 12; k++) {

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

    for(i = 0; i < 3; i++) {

        var cols = "";

        for(j = 0; j < 4; j++) {

            if(count >= displayed.length) 
            {

                cols = cols + 
                    "<div class='col-xs-" +
                    (12/4) + 
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
                        (12/4) + 
                        " col-fill fixitem' style='background-image:url(" +
                        displayed[count].url +
                        ");' id='tile" + count + "'>" +
                        "</div>";

                    displayed[count].tile = "tile" + count;
                    count++;

                }
            }
        }

        grid.html(grid.html() +
            "<div class='row-grid-" +
            (12/3) + "' id='row" +
            i + "'>" + cols + "</div>");
    }
}