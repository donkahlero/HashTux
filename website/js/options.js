var size = "md";
var refreshRate = "md";

var types = new type(true, false, true);
var services = new service(true, true, false);

var language = "";
var savedTypes = [];
var savedServices = [];

function initOptions() {
    $('#size-' + size).attr('class', 'btn btn-primary btn-md');
    $('#ref-' + refreshRate).attr('class', 'btn btn-primary btn-md');
    
    if(types.image === true)
        $('#type-img').attr('class', 'btn btn-primary btn-md');
    
    if(types.video === true)
        $('#type-vid').attr('class', 'btn btn-primary btn-md');
    
    if(types.text === true)
        $('#type-txt').attr('class', 'btn btn-primary btn-md');
    
    
    if(services.twitter === true)
        $('#serv-twitter').attr('class', 'btn btn-primary btn-md');
    
    if(services.instagram === true)
        $('#serv-instagram').attr('class', 'btn btn-primary btn-md');
    
    if(services.youtube === true)
        $('#serv-youtube').attr('class', 'btn btn-primary btn-md');
    
    
    if(language !== "")
        $('#' + language).attr('class', 'btn btn-primary btn-md');
}

function saveOptions() {

    alert("Types\n" +
            "Images: " + types.image + "\n" +
            "Videos: " + types.video + "\n" +
            "Text: " + types.text + "\n\n" +
            "Services\n" +
            "Twitter: " + services.twitter + "\n" +
            "Instagram: " + services.instagram + "\n" +
            "YouTube: " + services.youtube + "\n\n" +
            "Language: " + language);

    // Check if the size tracker is small, medium or large. Then
    // set the gridWidth, gridHeight and totalItems acoordingly.

    if(size === "sm")
    {
        gridWidth = 3;
        gridHeight = 2;
        totalItems = gridWidth * gridHeight;
    }

    else if(size === "md")
    {
        gridWidth = 4;
        gridHeight = 3;
        totalItems = gridWidth * gridHeight;
    }

    else if(size === "lg")
    {
        gridWidth = 6;
        gridHeight = 4;
        totalItems = gridWidth * gridHeight;
    }

    // Checks whether the refresh rate tracker is slow, medium or fast
    // and changes the refresh rate accordingly.

    if(refreshRate === "slow")
    {
        clearInterval(refreshTimer);
        refreshTimer = setInterval(refresh, 10000);
    }

    else if(refreshRate === "md")
    {
        clearInterval(refreshTimer);
        refreshTimer = setInterval(refresh, 5000);
    }

    else if(refreshRate === "fast")
    {
        clearInterval(refreshTimer);
        refreshTimer = setInterval(refresh, 2000);
    }

    savedTypes = [];
    savedServices = [];

    // Types

    if(types.image === true)
        savedTypes.push("image");

    if(types.video === true)
        savedTypes.push("video");

    if(types.text === true)
        savedTypes.push("text");

    // Services

    if(services.twitter === true)
        savedTypes.push("twitter");

    if(services.instagram === true)
        savedTypes.push("instagram");

    if(services.youtube === true)
        savedTypes.push("youtube");

    // Show an alert to notify the user that the changes are saved

    $('#saved').fadeTo(3000, 500).slideUp(500, function() {
        $('#saved').alert('close');
    });

    hideOptions();      // Hide the options menu
    reinitialize();     // Reinitialize the grid
    unfreeze();         // Unfreeze the tiles
}

function changeSize(id) {

    if(id === 'size-sm')
    {
        size = "small";

        $('#size-sm').attr('class', 'btn btn-primary btn-md');
        $('#size-md').attr('class', 'btn btn-default btn-md');
        $('#size-lg').attr('class', 'btn btn-default btn-md');

    }

    else if(id === 'size-md')
    {
        size = "medium";

        $('#size-sm').attr('class', 'btn btn-default btn-md');
        $('#size-md').attr('class', 'btn btn-primary btn-md');
        $('#size-lg').attr('class', 'btn btn-default btn-md');
    }

    else
    {
        size = "large";

        $('#size-sm').attr('class', 'btn btn-default btn-md');
        $('#size-md').attr('class', 'btn btn-default btn-md');
        $('#size-lg').attr('class', 'btn btn-primary btn-md');
    }
}

function changeRefRate(id) {

    if(id === 'ref-slow')
    {
        refreshRate = "slow";

        $('#ref-slow').attr('class', 'btn btn-primary btn-md');
        $('#ref-md').attr('class', 'btn btn-default btn-md');
        $('#ref-fast').attr('class', 'btn btn-default btn-md');

    }

    else if(id === 'ref-md')
    {
        refreshRate = "medium";

        $('#ref-slow').attr('class', 'btn btn-default btn-md');
        $('#ref-md').attr('class', 'btn btn-primary btn-md');
        $('#ref-fast').attr('class', 'btn btn-default btn-md');
    }

    else
    {
        refreshRate = "fast";

        $('#ref-slow').attr('class', 'btn btn-default btn-md');
        $('#ref-md').attr('class', 'btn btn-default btn-md');
        $('#ref-fast').attr('class', 'btn btn-primary btn-md');
    }
}

function changeType(id) {

    if(id === 'type-img')
    {
        if(types.image === true)
        {
            types.image = false;
            $('#type-img').attr('class', 'btn btn-default btn-md');
        }

        else
        {
            types.image = true;
            $('#type-img').attr('class', 'btn btn-primary btn-md');
        }           
    }

    else if(id === 'type-vid')
    {
        if(types.video === true)
        {
            types.video = false;
            $('#type-vid').attr('class', 'btn btn-default btn-md');
        }

        else
        {
            types.video = true;
            $('#type-vid').attr('class', 'btn btn-primary btn-md');
        } 
    }

    else
    {
        if(types.text === true)
        {
            types.text = false;
            $('#type-txt').attr('class', 'btn btn-default btn-md');
        }

        else
        {
            types.text = true;
            $('#type-txt').attr('class', 'btn btn-primary btn-md');
        } 
    }
}

function changeService(id) {

    if(id === 'serv-twitter')
    {
        if(services.twitter === true)
        {
            services.twitter = false;
            $('#serv-twitter').attr('class', 'btn btn-default btn-md');
        }

        else
        {
            services.twitter = true;
            $('#serv-twitter').attr('class', 'btn btn-primary btn-md');
        }           
    }

    if(id === 'serv-instagram')
    {
        if(services.instagram === true)
        {
            services.instagram = false;
            $('#serv-instagram').attr('class', 'btn btn-default btn-md');
        }

        else
        {
            services.instagram = true;
            $('#serv-instagram').attr('class', 'btn btn-primary btn-md');
        }           
    }

    if(id === 'serv-youtube')
    {
        if(services.youtube === true)
        {
            services.youtube = false;
            $('#serv-youtube').attr('class', 'btn btn-default btn-md');
        }

        else
        {
            services.youtube = true;
            $('#serv-youtube').attr('class', 'btn btn-primary btn-md');
        }           
    }
}

function changeLanguage(id) {
    if(language !== "")
    {
        $('#' + language).attr('class', 'btn btn-default btn-md');
    }

    $('#' + id).attr('class', 'btn btn-primary btn-md');
    language = id;
}

function showOptions() {
    initOptions();
    freeze();
    
    $('#options').fadeIn(500);

    $('#optionsPanel').click(function () {
        event.stopPropagation();
    });
}

function hideOptions() {
    unfreeze();
    
    $('#options').fadeOut(500);

    $('#aborted').fadeTo(3000, 500).slideUp(500, function() {
        $('#aborted').alert('close');
    });
}