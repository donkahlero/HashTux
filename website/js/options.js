var size = "md";
var refreshRate = "md";

var types = new type(true, true, true);
var services = new service(true, true, true);

var language = "";
var savedTypes = [];
var savedServices = [];

function type(image, video, text) {
    this.image = image;
    this.video = video;
    this.text = text;
}

function service(twitter, instagram, youtube) {
    this.twitter = twitter;
    this.instagram = instagram;
    this.youtube = youtube;
}

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

    // Check if the size tracker is small, medium or large. Then
    // set the gridWidth, gridHeight and totalItems acoordingly.

    if(size === "sm")
    {
        gridWidth = 6;
        gridHeight = 4;
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
        gridWidth = 3;
        gridHeight = 2;
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
        savedServices.push("twitter");

    if(services.instagram === true)
        savedServices.push("instagram");

    if(services.youtube === true)
        savedServices.push("youtube");
    
    options.request_type = "update";
    
    if(savedTypes.length !== 3 && savedTypes.length !== 0)
    {
        options.content_type = savedTypes;
    }
    
    else
    {
        delete options.content_type;
    }
    
    if(savedServices.length !== 3 && savedServices.length !== 0)
    {
        options.service = savedServices;
    }
    
    else
    {
        delete options.service;
    }
    
    if(language !== "")
    {
        options.language = language;
    }
    
    else
    {
        delete options.language;
    }

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
        size = "sm";

        $('#size-sm').attr('class', 'btn btn-primary btn-md');
        $('#size-md').attr('class', 'btn btn-default btn-md');
        $('#size-lg').attr('class', 'btn btn-default btn-md');

    }

    else if(id === 'size-md')
    {
        size = "md";

        $('#size-sm').attr('class', 'btn btn-default btn-md');
        $('#size-md').attr('class', 'btn btn-primary btn-md');
        $('#size-lg').attr('class', 'btn btn-default btn-md');
    }

    else
    {
        size = "lg";

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
    $('#options').fadeIn(500);
    freeze();
    initOptions();
    
    $('#optionsPanel').click(function(e) {
        e.stopPropagation();
    });
}

function hideOptions() {
    $('#options').fadeOut(500);
    unfreeze();

    $('#aborted').fadeTo(3000, 500).slideUp(500, function() {
        $('#aborted').alert('close');
    });
}