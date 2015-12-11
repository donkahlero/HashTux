
<html lang="en">
    <head>
        
        <title>HashTux</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <link rel="stylesheet" type="text/css" href="//cdn.datatables.net/1.10.10/css/jquery.dataTables.css">
        <link href="css/bootstrap.css" rel="stylesheet">
        <link href="css/stats.css" rel="stylesheet">  
        <link href="css/hashtux.css" rel="stylesheet">
        <script src="js/userstats_fetcher.js"></script>
        <script type="text/javascript" src="https://www.google.com/jsapi"></script>
        <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
        <script src="js/bootstrap.min.js"></script>
		<script type="text/javascript" charset="utf8" src="//cdn.datatables.net/1.10.10/js/jquery.dataTables.js"></script>
        <script type='text/javascript'>
        window.onload =  fetch('search_term_year');	
		
        // Load the Visualization API and the piechart package.
        google.load('visualization', '1.0', {'packages':['corechart', 'bar']});

        // Set a callback to run when the Google Visualization API is loaded.
        google.setOnLoadCallback(drawChart);
        

		var termtype;
		var type = "search_term";
		var period = "year"; 
		var items = [];
		
		function fetch(term) {
		    items = [];
		    var options = {request_type: "stats", options: []};
		    $.ajax({
		        url: "/ajax_post.php?search=" + term,
		        type: "post",                    
		        data: JSON.stringify(options),
		                    
		        success: function (myString) { 
		            termtype = term;
		            parse_to_items(myString);
		            creatTable();
		            
		            
		        }
		    });
		}

		function changeType(typeTerm){
			
			type = typeTerm;
			
            fetch(type + '_' + period);
            
			}
		
		function changePeriod(periodTerm){
			
			period = periodTerm; 
			fetch(type + '_' + period);
			
			}
		 
		var statsItems  = [];
		var  itemsEx = [];
		
		function getItemValues(){
			statsItems  = [];
			itemsEx = [];
			
			for(i = 0; i < items.length; i++)
							{
			              itemName = items[i].name; 
			              itemCount = items[i].value;
			              itemsEx = [itemName, itemCount];
			              
			              statsItems.push(itemsEx);
						}
			
			   }

	      function drawChart() {
			var chartItems = [];
				chartItems = statsItems.slice(Math.max(statsItems.length - 10, 1));
	        // Create the data table.
	        var data = new google.visualization.DataTable();
	        data.addColumn('string', 'Hashtag');
	        data.addColumn('number', 'Times searched');
	        data.addRows(chartItems);

	        // Set chart options
	        var options = {
	            'title': getTitle(),
	            'width':600,
	            'height':400,
	            animation: {"startup":true, "duration":5000}
	        };

	        // Instantiate and draw our chart, passing in some options.
	        var chart = new google.visualization.BarChart(document.getElementById('graphContainer'));
	        chart.draw(data, options);
	        
	        function getTitle($Name) {
	            $Name = 'NameOfChart';
	            return $Name;
	        }
	      }
			   
		
			function creatTable() {
					 getItemValues();
				
				switch (termtype){
	
				 case 'search_term_year':
	                             
	               $('#tableContainer').html("<table class='searchTermTableYear' id='searchTermTableYear' width='100%'></table>");
	             	        
						 $('#searchTermTableYear').DataTable( {
						    	retrieve: true,
						    	"aaSorting": [[1,'desc'], ],
						        data: statsItems,
						        
						       	columns: [		      																			
						        	{ title: "Top Search Terms for This Year" },
						            { title: "Count" },
						           
						     	] 
						    } );
						    break; 
				 case 'search_term_month': 
	                             
	                $('#tableContainer').html("<table class='searchTermTableMonth' id='searchTermTableMonth' width='100%'></table>");
					
				    	 $('#searchTermTableMonth').DataTable( {
				    		 	retrieve: true,
				    		 	"aaSorting": [[1,'desc']],
					            data: statsItems,
					            columns: [
					                { title: "Top Search Terms for Last Month" },
					                { title: "Count" },
					               
					            ]
					        } );
					 		break;
				 case 'search_term_week': 	
					 		
					$('#tableContainer').html("<table class='searchTermTableWeek' id='searchTermTableWeek' width='100%'></table>");
	
						$('#searchTermTableWeek').DataTable( {
					        	retrieve: true,
					        	"aaSorting": [[1,'desc']],
					            data: statsItems,
					            columns: [
					                { title: "Top Search Terms for Last Week" },
					                { title: "Count" },
					               
					            ]
					        } );
					        break; 
				 case 'search_term_today':
					 
					$('#tableContainer').html("<table class='searchTermTableToday' id='searchTermTableToday' width='100%'></table>");
					 
					    $('#searchTermTableToday').DataTable( {
					        	retrieve: true,
					        	"aaSorting": [[1,'desc']],
					            data: statsItems,
					            columns: [
					                { title: "Top Search Terms for the Last 24H" },
					                { title: "Count" },
					               
					            ]
					        } );
					        break;
				 case 'browser_version_year':
					 
					 $('#tableContainer').html("<table class='browserVersionTableYear' id='browserVersionTableYear' width='100%'></table>");
	
						 $('#browserVersionTableYear').DataTable( {
				        	retrieve: true,
				        	"aaSorting": [[1,'desc']],
				            data: statsItems,
				            columns: [
				                { title: "Top Browsers with Version for This Year" },
				                { title: "Count" },
				               
				            ]
				        } ); 
					    break;
				 case 'browser_version_month':
					 
					 $('#tableContainer').html("<table class='browserVersionTableMonth' id='browserVersionTableMonth' width='100%'></table>");
	
						 $('#browserVersionTableMonth').DataTable( {
				        	retrieve: true,
				        	"aaSorting": [[1,'desc']],
				            data: statsItems,
				            columns: [
				                { title: "Top Browsers with Version for Last Month" },
				                { title: "Count" },
				               
				            ]
				        } ); 
					    break;
				 case 'browser_version_week':
						 
						 $('#tableContainer').html("<table class='browserVersionTableWeek' id='browserVersionTableWeek' width='100%'></table>");
		
							 $('#browserVersionTableWeek').DataTable( {
					        	retrieve: true,
					        	"aaSorting": [[1,'desc']],
					            data: statsItems,
					            columns: [
					                { title: "Top Browsers with Version for Last Week" },
					                { title: "Count" },
					               
					            ]
					        } ); 
						    break;
				 case 'browser_version_today':
					 
					 $('#tableContainer').html("<table class='browserVersionTableToday' id='browserVersionTableToday' width='100%'></table>");
	
						 $('#browserVersionTableToday').DataTable( {
				        	retrieve: true,
				        	"aaSorting": [[1,'desc']],
				            data: statsItems,
				            columns: [
				                { title: "Top Browsers with Version for Last 24H" },
				                { title: "Count" },
				               
				            ]
				        } ); 
					    break;
				 case 'browser_year':
					 
					 $('#tableContainer').html("<table class='browserTableYear' id='browserTableYear' width='100%'></table>");
	
						 $('#browserTableYear').DataTable( {
				        	retrieve: true,
				        	"aaSorting": [[1,'desc']],
				            data: statsItems,
				            columns: [
				                { title: "Top Browsers for This Year" },
				                { title: "Count" },
				               
				            ]
				        } ); 
					    break;
 				case 'browser_month':
					 
					 $('#tableContainer').html("<table class='browserTableMonth' id='browserTableMonth' width='100%'></table>");
	
						 $('#browserTableMonth').DataTable( {
				        	retrieve: true,
				        	"aaSorting": [[1,'desc']],
				            data: statsItems,
				            columns: [
				                { title: "Top Browsers for Last Month" },
				                { title: "Count" },
				               
				            ]
				        } ); 
					    break;
 				case 'browser_week':
					 
					 $('#tableContainer').html("<table class='browserTableWeek' id='browserTableWeek' width='100%'></table>");
	
						 $('#browserTableWeek').DataTable( {
				        	retrieve: true,
				        	"aaSorting": [[1,'desc']],
				            data: statsItems,
				            columns: [
				                { title: "Top Browsers for Last Week" },
				                { title: "Count" },
				               
				            ]
				        } ); 
					    break;
 				case 'browser_today':
					 
					 $('#tableContainer').html("<table class='browserTableToday' id='browserTableToday' width='100%'></table>");
	
						 $('#browserTableToday').DataTable( {
				        	retrieve: true,
				        	"aaSorting": [[1,'desc']],
				            data: statsItems,
				            columns: [
				                { title: "Top Browsers for Last Today" },
				                { title: "Count" },
				               
				            ]
				        } ); 
					    break;
				 
				 case 'platform_year':
					 
					 $('#tableContainer').html("<table class='platformTableYear' id='platformTableYear' width='100%'></table>");
	
						 $('#platformTableYear').DataTable( {
				        	retrieve: true,
				        	"aaSorting": [[1,'desc']],
				            data: statsItems,
				            columns: [
				                { title: "Top Platforms This Year" },
				                { title: "Count" },
				               
				            ]
				        } ); 
					    break;
	 			 case 'platform_month':
					 
					 $('#tableContainer').html("<table class='platformTableMonth' id='platformTableMonth' width='100%'></table>");
	
						 $('#platformTableMonth').DataTable( {
				        	retrieve: true,
				        	"aaSorting": [[1,'desc']],
				            data: statsItems,
				            columns: [
				                { title: "Top Platforms For Last Month" },
				                { title: "Count" },
				               
				            ]
				        } ); 
					    break;
	 			case 'platform_week':
					 
					 $('#tableContainer').html("<table class='platformTableWeek' id='platformTableWeek' width='100%'></table>");
	
						 $('#platformTableWeek').DataTable( {
				        	retrieve: true,
				        	"aaSorting": [[1,'desc']],
				            data: statsItems,
				            columns: [
				                { title: "Top Platforms For Last Week" },
				                { title: "Count" },
				               
				            ]
				        } ); 
					    break;
	 			case 'platform_today':
					 
					 $('#tableContainer').html("<table class='platformTabletoday' id='platformTabletoday' width='100%'></table>");
	
						 $('#platformTabletoday').DataTable( {
				        	retrieve: true,
				        	"aaSorting": [[1,'desc']],
				            data: statsItems,
				            columns: [
				                { title: "Top Platforms For Last 24H" },
				                { title: "Count" },
				               
				            ]
				        } ); 
					    break;
	 			case 'platform_browser_year':
					 
					 $('#tableContainer').html("<table class='platformBrowserTable' id='platformBrowserTable' width='100%'></table>");
	
						 $('#platformBrowserTable').DataTable( {
				        	retrieve: true,
				        	"aaSorting": [[1,'desc']],
				            data: statsItems,
				            columns: [
				                { title: "Top Platforms use with Browser for This Year" },
				                { title: "Count" },
				               
				            ]
				        } ); 
					    break;
	 			case 'platform_browser_month':
					 
					 $('#tableContainer').html("<table class='platformBrowserMonth' id='platformBrowserMonth' width='100%'></table>");
	
						 $('#platformBrowserMonth').DataTable( {
				        	retrieve: true,
				        	"aaSorting": [[1,'desc']],
				            data: statsItems,
				            columns: [
				                { title: "Top Platforms use with Browser for Last Month" },
				                { title: "Count" },
				               
				            ]
				        } ); 
					    break;
	 			case 'platform_browser_week':
					 
					 $('#tableContainer').html("<table class='platformBrowserWeek' id='platformBrowserWeek' width='100%'></table>");
	
						 $('#platformBrowserWeek').DataTable( {
				        	retrieve: true,
				        	"aaSorting": [[1,'desc']],
				            data: statsItems,
				            columns: [
				                { title: "Top Platforms use with Browser for Last Week" },
				                { title: "Count" },
				               
				            ]
				        } ); 
					    break;
	 			case 'platform_browser_today':
					 
					 $('#tableContainer').html("<table class='platformBrowserDay' id='platformBrowserDay' width='100%'></table>");
	
						 $('#platformBrowserDay').DataTable( {
				        	retrieve: true,
				        	"aaSorting": [[1,'desc']],
				            data: statsItems,
				            columns: [
				                { title: "Top Platforms use with Browser for Last 24H" },
				                { title: "Count" },
				               
				            ]
				        } ); 
					    break;
				 }
			
		           
		}  
    </script>
    
</head>

<body>
	<div >
        <h2 align="center">Welcome to HashTux User Habits Statistics</h2>
    </div>
<div class="container">
 <nav class="navbar navbar-inverse">
  <div class="container-fluid">
    <div class="navbar-header">
      <a class="navbar-brand" href="">HashTux</a>
    </div>
    <div>
      <ul class="nav navbar-nav ">
        <li><a class="btn" 	onclick="changeType('search_term')">Search Term</a></li>
        <li><a class="btn"	onclick="changeType('browser')">Browser</a></li>
        <li><a class="btn"	onclick="changeType('platform')">Platform</a></li>     
       <li class="dropdown">
          <a class="dropdown-toggle" data-toggle="dropdown" href="#">Combinations	
          <ul class="dropdown-menu">
           <li><a class="btn"		onclick="changeType('platform_browser')">Platform/Browser</a></li>
          	  <li><a class="btn"	onclick="changeType('browser_version')">Browser/Version</a></li>
          </ul>
        </li>
      </ul>
      <ul class="nav navbar-nav navbar-right"> 
       <li class="dropdown">
          <a class="dropdown-toggle" data-toggle="dropdown" href="#">Period
          <span class="caret"></span></a>
          <ul class="dropdown-menu">
            <li><a class="btn" 	onclick="changePeriod('today')">Last 24H</a></li>
            <li><a class="btn" 	onclick="changePeriod('week')">Last Week</a></li>
            <li><a class="btn" 	onclick="changePeriod('month')">Last Month</a></li>
            <li><a class="btn" 	onclick="changePeriod('year')">Last Year</a></li>
          </ul>
        </li>
      </ul> 
    </div>  
  </div>
</div>
     <div class="container" >
        <div class="row" >
            <div class="col-md-6" id="tableContainer"></div>
            	<div class="col-md-6" id="graphContainer">
                </div> 
            </div>
            
        </div>
    </div>
  </header>
</body>
</html>
