<?php
	/* Start a new session or resume if the client has a cookie ;) */
	session_start();
?>
<html lang="en">
  <head>   
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    
    <title>HashTux Stats</title>
    
    <link href="css/bootstrap.css" rel="stylesheet">
    <link href="css/stats.css" rel="stylesheet">  
    <link href="css/hashtux.css" rel="stylesheet">
    <script type="text/javascript" src="https://www.google.com/jsapi"></script>
    <script type="text/javascript"></script>
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
    <script src="js/bootstrap.min.js"></script>
    <script src="js/userstats_fetcher.js"></script>
    <script src="js/graph.js"></script>
    <script>

function statsObj(name,value) {
	  this.name = name;
	  this.value = value; 
	}

var statsList = [new statsObj("hashtux",31),
	                    new statsObj("Summer",12),
	                    new statsObj("Sunny",13),
	                    new statsObj("Candy",14),
					    new statsObj("Hash",6),
					    new statsObj("tux",9),
						new statsObj("lol",10),
						new statsObj("Pepperoni",2),
						new statsObj("beautiful",5),
						new statsObj("beauty",5)
					   ];


function creatTable(){
     var table = document.createElement('table');
	 var tableBody = document.createElement('tbody');

		for(i = 0; i < items.length; i++)
			{
			    var num = i + 1;
				var row = document.createElement('tr');
				var numberCell = document.createElement('td');
				var nameCell = document.createElement('td');
				var valueCell = document.createElement('td');
				
				numberCell.appendChild(document.createTextNode(num.toString()));
				nameCell.appendChild(document.createTextNode(items[i].name));
				valueCell.appendChild(document.createTextNode(items[i].value));
		
		    	row.appendChild(numberCell);
		    	row.appendChild(nameCell);
		    	row.appendChild(valueCell);
		    	tableBody.appendChild(row);
		    	
			}
		table.appendChild(tableBody);
		$('#myTableSearchterm').html(table);
		
    }
    window.onload = function(){
        fetch("search_term_today");
    }

    </script>
    
  </head>
  
	  <body>
	      
	    <div>
	        <h1 align="center">#hashtux</h1>
	        <h2 align="center">Welcome to HashTux User Habits Statistics</h2>
	    </div>
	        
	    <div class="container" align="center">
	               <div class="input-group" style="float:left;" >
	                 	<div class="dropdown">
	              			<button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown">Select a sorting Field  
	                			  <span class="caret"></span></button>
	                      			<ul class="dropdown-menu multi-level" role="menu" aria-labelledby="dropdownMenu">
	                        			<li><a href="#" id="search_termButton" onclick="creatTable();return false;"> Search Terms </a></li>
	      								<li><a href="#" id="browserButton"     onclick="showBrowserTable();return false;"> Browsers </a></li>
	     								<li><a href="#" id="platformButton"    onclick="showPlatformTable();return false;"> Platforms </a></li>
	      								<li class="divider"></li>
	      								<li class="dropdown-submenu">
							                <a tabindex="-1" href="#">Top Search Terms&nbsp;&nbsp;</a>
							                <ul class="dropdown-menu">
							                  <li><a tabindex="-1" href="#">&nbsp;&nbsp;To Day&nbsp;&nbsp;</a></li>
							                  <li><a tabindex="-1" href="#">&nbsp;&nbsp;Last Week&nbsp;&nbsp;</a></li>
							                  <li><a tabindex="-1" href="#">&nbsp;&nbsp;Last Month&nbsp;&nbsp;</a></li>
							                  <li><a tabindex="-1" href="#">&nbsp;&nbsp;Last Year&nbsp;&nbsp;</a></li>
							                </ul>
							              </li>
									   <li class="dropdown-submenu">
				                           <a tabindex="-1" href="#">Top Browsers</a>
							                <ul class="dropdown-menu">
							                  <li><a tabindex="-1" href="#">&nbsp;&nbsp;To Day&nbsp;&nbsp;</a></li>
							                  <li><a tabindex="-1" href="#">&nbsp;&nbsp;Last Week&nbsp;&nbsp;</a></li>
							                  <li><a tabindex="-1" href="#">&nbsp;&nbsp;Last Month&nbsp;&nbsp;</a></li>
							                  <li><a tabindex="-1" href="#">&nbsp;&nbsp;Last Year&nbsp;&nbsp;</a></li>
							                </ul>
							              </li>
							              <li class="dropdown-submenu">
							                <a tabindex="-1" href="#">Top Platforms</a>
							                <ul class="dropdown-menu">
							                  <li><a tabindex="-1" href="#">&nbsp;&nbsp;To Day&nbsp;&nbsp;</a></li>
							                  <li><a tabindex="-1" href="#">&nbsp;&nbsp;Last Week&nbsp;&nbsp;</a></li>
							                  <li><a tabindex="-1" href="#">&nbsp;&nbsp;Last Month&nbsp;&nbsp;</a></li>
							                  <li><a tabindex="-1" href="#">&nbsp;&nbsp;Last Year&nbsp;&nbsp;</a></li>
							                </ul>
							              </li>
	   						 		</ul>
	 			     	</div>
	            </div>
	              
	    </div>
			      
			    <div class="container">
			        <div class="row">
			            <div class="col-md-6">
			        <table  table class= "table table-hover"  id="myTableSearchterm" width="100%" > 
			                 <h3>Search terms </h3> 
			         </table>
			            
			        <table table class= "table table-hover" id="myTableBrowser" width="100%">  
			                <h3>Top 10 Browsers </h3> 
			                  
			         </table>
			
			        <table table class= "table table-hover" id="myTablePlatform" width="100%">  
			                <h3>Platforms </h3>       
			        </table>
			            </div>
			            
			            <div class="col-md-6">
			             <!--Div that will hold the pie chart-->
			               <div class="container-fluid" align="center" width="100%" id="""chart_div">
			                </div> 
			            </div>
			
			        </div>
			    </div> 
					 
	  </body>
</html>




    
