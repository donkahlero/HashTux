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
    
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
    <script src="js/bootstrap.min.js"></script>
    <script src="js/userstats_fetcher.js"></script>
    <script>
    

function showSearchtermTable(){
	  $("#myTableBrowser").hide();
	  $("#myTablePlatform").hide();
	  $("#myTableLanguage").hide();
	  $("#myTableSearchterm").show();	
         parse_to_items();
    }
function showBrowserTable(){
	  $("#myTableSearchterm").hide();
	  $("#myTablePlatform").hide();
	  $("#myTableLanguage").hide();	
	  $("#myTableBrowser").show();
  }

function showPlatformTable(){
	  $("#myTableSearchterm").hide();	
	  $("#myTableBrowser").hide();
	  $("#myTableLanguage").hide();
	  $("#myTablePlatform").show();
    }
function showLanguageTable(){
	  $("#myTableSearchterm").hide();	
	  $("#myTableBrowser").hide();
	  $("#myTablePlatform").hide();
	  $("#myTableLanguage").show();
  }
function showallTables(){
	  $("#myTableSearchterm").show();	
	  $("#myTableBrowser").show();
	  $("#myTablePlatform").show();
	  $("#myTableLanguage").show();
	
}
function hideallTables(){
	 $("#myTableSearchterm").hide();	
	  $("#myTableBrowser").hide();
	  $("#myTablePlatform").hide();
	  $("#myTableLanguage").hide();
}
    </script>
    
  </head>
  
  <body>
      
    <div>
        <h1 align="center">#hashtux</h1>
        <h2 align="center">Welcome to HashTux User Habits Statistics</h2>
    </div>
        
    <div class="container" align="center">
               <div class="input-group" align="center">
                 	<div class="dropdown">
              			<button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown">Select a sorting Field  
                			  <span class="caret"></span></button>
                      			<ul class="dropdown-menu">
                        			<li><a href="#" id="search_termButton" onclick="showSearchtermTable();return false;"> Search Term </a></li>
      								<li><a href="#" id="browserButton"     onclick="showBrowserTable();return false;"> Browser </a></li>
     								<li><a href="#" id="platformButton"    onclick="showPlatformTable();return false;"> Platform </a></li>
      								<li><a href="#" id="languageButton"    onclick="showLanguageTable();return false;"> Language </a></li>
      								<li class="divider"></li>
      								<li><a href="#" id="showallButton"    onclick="showallTables();return false;"> Show all </a></li>
      								<li><a href="#" id="hideallButton"    onclick="hideallTables();return false;"> Hide all </a></li>
   						 		</ul>
 			     	</div>
             </div>
    </div>
      
    <div class="container">
          
        <div class="row">
    <div class="col-md-6">
    <h3>User Habits </h3>

        <table table class= "table table-hover" id="myTableSearchterm" width="100%" >  
                 <thead>  
                   <tr>  
                     <th>Top 10 search terms</th>
                   </tr>  
                 </thead>  
                 <tbody>  
                   <tr>  
                   <td>
                     1.Hashtux<br>
                     2.Summer<br>   
                     3.Sun<br>  
                     4.Candy<br>  
                     5.Pepperoni<br>
                     </td>  
                   </tr>  
                 </tbody>  
               </table>

            <table table class= "table table-hover" id="myTableBrowser" width="100%">  
                         <thead>  
                           <tr>  
                             <th>Top 10 Browsers</th>   
                           </tr>  
                         </thead>  
                         <tbody>  
                           <tr>  
                           <td>
                             1.Safari<br>
                             2.Firefox<br>   
                             3.Chrome<br>  
                             4.Internet Explorer<br>  
                             5.Opera<br> 
                             </td>  
                           </tr>  
                         </tbody>  
                       </table>

                 <table table class= "table table-hover" id="myTablePlatform" width="100%">  
                         <thead>  
                           <tr>  
                             <th>Top Platforms</th>   
                           </tr>  
                         </thead>  
                         <tbody>  
                           <tr>  
                           <td>
                             1.Microsoft Windows<br>
                             2.Linux<br>   
                             3.iOS<br>  
                             4.Android<br>  
                             </td>  
                           </tr>  
                         </tbody>  
                       </table>

                 <table table class= "table table-hover" id="myTableLanguage" width="100%">  
                         <thead>  
                           <tr>  
                             <th>Top 10 Languages</th>   
                           </tr>  
                         </thead>  
                         <tbody>  
                           <tr>  
                           <td>
                             1.English<br>
                             2.French<br>   
                             3.German<br>  
                             4.Spanish<br> 
                             5.Swedish<br>    
                             </td>  
                           </tr>  
                         </tbody>  
                       </table>

            </div>
            
            <div class="col-md-6">
                <div class="container-fluid" width="100%">
                                            <button type="button" class="btn btn-default btn-md" id="optionsBtn"
                                style="float:right;" onclick="freeze()">
                            Freeze
                        </button>
                </div>   
            </div>

        </div>
    </div>
		 
  </body>
</html>




    
