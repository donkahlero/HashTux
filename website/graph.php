<html>
  <head>
    <!--Load the AJAX API-->
    <script type="text/javascript" src="https://www.google.com/jsapi"></script>
    <script type="text/javascript">

      // Load the Visualization API and the piechart package.
      google.load('visualization', '1.0', {'packages':['corechart', 'bar']});

      // Set a callback to run when the Google Visualization API is loaded.
      google.setOnLoadCallback(drawChart);

      // Callback that creates and populates a data table,
      // instantiates the pie chart, passes in the data and
      // draws it.
      
    
      function drawChart() {

        // Create the data table.
        var data = new google.visualization.DataTable();
        data.addColumn('string', 'Hashtag');
        data.addColumn('number', 'Times searched');
        data.addRows(getItemValues());

        // Set chart options
        var options = {
            'title': getTitle(),
            'width':600,
            'height':400,
            animation: {"startup":true, "duration":5000}
        };

        // Instantiate and draw our chart, passing in some options.
        var chart = new google.visualization.BarChart(document.getElementById('chart_div'));
        chart.draw(data, options);
        
        function getTitle($Name) {
            $Name = 'NameOfChart';
            return $Name;
        }
      }
    </script>
  </head>

  <body>
     <div
        <!--Div that will hold the pie chart-->
        <div id="chart_div"></div>
        <button type="button" class="btn btn-default btn-md" id="reload"
                style="float:right; margin-right: 760px;" onclick="drawChart()"> reload</button>
    </div>
  </body>
</html
>