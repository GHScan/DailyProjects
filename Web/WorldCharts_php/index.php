<!DOCTYPE html>
<html>
<head>
    <link rel="stylesheet" href="http://ajax.googleapis.com/ajax/libs/jquerymobile/1.4.5/jquery.mobile.min.css" />
    <title>World charts</title>
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"></script>
    <script>
        $(document).bind('mobileinit', function () {
            $.mobile.pushStateEnabled = false;
        });
    </script>
    <script src="http://ajax.googleapis.com/ajax/libs/jquerymobile/1.4.5/jquery.mobile.min.js"></script>
    <script src="https://www.gstatic.com/charts/loader.js"></script>
    <script src="https://www.google.com/jsapi"></script>
</head>
<body>
    <h1 align="center">World charts</h1>
    <div id="tabs" data-role="tabs">
        <div data-role="navbar">
            <ul>
                <li><a href="#GDP" data-ajax="false">GDP</a></li>
                <li><a href="#GDP_PPP" data-ajax="false">GDP(PPP)</a></li>
                <li><a href="#GDP_growth_rate" data-ajax="false">GDP(growth rate)</a></li>
                <li><a href="#Area" data-ajax="false">Area</a></li>
                <li><a href="#Population" data-ajax="false">Popluation</a></li>
                <li><a href="#Population_growth_rate" data-ajax="false">Popluation(growth rate)</a></li>
                <li><a href="#Population_density" data-ajax="false">Popluation density</a></li>
                <li><a href="#GDP_per_capita" data-ajax="false">GDP per capita</a></li>
                <li><a href="#GDP_PPP_per_capita" data-ajax="false">GDP(PPP) per capita</a></li>
            </ul>
        </div>
        <div id="GDP" colors="white;green"></div>
        <div id="GDP_PPP" colors="white;green"></div>
        <div id="GDP_growth_rate" colors="red;orangered;white;lightgreen;green"></div>
        <div id="Area" colors="white;blue"></div>
        <div id="Population" colors="white;lightgreen;lightgreen;green;green"></div>
        <div id="Population_growth_rate" colors="red;lightgreen;green"></div>
        <div id="Population_density" colors="white;green;green;orangered;orangered;red;red;red;red;red;red;red;red;red;red;red;red;red;red;red;red"></div>
        <div id="GDP_per_capita" colors="orangered;white;lightgreen;lightgreen;lightgreen;lightgreen;lightgreen;green;green;green;green"></div>
        <div id="GDP_PPP_per_capita" colors="orangered;white;lightgreen;lightgreen;lightgreen;lightgreen;lightgreen;green;green;green;green"></div>
    </div>

    <script>
        google.charts.load('current', { 'packages': ['geochart'] });
        google.charts.setOnLoadCallback(onChartsLoad);

        function onChartsLoad() {

            <?php  

                $labels = array('GDP', 'GDP_PPP', 'GDP_growth_rate', 'Area', 'Population', 'Population_growth_rate', 'Population_density', 'GDP_per_capita', 'GDP_PPP_per_capita');
                foreach ($labels as $label) {
                    ?>

                    (function() {
                        var records = [
                            ['Country', <?php echo "'$label'" ?>],

                            <?php
                                foreach (file($label . '.csv') as $line) {
                                    $tokens = explode(',', $line);
                                    $country = $tokens[0];
                                    $number = (float)$tokens[1];
                                    echo "['$country', $number],\n";
                                }
                            ?>
                        ];

                        var panel = $('#' + <?php echo "'$label'" ?>).attr('align', 'center');

                        var data = google.visualization.arrayToDataTable(records);
                        var chart = new google.visualization.GeoChart(panel[0]);
                        chart.draw(data, {
                            width: $(window).width(),
                            keepAspectRatio: true,
                            colorAxis: { colors: panel.attr('colors').split(';')}
                        });
                    })();

                    <?php
                }

            ?>
        }
    </script>
</body>
</html>
