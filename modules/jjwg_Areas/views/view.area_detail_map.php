<?php

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
class Jjwg_AreasViewArea_Detail_Map extends SugarView
{
    public function __construct()
    {
        parent::__construct();
    }




    public function display()
    {
        ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
  <title><?php echo $GLOBALS['mod_strings']['LBL_AREA_MAP']; ?></title>
  <meta name="viewport" content="initial-scale=1.0, user-scalable=no" />
  <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
  <link rel="stylesheet" type="text/css" href="cache/themes/<?php echo $GLOBALS['theme']; ?>/css/style.css" />
  <style type="text/css">
    html { height: 100% }
    body { height: 100%; margin: 0px; padding: 0px }
    #mapCanvas {
      width: 700px;
      height: 500px;
      float: left;
    }
    #infoPanel {
      width: 450px;
      float: left;
      margin-left: 10px;
    }
    #mapCanvas, #infoPanel, #markerStatus, #info, #address {
      font-size: 12px;
      line-height: 16px;
      font-family:Arial,Verdana,Helvetica,sans-serif;
      color: #444444;
      margin-bottom: 5px;
    }
    b {
      font-weight: normal;
      color: #000000;
    }
  </style>

  <script type="text/javascript" src="//maps.googleapis.com/maps/api/js?key=<?= $GLOBALS['jjwg_config']['google_maps_api_key']; ?>&sensor=false&libraries=drawing"></script>

  <script type="text/javascript">

// Define Map Data for Javascript
var jjwg_config_defaults = <?php echo (!empty($GLOBALS['jjwg_config_defaults'])) ? json_encode($GLOBALS['jjwg_config_defaults']) : '[]'; ?>;
var jjwg_config = <?php echo (!empty($GLOBALS['jjwg_config'])) ? json_encode($GLOBALS['jjwg_config']) : '[]'; ?>;
var polygonPoints = <?php echo (!empty($GLOBALS['polygon'])) ? json_encode($GLOBALS['polygon']) : '[]'; ?>;

function initialize() {

    //create map
  var latLng = new google.maps.LatLng(
    <?php echo (!empty($GLOBALS['loc']['lat'])) ? $GLOBALS['loc']['lat'] : $GLOBALS['jjwg_config']['map_default_center_latitude']; ?>,
    <?php echo (!empty($GLOBALS['loc']['lng'])) ? $GLOBALS['loc']['lng'] : $GLOBALS['jjwg_config']['map_default_center_longitude']; ?>
  );

  var map = new google.maps.Map(document.getElementById('mapCanvas'), {
    zoom: 4,
    center: latLng,
    mapTypeId: google.maps.MapTypeId.ROADMAP
  });

  var bounds = new google.maps.LatLngBounds();


  // Define the Custom Area Polygons (jjwg_Areas Module)
  var polygon = [];
  var p = [];
  var myAreaPolygon = [];

  if (polygonPoints.length > 0) {

    // Define coordinates from objects
    myCoords = [];
    for (var j=0; j<polygonPoints.length; j++) {
        p = polygonPoints[j];
        myCoords[j] = new google.maps.LatLng(parseFloat(p.lat), parseFloat(p.lng)); // lat, lng
        bounds.extend(myCoords[j]);
    }
    myAreaPolygon[0] = new google.maps.Polygon({
        paths: myCoords,
        strokeColor: "#000099",
        strokeOpacity: 0.8,
        strokeWeight: 1,
        fillColor: "#000099",
        fillOpacity: 0.15,
        zIndex: 1
    });
    //console.log(myAreaPolygon[0]);

    myAreaPolygon[0].setMap(map);

    map.fitBounds(bounds);

  }

}

// Onload handler to fire off the app.
google.maps.event.addDomListener(window, 'load', initialize);
</script>
</head>
<body>
  <div id="mapCanvas"></div>
  <div id="infoPanel"><b></b>
    <div id="markerStatus"><i></i></div>
    <div id="info"></div>
    <div id="address"></div>
  </div>
</body>
</html>
<?php
    }
}
