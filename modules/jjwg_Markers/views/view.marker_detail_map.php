<?php

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
class Jjwg_MarkersViewMarker_Detail_Map extends SugarView
{
    public function __construct()
    {
        parent::__construct();
    }




    public function display()
    {
        $custom_markers_dir = 'themes/default/images/jjwg_Markers/'; ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
  <title><?php echo $GLOBALS['mod_strings']['LBL_MARKER_DISPLAY'] ?? ''; ?></title>
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

  <script type="text/javascript" src="//maps.googleapis.com/maps/api/js?key=<?= $GLOBALS['jjwg_config']['google_maps_api_key']; ?>&sensor=false"></script>

  <script type="text/javascript">

var geocoder = new google.maps.Geocoder();

function geocodePosition(pos) {
  geocoder.geocode({
    latLng: pos
  }, function(responses) {
    if (responses && responses.length > 0) {
      updateMarkerAddress(responses[0].formatted_address);
    } else {
      updateMarkerAddress('Cannot determine address at this location.');
    }
  });
}

function updateMarkerStatus(str) {
  document.getElementById('markerStatus').innerHTML = str;
}

function updateMarkerPosition(latLng) {
  document.getElementById('info').innerHTML = [
    latLng.lat(),
    latLng.lng()
  ].join(', ');
}

function updateMarkerAddress(str) {
  document.getElementById('address').innerHTML = str;
}

function initialize() {

  var latLng = new google.maps.LatLng(
    <?php echo (!empty($GLOBALS['loc']['lat'])) ? $GLOBALS['loc']['lat'] : $GLOBALS['jjwg_config']['map_default_center_latitude']; ?>,
    <?php echo (!empty($GLOBALS['loc']['lng'])) ? $GLOBALS['loc']['lng'] : $GLOBALS['jjwg_config']['map_default_center_longitude']; ?>
  );

  var map = new google.maps.Map(document.getElementById('mapCanvas'), {
    zoom: 4,
    center: latLng,
    mapTypeId: google.maps.MapTypeId.ROADMAP
  });

  var customImage = new google.maps.MarkerImage('<?php echo $custom_markers_dir; ?>/<?php echo javascript_escape($GLOBALS['loc']['image']); ?>.png',
    new google.maps.Size(32,37),
    new google.maps.Point(0,0),
    new google.maps.Point(16,37)
  );
  var shape = {coord: [1, 1, 1, 37, 32, 37, 32, 1],type: 'poly'};

  var marker = new google.maps.Marker({
    position: latLng,
    title: '<?php echo javascript_escape($GLOBALS['loc']['name']); ?>',
    map: map,
    icon: customImage,
    shape: shape,
    draggable: false
  });
  // Update current position info.
  updateMarkerPosition(latLng);
  geocodePosition(latLng);
}

// Onload handler to fire off the app.
google.maps.event.addDomListener(window, 'load', initialize);
</script>
</head>
<body>
  <div id="mapCanvas"></div>
  <div id="infoPanel"><b></b>
    <div id="markerStatus"><i></i></div>
    <b><?php echo $GLOBALS['mod_strings']['LBL_MARKER_MARKER_POSITION']; ?></b>
    <div id="info"></div>
    <b><?php echo $GLOBALS['mod_strings']['LBL_MARKER_CLOSEST_MATCHING_ADDRESS']; ?></b>
    <div id="address"></div>
  </div>
</body>
</html>

<?php
    }
}
