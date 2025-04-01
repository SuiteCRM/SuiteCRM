<?php

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
class Jjwg_AreasViewArea_Edit_Map extends SugarView
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
    html,body{
      margin:0;
      padding:0;
      width:100%;
      height:100%;
      font-family:Arial, Helvetica, sans-serif;
    }
    #main-map{
      width: 700px;
      height: 500px;
      float: left;
    }
    #side{
      float: left;
      width: 450px;
      margin-left: 10px;
      font-size: 12px;
      line-height: 16px;
      font-family:Arial,Verdana,Helvetica,sans-serif;
      color: #444444;
      font-weight: normal;
    }

    #dataPanel{
      overflow:auto;
      border:1px solid #DDDDDD;
      font-size: 12px;
      line-height: 16px;
      font-family:Arial,Verdana,Helvetica,sans-serif;
      color: #444444;
      font-weight: normal;
    }
    input{
      font-size: 12px;
      line-height: 16px;
      font-family:Arial,Verdana,Helvetica,sans-serif;
      color: #444444;
      font-weight: normal;
    }

    input.navi{
      font-size: 12px;
      line-height: 16px;
      font-family:Arial,Verdana,Helvetica,sans-serif;
      color: #444444;
      font-weight: normal;
      margin-bottom:10px;
    }
    p{
      font-size: 12px;
      line-height: 16px;
      font-family:Arial,Verdana,Helvetica,sans-serif;
      color: #444444;
      padding-bottom: 10px;
    }
    b {
      font-weight: normal;
      color: #000000;
    }
    .message-box { text-align: center; padding: 5px; color:#545454; width:80%;  margin:5px auto; font-size:12px;}
    .clean { background-color: #efefef; border-top: 2px solid #dedede; border-bottom: 2px solid #dedede; }
    .info  { background-color: #f7fafd; border-top: 2px solid #b5d3ff; border-bottom: 2px solid #b5d3ff; }
    .ok    { background-color: #d7f7c4; border-top: 2px solid #82cb2f; border-bottom: 2px solid #82cb2f; }
    .alert { background-color: #fef5be; border-top: 2px solid #fdd425; border-bottom: 2px solid #fdd425; }
    .error { background-color: #ffcdd1; border-top: 2px solid #e10c0c; border-bottom: 2px solid #e10c0c; }
  </style>

  <script type="text/javascript" src="//maps.googleapis.com/maps/api/js?key=<?= $GLOBALS['jjwg_config']['google_maps_api_key']; ?>&sensor=false&libraries=drawing"></script>
  <script src="include/javascript/jquery/jquery-min.js"></script>

  <script type="text/javascript">

// Define Map Data for Javascript
var jjwg_config_defaults = <?php echo (!empty($GLOBALS['jjwg_config_defaults'])) ? json_encode($GLOBALS['jjwg_config_defaults']) : '[]'; ?>;
var jjwg_config = <?php echo (!empty($GLOBALS['jjwg_config'])) ? json_encode($GLOBALS['jjwg_config']) : '[]'; ?>;
var polygonPoints = <?php echo (!empty($GLOBALS['polygon'])) ? json_encode($GLOBALS['polygon']) : '[]'; ?>;

$(function(){

    //create map
    var latLng = new google.maps.LatLng(
        <?php echo (!empty($GLOBALS['loc']['lat'])) ? $GLOBALS['loc']['lat'] : $GLOBALS['jjwg_config']['map_default_center_latitude']; ?>,
        <?php echo (!empty($GLOBALS['loc']['lng'])) ? $GLOBALS['loc']['lng'] : $GLOBALS['jjwg_config']['map_default_center_longitude']; ?>
    );

    var myOptions = {
        zoom: 4,
        center: latLng,
        mapTypeId: google.maps.MapTypeId.ROADMAP
    }
    map = new google.maps.Map(document.getElementById('main-map'), myOptions);

    var bounds = new google.maps.LatLngBounds();

    // Drawing Controls
    var drawingManager = new google.maps.drawing.DrawingManager({
        drawingMode: google.maps.drawing.OverlayType.POLYGON,
        drawingControl: true,
        drawingControlOptions: {
            position: google.maps.ControlPosition.TOP_CENTER,
            drawingModes: [google.maps.drawing.OverlayType.POLYGON]
        },
        polygonOptions: {
            strokeColor: "#000099",
            strokeOpacity: 0.8,
            strokeWeight: 1,
            fillColor: "#000099",
            fillOpacity: 0.20,
            clickable: false,
            editable: true,
            zIndex: 1
        }
    });
    drawingManager.setMap(map);


    // Define the Area Polygon
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
            editable: true,
            strokeColor: "#000099",
            strokeOpacity: 0.8,
            strokeWeight: 1,
            fillColor: "#000099",
            fillOpacity: 0.15,
            zIndex: 1
        });
        //console.log(myAreaPolygon[0]);

        // Set to global array for later reference and destroy
        myAreaPolygon[0].setMap(map);


        // Right click to remove vertex
        myAreaPolygon[0].addListener('rightclick', function(mev){
            if (mev.vertex != null && this.getPath().getLength() > 2) {
                this.getPath().removeAt(mev.vertex);
            }
        });


        map.fitBounds(bounds);

    }


    // Event listener on add new polygon
    google.maps.event.addListener(drawingManager, 'polygoncomplete', function(polygon) {
        // Push to reference array
        myAreaPolygon.push(polygon);
    });


    // Reset polygon(s)
    $('#reset').click(function(){
        // Destroy existing polygons on map
        for (var i=0; i<myAreaPolygon.length; i++) {
            myAreaPolygon[i].setMap(null);
        }
        // Destroy myAreaPolygon array of polygon objects
        myAreaPolygon = [];
    });


    // Define polygon(s) coordinates as lng,lat,elv string and set to 'coordinates' field
    $('#showData').click(function() {

        $('#dataPanel').empty();
        var myCoords = Array();
        var myDataString = '';

        for (var i=0; i<myAreaPolygon.length; i++) {
            var polygon = myAreaPolygon[i];
            if (polygon != '') {
                myCoords = polygon.getPath().getArray();
                if (myCoords.length > 1) {
                    for (var j=0; j<myCoords.length; j++) {
                        var myCoord = myCoords[j];
                        // Return format: lng,lat,elv
                        // Reduce percision to 8 after decimal and trim zeros
                        var lng = myCoord.lng().toFixed(8).replace(/0+$/g, "");
                        var lat = myCoord.lat().toFixed(8).replace(/0+$/g, "");
                        myDataString += lng + ',' + lat + ',0 ';
                    }
                    myDataString = myDataString.replace(/^\s+|\s+$/g,"");
                    myDataString += "\n\n";
                }
            }
        }

        // Update Coordinates display
        myDataString = myDataString.replace(/^[\s\n\r]+|[\s\n\r]+$/g,"");
        $('#dataPanel').append(myDataString.replace(/[\n\r]/g,"<br />"));
        // Update parent form 'coordinates' field
        parent.document.getElementById('coordinates').value = myDataString;

    });


});

</script>
</head>
<body>
  <div id="main-map">
  </div>
  <div id="side">
    <b><?php echo $GLOBALS['mod_strings']['LBL_AREA_EDIT_TITLE']; ?></b><br />
        <?php echo $GLOBALS['mod_strings']['LBL_AREA_EDIT_DESC_1']; ?><br />
        <?php echo $GLOBALS['mod_strings']['LBL_AREA_EDIT_DESC_2']; ?><br />
    <input id="reset" value="<?php echo $GLOBALS['mod_strings']['LBL_AREA_EDIT_RESET']; ?>" type="button" class="navi"/>
    <input id="showData" value="<?php echo $GLOBALS['mod_strings']['LBL_AREA_EDIT_USE_AREA_COORDINATES']; ?>" type="button" class="navi"/>
    <br />
        <?php echo $GLOBALS['mod_strings']['LBL_AREA_EDIT_COORDINATE_RESULTS']; ?><br />
    <div id="dataPanel">
    </div>
  </div>
</body>
</html>
<?php
    }
}
?>
