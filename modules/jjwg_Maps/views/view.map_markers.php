<?php

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
class Jjwg_MapsViewMap_Markers extends SugarView
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
  <title><?php echo $GLOBALS['mod_strings']['LBL_MAP_DISPLAY']; ?></title>
  <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
  <link rel="stylesheet" type="text/css" href="cache/themes/<?php echo $GLOBALS['theme']; ?>/css/style.css" />
<?php if (!empty($GLOBALS['jjwg_config']['google_maps_api_key'])): ?>
  <style type="text/css">
    html,body{
      margin:0;
      padding:0;
      width:100%;
      font-family:Arial, Helvetica, sans-serif;
    }
    #map_canvas {
      width: 100%;
      height: 500px;
      margin:0;
      padding:0;
      border: 0;
    }
    div.marker {
      font-size: 12px;
      font-family:Arial,Verdana,Helvetica,sans-serif;
      overflow: hidden;
    }
    #legend {
      background: rgba(100%, 100%, 100%, 0.60);
      padding: 5px;
      margin: 5px;
      border: 1px solid #999999;
      width: 140px;
      min-width: 140px;
      overflow-x: auto;
      max-height: 440px;
      overflow-y: auto;
      white-space: nowrap;
      font-size: 12px;
      line-height: 16px;
      font-family:Arial,Verdana,Helvetica,sans-serif;
      color: #333333;
    }
    #legend b {
      font-weight: bold;
      font-family:Arial,Verdana,Helvetica,sans-serif;
      color: #333333;
    }
    #legend img {
      vertical-align: middle;
      margin: 1px;
      border: none;
    }

    b {
      font-size: 12px;
      line-height: 16px;
      font-weight: bold;
      color: #000000;
    }
    div.dataTables_filter {
      margin-right: 10px;
    }
  </style>
  <link rel="stylesheet" type="text/css" href="//cdnjs.cloudflare.com/ajax/libs/datatables/1.9.4/css/jquery.dataTables.min.css" />
  <link rel="stylesheet" type="text/css" href="//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/css/TableTools.min.css" />
  <script type="text/javascript" src="//maps.googleapis.com/maps/api/js?key=<?= $GLOBALS['jjwg_config']['google_maps_api_key']; ?>&sensor=false&libraries=drawing,geometry"></script>
  <script src="include/javascript/jquery/jquery-min.js"></script>
  <script type="text/javascript" src="modules/jjwg_Maps/javascript/jquery.iframe-auto-height.plugin.1.9.3.min.js"></script>
  <script type="text/javascript" src="modules/jjwg_Maps/javascript/markerclusterer_packed.js"></script>
  <script type="text/javascript" src="//cdnjs.cloudflare.com/ajax/libs/datatables/1.9.4/jquery.dataTables.min.js"></script>
  <script type="text/javascript" src="//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/js/TableTools.min.js"></script>
  <script type="text/javascript">
// Define SugarCRM App data for Javascript
var app_strings = <?php echo (!empty($GLOBALS['app_strings'])) ? json_encode($GLOBALS['app_strings']) : '[]'; ?>;
var app_list_strings = <?php echo (!empty($GLOBALS['app_list_strings'])) ? json_encode($GLOBALS['app_list_strings']) : '[]'; ?>;
var mod_strings = <?php echo (!empty($GLOBALS['mod_strings'])) ? json_encode($GLOBALS['mod_strings']) : '[]'; ?>;
// Define Map Data for Javascript
var jjwg_config_defaults = <?php echo (!empty($GLOBALS['jjwg_config_defaults'])) ? json_encode($GLOBALS['jjwg_config_defaults']) : '[]'; ?>;
var jjwg_config = <?php echo (!empty($GLOBALS['jjwg_config'])) ? json_encode($GLOBALS['jjwg_config']) : '[]'; ?>;
var list_array = <?php echo (!empty($this->bean->list_array)) ? json_encode($this->bean->list_array) : '[]'; ?>;
<?php
// Check to see if map center is empty of lng,lat of 0,0
if (empty($this->bean->map_center) || (empty($this->bean->map_center['lat']) && empty($this->bean->map_center['lng']))) {
    // Ensure something shows on the map
    if (empty($this->bean->map_markers) && empty($this->bean->custom_markers) && empty($this->bean->custom_areas)) {
        // Define default point as map center
        $this->bean->map_center['lat'] = $GLOBALS['jjwg_config']['map_default_center_latitude'];
        $this->bean->map_center['lng'] = $GLOBALS['jjwg_config']['map_default_center_longitude'];
        if (!isset($this->bean->map_center['html'])) {
            $this->bean->map_center['html'] = $GLOBALS['mod_strings']['LBL_DEFAULT'];
        }
        if (!isset($this->bean->map_center['name'])) {
            $this->bean->map_center['name'] = $GLOBALS['mod_strings']['LBL_DEFAULT'];
        }
    }
} ?>
var map_center = <?php echo (!empty($this->bean->map_center)) ? json_encode($this->bean->map_center) : 'null'; ?>;
var map_markers = <?php echo (!empty($this->bean->map_markers)) ? json_encode($this->bean->map_markers) : '[]'; ?>;
var map_markers_groups = <?php echo (!empty($this->bean->map_markers_groups)) ? json_encode($this->bean->map_markers_groups) : '[]'; ?>;
var custom_markers = <?php echo (!empty($this->bean->custom_markers)) ? json_encode($this->bean->custom_markers) : '[]'; ?>;
var custom_areas = <?php echo (!empty($this->bean->custom_areas)) ? json_encode($this->bean->custom_areas) : '[]'; ?>;
<?php
    // Define Map Data
    $num_markers = is_countable($this->bean->map_markers) ? count($this->bean->map_markers) : 0;
        $num_groups = is_countable($this->bean->map_markers_groups) ? count($this->bean->map_markers_groups) : 0;
        if ($num_groups > 216) {
            $num_groups = 216;
        }
        $group_name_to_num = array();
        $i = 1;
        // Define Group Name to Icon Number Mapping 1-216(max)
        if (!empty($this->bean->map_markers_groups)) {
            foreach ($this->bean->map_markers_groups as $name) {
                $group_name_to_num[$name] = $i;
                $i++;
            }
        }
        // Define Dir of Group Icons
        $icons_dir_base = 'themes/default/images/jjwg_Maps/';
        if ($num_groups <= 10) {
            $icons_dir = $icons_dir_base.'0-10/';
        } elseif ($num_groups <= 25) {
            $icons_dir = $icons_dir_base.'0-25/';
        } elseif ($num_groups <= 100) {
            $icons_dir = $icons_dir_base.'0-100/';
        } elseif ($num_groups <= 216) {
            $icons_dir = $icons_dir_base.'0-216/';
        } else {
            $icons_dir = $icons_dir_base.'0-10/'; // Demo Version
        }

        // Define Custom Markers Dir and Common Icons
        $custom_markers_dir = 'custom/themes/default/images/jjwg_Markers/';
        $custom_markers_icons = array();
        foreach ($this->bean->custom_markers as $marker) {
            $custom_markers_icons[] = $marker['image'];
        }
        $num_custom_markers = is_countable($this->bean->custom_markers) ? count($this->bean->custom_markers) : 0;
        $custom_markers_icons = array_unique($custom_markers_icons); ?>

// Define Map Data for Javascript
var num_markers = <?php echo (!empty($num_markers)) ? json_encode($num_markers) : '0'; ?>;
var num_groups = <?php echo (!empty($num_groups)) ? json_encode($num_groups) : '0'; ?>;
var group_name_to_num = <?php echo (!empty($group_name_to_num)) ? json_encode($group_name_to_num) : '[]'; ?>;
var icons_dir = <?php echo (!empty($icons_dir)) ? json_encode($icons_dir) : "'themes/default/images/jjwg_Maps/0-10/'"; ?>;
var num_custom_markers = <?php echo (!empty($num_custom_markers)) ? json_encode($num_custom_markers) : '0'; ?>;
var custom_markers_dir = <?php echo (!empty($custom_markers_dir)) ? json_encode($custom_markers_dir) : "'custom/themes/default/images/jjwg_Markers/'"; ?>;
var custom_markers_icons = <?php echo (!empty($custom_markers_icons)) ? json_encode($custom_markers_icons) : '[]'; ?>;

/******************************************************************************/


// Define map vars
var map = null;
var bounds = null;
var loc = [];
var myLatLng = [];

// MarkerImage objects
var markerImage = [];
var shape = null;

// Marker objects
var marker = [];
var markerGroupVisible = [];

// Legend and Clusterer Control
var legend = null;
var markerClusterer = null;
var markerClustererToggle = null;
var clusterControlDiv = null;
// Clusterer Images - Protocol Independent
MarkerClusterer.IMAGE_PATH = "//raw.githubusercontent.com/googlemaps/js-marker-clusterer/gh-pages/images/m";

// Drawing Controls
var drawingManager = null;
var selectedShape = null;
var selectedShapeMarkerById = null;

// InfoWindow objects: array of InfoWindow objects used for all markers, custom markers and custom areas
var infowindow = [];

// All types of Marker objects
var markers = [];

// Areas/Polygons objects
var myAreaPolygon = null;

// DataTable
var oDataTable = null;
var oDataTableShown = null;
var oDataTableShownIds = null;


function setCenterMarker() {

    // Center Marker - marker[0]
    if (map_center !== null) {
        loc[0] = map_center;
        myLatLng[0] = new google.maps.LatLng(loc[0].lat, loc[0].lng);
        marker[0] = new google.maps.Marker({
            position: myLatLng[0],
            map: map,
            icon: markerImage[0],
            shape: shape,
            title: loc[0].name,
            group_name: '',
            group_num: '',
            infoHtml: loc[0].html,
            zIndex: 99
        });
        //console.log(0);
        google.maps.event.addListener(marker[0], 'click', function() {
            infowindow[0] = new google.maps.InfoWindow();
            infowindow[0].open(map, this);
            infowindow[0].setContent(this.infoHtml);
        });
        bounds.extend(myLatLng[0]);
    }
}


function setMarkers() {

    // Markers and InfoWindows
    for (var i=1, mLen=map_markers.length; i<=mLen; i++) {
        loc[i] = map_markers[i-1];
        if (loc[i].group == '') loc[i].group = map_markers_groups[0];
        myLatLng[i] = new google.maps.LatLng(loc[i].lat, loc[i].lng);
        marker[i] = new google.maps.Marker({
            position: myLatLng[i],
            map: map,
            icon: markerImage[group_name_to_num[loc[i].group]],
            shape: shape,
            title: loc[i].name,
            id: loc[i].id,
            group_name: loc[i].group,
            group_num: group_name_to_num[loc[i].group],
            infoHtml: loc[i].html,
            infoI: i,
            zIndex: 5
        });
        //console.log(marker[i].infoI);
        google.maps.event.addListener(marker[i], 'click', function() {
            if (typeof infowindow[this.infoI] != 'object') {
                infowindow[this.infoI] = new google.maps.InfoWindow();
            }
            infowindow[this.infoI].open(map, this);
            infowindow[this.infoI].setContent(this.infoHtml);
        });
        //console.log(marker[i]);
        bounds.extend(myLatLng[i]);
        markers.push(marker[i]);
    } // end for

}


function toggleMarkerGroupVisibility(group_num) {

    // Check markerGroupVisible
    visibility = markerGroupVisible[group_num];

    if (typeof group_num !== 'undefined' && group_num !== '') {
        // Markers
        var toggled = false;
        for (var i=0, mLen=marker.length; i<mLen; i++) {
            if (typeof marker[i] === 'object') {
                if (marker[i].group_num == group_num) {
                    // Change Marker Visibility
                    marker[i].setVisible(!visibility);
                    toggled = true;
                }
            }
        }
        if (toggled === true) {
            markerGroupVisible[group_num] = !visibility;
            markerClusterer.repaint();
        }
    }

    return markerGroupVisible[group_num];

}


function clickMarkerById(id) {

    for (var i=0, mLen=marker.length; i<mLen; i++) {
        if (typeof marker[i] === 'object') {
            if (marker[i].id == id) {
                map.panTo(marker[i].position);
                google.maps.event.trigger(marker[i], "click");
            }
        }
    }
    return false;
}


function panToMarkerById(id) {

    for (var i=0, mLen=marker.length; i<mLen; i++) {
        if (typeof marker[i] === 'object') {
            if (marker[i].id == id) {
                var lat = marker[i].position.ob;
                var lng = marker[i].position.pb;
                moveToLocation(lat, lng)
            }
        }
    }
}

function panToLocation(lat, lng){

    var center = new google.maps.LatLng(lat, lng);
    map.panTo(center);
}


function setClusterControl() {

    // Controls for Clusters
    clusterControlDiv = document.createElement('div');
    // Set CSS styles for the DIV containing the control
    // Setting padding to 5 px will offset the control
    // from the edge of the map
    clusterControlDiv.style.padding = '6px';

    // Set CSS for the control border
    var controlUI = document.createElement('div');
    controlUI.style.backgroundColor = '#ffffff';
    controlUI.style.borderStyle = 'solid';
    controlUI.style.borderColor = '#a9a9a9';
    controlUI.style.borderWidth = '1px';
    controlUI.style.cursor = 'pointer';
    controlUI.style.textAlign = 'center';
    controlUI.title = 'Click to Toggle Clustering';
    clusterControlDiv.appendChild(controlUI);

    // Set CSS for the control interior
    var controlText = document.createElement('div');
    controlText.style.fontFamily = 'Arial,Verdana,Helvetica,sans-serif';
    controlText.style.fontSize = '12px';
    controlText.style.paddingLeft = '4px';
    controlText.style.paddingRight = '4px';
    controlText.style.paddingTop = '1px';
    controlText.style.paddingBottom = '1px';
    controlText.innerHTML = 'Toggle Clustering';
    controlUI.appendChild(controlText);

    clusterControlDiv.index = 1;
    map.controls[google.maps.ControlPosition.TOP_RIGHT].push(clusterControlDiv);

    // http://google-maps-utility-library-v3.googlecode.com/svn/tags/markerclustererplus/2.1.1/examples/advanced_example.html?
    google.maps.event.addDomListener(controlUI, 'click', function() {
        if (markerClustererToggle !== true) {
            markerClusterer.setOptions({map:map});//restores the clusterIcons
            markerClustererToggle = true;
        } else {
            markerClusterer.setOptions({map:null});//hides the clusterIcons
            markerClustererToggle = false;
        }
        markerClusterer.repaint();
    });

}


function setDrawingControls() {

    // Drawing Controls
    var overlayOptions = { strokeColor: "#000099", strokeOpacity: 0.8, strokeWeight: 1, fillColor: "#000099", fillOpacity: 0.20, clickable: true, draggable: true, editable: true, zIndex: 5 };
    drawingManager = new google.maps.drawing.DrawingManager({
        drawingMode: null,
        drawingControl: true,
        drawingControlOptions: {
            position: google.maps.ControlPosition.TOP_CENTER,
            drawingModes: [
                google.maps.drawing.OverlayType.RECTANGLE,
                google.maps.drawing.OverlayType.CIRCLE,
                google.maps.drawing.OverlayType.POLYGON
            ]
        },
        rectangleOptions: overlayOptions,
        circleOptions: overlayOptions,
        polygonOptions: overlayOptions
    });
    drawingManager.setMap(map);

    google.maps.event.addListener(drawingManager, 'overlaycomplete', function(e) {
        // Switch back to non-drawing mode after drawing a shape
        drawingManager.setDrawingMode(null);
        // Add an event listeners
        var newShape = e.overlay;
        newShape.type = e.type;
        google.maps.event.addListener(newShape, 'click', function() {
            setSelection(newShape);
        });
        google.maps.event.addListener(newShape, 'dragend', function() {
            setSelection(newShape);
        });
        if (newShape.type == 'polygon') {
            // Right click to remove vertex
            google.maps.event.addListener(newShape, 'rightclick', function(mev){
                if (mev.vertex != null && this.getPath().getLength() > 2) {
                    this.getPath().removeAt(mev.vertex);
                }
            });
        }
        setSelection(newShape);
    });

    // Clear the current selection when the drawing mode is changed, or when the map is clicked.
    google.maps.event.addListener(drawingManager, 'drawingmode_changed', clearSelection);
    google.maps.event.addListener(map, 'click', clearSelection);


    // "Delete Selected" Shape Button
    deleteSelectedDiv = document.createElement('div');
    // Set CSS styles for the DIV containing the control
    // Setting padding to 5 px will offset the control
    // from the edge of the map
    deleteSelectedDiv.style.padding = '6px';

    // Set CSS for the control border
    var controlUI = document.createElement('div');
    controlUI.style.backgroundColor = '#ffffff';
    controlUI.style.borderStyle = 'solid';
    controlUI.style.borderColor = '#a9a9a9';
    controlUI.style.borderWidth = '1px';
    controlUI.style.cursor = 'pointer';
    controlUI.style.textAlign = 'center';
    controlUI.title = 'Click to Remove Selected Shape';
    deleteSelectedDiv.appendChild(controlUI);

    // Set CSS for the control interior
    var controlText = document.createElement('div');
    controlText.style.fontFamily = 'Arial,Verdana,Helvetica,sans-serif';
    controlText.style.fontSize = '12px';
    controlText.style.paddingLeft = '4px';
    controlText.style.paddingRight = '4px';
    controlText.style.paddingTop = '4px';
    controlText.style.paddingBottom = '4px';
    controlText.innerHTML = 'Remove';
    controlUI.appendChild(controlText);

    deleteSelectedDiv.index = 1;
    map.controls[google.maps.ControlPosition.TOP_CENTER].push(deleteSelectedDiv);

    // Clear the current selection when the remove shape button is clicked
    google.maps.event.addDomListener(deleteSelectedDiv, 'click', deleteSelectedShape);

}

function listSelectedShape() {
    // Check for selectedShape
    //console.log(selectedShape);
    selectedShapeMarkerById = [];
    if (typeof selectedShape === 'object') {
        // Loop thru mMarker objects
        for (var i=0, mLen=marker.length; i<mLen; i++) {
            if (typeof marker[i] === 'object') {
                selectedShapeMarkerById[marker[i].id] = false;
                // Contains/Bounds Checks
                if (selectedShape.type == 'polygon') {
                    if (google.maps.geometry.poly.containsLocation(marker[i].position, selectedShape)) {
                        selectedShapeMarkerById[marker[i].id] = true;
                    }
                } else if (selectedShape.type == 'rectangle' || selectedShape.type == 'circle') {
                    var shapeBounds = selectedShape.getBounds();
                    if (shapeBounds.contains(marker[i].position)) {
                        selectedShapeMarkerById[marker[i].id] = true;
                    }
                }
            }
        }
        //console.log(selectedShapeMarkerById);
        // Redraw oDataTable
        oDataTable.fnDraw();
    }
}

function clearSelection() {
    if (selectedShape) {
        selectedShape.setEditable(false);
        selectedShape = null;
        selectedShapeMarkerById = null;
        // Redraw oDataTable
        oDataTable.fnDraw();
    }
}

function setSelection(shape, editable) {
    editable = (typeof editable !== 'undefined') ? editable : true;
    clearSelection();
    selectedShape = shape;
    if (editable === true) {
        shape.setEditable(true);
    }
    listSelectedShape();
}

function deleteSelectedShape() {
    if (selectedShape) {
        selectedShape.setMap(null);
        selectedShape = null;
        selectedShapeMarkerById = null;
    }
    // Redraw oDataTable
    oDataTable.fnDraw();
}


function setCustomMarkers() {

    // Define the Custom Marker Images (jjwg_Markers Module)
    var customImage = [];
    for (var i=0, cLen=custom_markers_icons.length; i<cLen; i++) {
        image = custom_markers_icons[i];
        customImage[image] = new google.maps.MarkerImage(custom_markers_dir+image+'.png',
            new google.maps.Size(32,37),
            new google.maps.Point(0,0),
            new google.maps.Point(16,37)
        );
    }
    var custom_shape = {coord: [1, 1, 1, 37, 32, 37, 32, 1],type: 'poly'};

    for (var i=num_markers+1, numTot=num_markers+num_custom_markers; i<=numTot; i++) {

        loc[i] = custom_markers[i-num_markers-1];
        myLatLng[i] = new google.maps.LatLng(loc[i].lat, loc[i].lng);
        marker[i] = new google.maps.Marker({
            position: myLatLng[i],
            map: map,
            icon: customImage[loc[i].image],
            shape: custom_shape,
            title: loc[i].name,
            infoHtml: loc[i].html,
            infoI: i,
            zIndex: 25
        });
        //console.log(marker[i].infoI);
        google.maps.event.addListener(marker[i], 'click', function() {
            if (typeof infowindow[this.infoI] != 'object') {
                infowindow[this.infoI] = new google.maps.InfoWindow();
            }
            infowindow[this.infoI].open(map, this);
            infowindow[this.infoI].setContent(this.infoHtml);
        });
        bounds.extend(myLatLng[i]);
        markers.push(marker[i]);

    } // end for

}

function setCustomAreas() {

  // Define the Custom Area Polygons (jjwg_Areas Module)
    var polygon = [];
    var p = [];
    myAreaPolygon = [];

    for (var i=0, cLen=custom_areas.length; i<cLen; i++) {

        // coordinates: space separated lng,lat,elv points
        myCoords = [];
        polygon = custom_areas[i].coordinates.replace(/^[\s\n\r]+|[\s\n\r]+$/g,"").split(/[\n\r ]+/);
        for (var j=0, pLen=polygon.length; j<pLen; j++) {
            p = polygon[j].split(",");
            myCoords[j] = new google.maps.LatLng(parseFloat(p[1]), parseFloat(p[0])); // lat, lng
            bounds.extend(myCoords[j]);
        }
        myAreaPolygon[i] = new google.maps.Polygon({
            paths: myCoords,
            strokeColor: "#000099",
            type: "polygon",
            strokeOpacity: 0.8,
            strokeWeight: 1,
            fillColor: "#000099",
            fillOpacity: 0.15,
            title: custom_areas[i].name,
            infoHtml: custom_areas[i].html,
            infoI: i+num_markers+num_custom_markers+1, // inc with markers counts
            zIndex: 1
        });
        //console.log(myAreaPolygon[i].infoI);
        myAreaPolygon[i].setMap(map);
        google.maps.event.addListener(myAreaPolygon[i], 'click', function(event) {
            if (typeof infowindow[this.infoI] != 'object') {
                infowindow[this.infoI] = new google.maps.InfoWindow();
            }
            infowindow[this.infoI].setContent(this.infoHtml);
            infowindow[this.infoI].setPosition(event.latLng);
            infowindow[this.infoI].open(map);
            // List Selection and InfoWindow closeclick
            setSelection(this, false);
            google.maps.event.addListener(infowindow[this.infoI], 'closeclick', function(event) {
                clearSelection();
            });
        });

    }

}

function initialize() {

    map = new google.maps.Map(document.getElementById("map_canvas"), {
        zoom: 4,
        center: new google.maps.LatLng(
            jjwg_config['map_default_center_latitude'], jjwg_config['map_default_center_longitude']
        ),
        mapTypeId: google.maps.MapTypeId.ROADMAP
    });

    bounds = new google.maps.LatLngBounds();

    // Define the Marker Images
    for (var i=0, gLen=map_markers_groups.length; i<=gLen; i++) {
        markerImage[i] = new google.maps.MarkerImage(icons_dir+'/marker_'+i+'.png',
        new google.maps.Size(20,34), new google.maps.Point(0,0), new google.maps.Point(10,34));
        // Set initial visibility toggle to true for legend groups
        markerGroupVisible[group_name_to_num[map_markers_groups[i]]] = true;
        //markerGroupVisible[i] = true;
    }
    shape = {coord: [1, 1, 1, 34, 20, 34, 20, 1],type: 'poly'};

    setCenterMarker();
    setMarkers();
    setCustomMarkers();
    setCustomAreas();

    // Position Legend
    legend = document.getElementById('legend');
    map.controls[google.maps.ControlPosition.RIGHT_BOTTOM].push(legend);

    // Set a maximum zoom Level only on initial zoom
    map.fitBounds(bounds);
    google.maps.event.addListenerOnce(map, "idle", function() {
        if (map.getZoom() > 15) map.setZoom(15);
    });

    // Now using MarkerClustererPlus v2.1.1
    markerClusterer = new MarkerClusterer(map, markers, {
        maxZoom: (typeof jjwg_config['map_clusterer_max_zoom'] === 'undefined') ? jjwg_config_defaults['map_clusterer_max_zoom'] : jjwg_config['map_clusterer_max_zoom'],
        gridSize: (typeof jjwg_config['map_clusterer_grid_size'] === 'undefined') ? jjwg_config_defaults['map_clusterer_max_zoom'] : jjwg_config['map_clusterer_grid_size'],
        ignoreHidden: true
    });
    markerClustererToggle = true;
    setClusterControl();

    // Drawing Select to List Controls
    setDrawingControls();
}



// Define DataTable Data
function setODataTable() {

    if (num_markers > 0) {

        //console.log(map_markers);

        oDataTable = $('#displayDataTable').dataTable({
            "bPaginate": true,
            "bFilter": true,
            "bStateSave": true,
            "bProcessing": true,
            "sDom": '<Tlfrtip>',
            "oTableTools": {
                //Define the buttons beneath the Map
                "aButtons": [
                    {
                        //Copy Column Data.
                        "sExtends": "copy",
                        "sButtonText": "Copy",
                        "mColumns": "all"

                    },
                    {
                        //Export columns to CSV
                        "sExtends": "csv",
                        "sButtonText": "CSV",
                        "mColumns": "all"
                    },
                    {
                        //Export Columns to XLS
                        "sExtends": "xls",
                        "sButtonText": "XLS",
                        "mColumns": "all"
                    },
                    {
                        //Export Visible columns to PDF
                        "sExtends": "pdf",
                        "sButtonText": "PDF",
                        "mColumns": [1,2,3,4,5,6],
                    },
                    {
                        //Move to "Print" page.
                        "sExtends": "print",
                        "sButtonText": "Print",
                        "mColumns": "all"
                    },
                ],
                "sSwfPath": "//cdn.datatables.net/tabletools/2.2.2/swf/copy_csv_xls_pdf.swf"
            },
            "fnDrawCallback": function(oSettings) {
                if (typeof window.parent.resizeDataTables == 'function') {
                    window.parent.resizeDataTables();
                }
                setODataTableShown();
            },
            "oLanguage": { "sUrl": "modules/jjwg_Maps/DataTables/media/language/<?php echo $GLOBALS['current_language']; ?>.lang.js" },
            "aaData": map_markers,
            "aoColumns": [
                {
                    "sWidth": "1%",
                    "mDataProp": "id",
                    "bSearchable": false
                },
                {
                    "sWidth": "25%",
                    "mDataProp": "name",
                    "mRender": function (data, type, row) {
                        if (type == 'display') {
                            return '<a target="_blank" href="./index.php?module=' + row.module +
                                '&amp;action=DetailView&amp;record=' + row.id +
                                '" class="link target_blank">' + data + '</a>';
                        } else {
                            return data;
                        }
                    }
                },
                {
                    "sWidth": "35%",
                    "mDataProp": "address"
                },
                {
                    "sWidth": "10%",
                    "mDataProp": "phone"
                },
                {
                    "sWidth": "10%",
                    "mDataProp": "group",
                    "mRender": function (data, type, row) {
                        if (data !== null && data !== '') {
                            return data;
                        } else {
                            return "{"+mod_strings['LBL_MAP_NULL_GROUP_NAME']+"}";
                        }
                    }
                },
                {
                    "sWidth": "10%",
                    "mDataProp": "assigned_user_name"
                },
                {
                    "sWidth": "7%",
                    "mDataProp": "module",
                    "mRender": function (data, type, row) {
                        if (app_list_strings['moduleListSingular'][data] !== '') {
                            return app_list_strings['moduleListSingular'][data];
                        } else {
                            return data;
                        }
                    }
                },
                {
                    "sWidth": "2%",
                    "mDataProp": "image",
                    "bSearchable": false,
                    "mRender": function (data, type, row) {
                        if (type == 'display') {
                            return '<a href="#" onclick="clickMarkerById(\'' + row.id + '\')" class="link">' +
                                '<img src="themes/default/images/jjwg_Address_Cache.gif" /></a>';
                        } else {
                            return '';
                        }
                    }
                }
            ]
        });
        // Force Visibility of ID Column
        oDataTable.fnSetColumnVis(0, false);

        // Filter by Legend Toggle
        $.fn.dataTableExt.afnFiltering.push(
            function( oSettings, aData, iDataIndex ) {

                var shown = true;
                // Check Shape Selection: Limit by selectedShape and selectedShapeMarkerById
                if (selectedShapeMarkerById) {
                    if (typeof selectedShape === 'object') {
                        // Note: 'id' is hidden from aData
                        var rowId = oSettings.aoData[iDataIndex]._aData.id;
                        if (selectedShapeMarkerById[rowId] !== true) {
                            shown = false;
                        }
                    }
                }

                // Check the marker group visibility
                var group_name = aData[3];
                if (group_name == "{"+mod_strings['LBL_MAP_NULL_GROUP_NAME']+"}") {
                    var group_num = 1;
                } else {
                    var group_num = group_name_to_num[group_name];
                }
                if (markerGroupVisible[group_num] !== true) {
                    shown = false;
                }

                return shown;
            }
        );

    }
}

function setODataTableShown() {

    // Set the shown oDataTable based on the above filtering
    // Triggered by fnDrawCallback
    oDataTableShown = oDataTable._('tr', {"filter":"applied"});
    //console.log(oDataTableShown);
    oDataTableShownIds = [];
    for (var i=0, mLen=oDataTableShown.length; i<mLen; i++) {
        var rowData = oDataTableShown[i];
        oDataTableShownIds.push(rowData.id);
    }
    //console.log(oDataTableShownIds);
}


$(document).ready(function(){

    // Call Google Map initialize()
    initialize();

    // Set DataTables Data
    setODataTable();

    // Tabs Loading Bug - Hidden IFrame Map
    $('div.detailview_tabs ul li a', parent.document).on('click', function (){
        // Allow 1/4 sec for Tabs JavaScript to execute
        setTimeout(function(){
            // Reset Bounds, Max Zoom and Repaint Marker Clustering
            map.fitBounds(bounds);
            if (map.getZoom() > 15) map.setZoom(15);
            markerClusterer.repaint();
        },250);
    });

    // Toogle Marker Group Visibility on Click of Image
    $('#legend img').click(function(){
        var rel_group_num = $(this).attr('rel');
        visibile_result = toggleMarkerGroupVisibility(rel_group_num);
        if (!visibile_result) {
            $(this).css({ opacity: 0.55 });
        } else {
            $(this).css({ opacity: 1.0 });
        }
        // Redraw DataTable
        if (num_markers > 0) {
            oDataTable.fnDraw();
        }

    });

    // Target List Form Submit
    $('#tagetList').on("submit", function(event) {

        event.preventDefault();

        if (confirm('<?php echo $GLOBALS['mod_strings']['LBL_ADD_TO_TARGET_LIST']; ?>')) {

            $('#tagetListResult').html('<?php echo $GLOBALS['mod_strings']['LBL_ADD_TO_TARGET_LIST_PROCESSING']; ?>');
            var formData = $(this).serializeArray();
            var formUrl = $(this).attr("action");
            // Add oDataTableShownIds
            if (oDataTableShownIds !== null) {
                for (var i=0, mLen=oDataTableShownIds.length; i<mLen; i++) {
                    var valId = oDataTableShownIds[i];
                    formData.push({ name: "selected_ids[]", value: valId });
                }
            }

            $.ajax({
                url: formUrl,
                type: "post",
                data: formData
            }).done(function(response) {
                //console.log(response);
                $('#tagetListResult').html(response.message +
                    ' (' + response.list.name + ')');
            }).fail(function(jqXHR, textStatus, errorThrown) {
                response = jQuery.parseJSON(jqXHR.responseText);
                //console.log(response);
                $('#tagetListResult').html('#' + errorThrown + ' ' +
                    textStatus + '. ' + response.message +
                    ' (' + response.list.name + ')');
            });
        }
    });

});


</script>
<?php endif ?>

</head>

<body>
<?php if (empty($GLOBALS['jjwg_config']['google_maps_api_key'])): ?>
<!-- show error-->
<div class="error"><?= $GLOBALS['mod_strings']['LBL_ERROR_NO_GOOGLE_API_KEY'] ?></div>
<?php else: ?>
<!-- show map-->
<div id="map_canvas"></div>

  <br clear="all" />

<?php
  if (!empty($this->bean->map_center) || $num_markers > 0) {
      ?>
  <div id="legend">
  <b><?php echo $GLOBALS['mod_strings']['LBL_MAP_LEGEND']; ?></b><br/>
<?php
  if (!empty($this->bean->map_center)) {
      ?>
    <img src="<?php echo './'.$icons_dir.'/marker_0.png'; ?>" align="middle" />
    <?php echo $this->bean->map_center['name']; ?><br/>
<?php
  } ?>
  <!-- <b><?php echo $GLOBALS['mod_strings']['LBL_MAP_USER_GROUPS']; ?> </b><br/> -->
<?php
  foreach ($group_name_to_num as $group_name => $group_number) {
      ?>
    <img src="<?php echo './'.$icons_dir.'/marker_'.$group_number.'.png'; ?>"
         rel="<?php echo $group_number; ?>" align="middle" />
<?php
    if (empty($group_name)) {
        echo '{'.$GLOBALS['mod_strings']['LBL_MAP_NULL_GROUP_NAME'].'}';
    } else {
        echo htmlentities($group_name, ENT_COMPAT, "UTF-8", false);
    } ?><br/>
<?php
  } ?>
  </div>
<?php
  } ?>

<?php
  if ($num_markers > 0) {
      ?>
    <div id="DataTable">
        <table cellpadding="3" cellspacing="0" border="1" width="100%" class="list view" id="displayDataTable">
            <thead>
                <tr>
                    <th scope="col"><?php echo $GLOBALS['mod_strings']['LBL_ID']; ?></th>
                    <th scope="col"><?php echo $GLOBALS['mod_strings']['LBL_NAME']; ?></th>
                    <th scope="col"><?php echo $GLOBALS['mod_strings']['LBL_MAP_ADDRESS']; ?></th>
                    <th scope="col"><?php echo $GLOBALS['app_strings']['LBL_LIST_PHONE']; ?></th>
                    <th scope="col"><?php echo $GLOBALS['mod_strings']['LBL_MAP_GROUP']; ?></th>
                    <th scope="col"><?php echo $GLOBALS['mod_strings']['LBL_ASSIGNED_TO_NAME']; ?></th>
                    <th scope="col"><?php echo $GLOBALS['mod_strings']['LBL_MAP_TYPE']; ?></th>
                    <th scope="col"></th>
                </tr>
            </thead>
            <tbody>
                <tr>
                </tr>
            </tbody>
        </table>
    </div>
<?php
  } ?>

<?php
  if (in_array($this->bean->display_object->module_name, array('Accounts', 'Contacts', 'Leads', 'Prospects', 'Users')) &&
          ($GLOBALS['current_user']->isAdmin() || $this->bean->ACLAccess('list')) &&
          empty($_REQUEST['list_id']) && !empty($this->bean->list_array)) {
      ?>
<br clear="all" />
<div>
    <form id="tagetList" action="<?php echo htmlspecialchars((string) $_SERVER['PHP_SELF']); ?>" method="get">
        <input type="hidden" name="module" value="<?php echo htmlspecialchars((string) $GLOBALS['currentModule']); ?>">
        <input type="hidden" name="display_module" value="<?php echo htmlspecialchars((string) $this->bean->display_object->module_name); ?>">
        <input type="hidden" name="action" value="add_to_target_list" />
        <input type="hidden" name="to_pdf" value="1" />
        <?php if (array_key_exists('uid', $_GET)) {
          ?>
            <input type="hidden" name="selected_ids" value="<?php echo $_GET['uid'] ?>" />
        <?php
      } ?>
        <select id="list_id" tabindex="3" name="list_id" title="">
            <?php foreach ($this->bean->list_array as $key=>$value) {
          ?>
                <option value="<?php echo htmlspecialchars((string) $key); ?>"><?php echo htmlspecialchars((string) $value); ?></option>
            <?php
      } ?>
        </select>
        <input type="submit" value="<?php echo $GLOBALS['mod_strings']['LBL_ADD_TO_TARGET_LIST']; ?>">
        <span id="tagetListResult"></span>
    </form>
</div>
<?php
  } ?>
<?php endif ?>


</body>
</html>

<?php
// Testing Dump
//echo "<pre>";
//var_dump($this->bean);
//var_dump($GLOBALS['app_list_strings']);
//var_dump($GLOBALS['app_strings']);
//var_dump($GLOBALS['mod_strings']);
//echo "</pre>";
?>

</body>
</html>
<?php
    }
}
