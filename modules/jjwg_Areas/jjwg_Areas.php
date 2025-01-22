<?php

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('modules/jjwg_Areas/jjwg_Areas_sugar.php');
require_once('modules/jjwg_Maps/jjwg_Maps.php');

#[\AllowDynamicProperties]
class jjwg_Areas extends jjwg_Areas_sugar
{

    /**
     * @var settings array
     */
    public $settings = array();
    /**
     * coords processed from coordinates string
     * @var array of strings ('lng,lat,elv')
     */
    public $coords = array();
    /**
     * polygon processed from coordinates strings
     * @var array of arrays (keys: lng, lat, elv)
     */
    public $polygon = null;
    /**
     * Point in Area/Polygon check on vertices
     * @var boolean
     */
    public $point_on_vertex = true;
    /**
     * @area Polygon Area
     */
    public $area = 0;
    /**
     * Polygon Centroid (Area Balance Center)
     * @var array (keys: lng, lat, elv)
     */
    public $centroid = null;

    public function __construct($init=true)
    {
        parent::__construct();
        // Admin Config Setting
        if ($init) {
            $this->configuration();
        }
    }

    /**
     * Load Configuration Settings using Administration Module
     * See jjwg_Maps module for setting config
     * $GLOBALS['jjwg_config_defaults']
     * $GLOBALS['jjwg_config']
     */
    public function configuration()
    {
        $this->jjwg_Maps = BeanFactory::newBean('jjwg_Maps');
        $this->settings = $GLOBALS['jjwg_config'];
    }

    /**
     * Retrieve object by id
     */
    public function retrieve($id = -1, $encode = true, $deleted = true)
    {
        parent::retrieve($id, $encode, $deleted);

        $this->polygon = $this->define_polygon();
        $this->area = $this->define_area();
        $this->centroid = $this->define_centroid();

        return $this;
    }

    /**
     *
     * Define polygon coordinates
     */
    public function define_polygon()
    {
        if (!empty($this->polygon)) {
            return $this->polygon;
        }

        if (preg_match('/[\n\r]/', (string) $this->coordinates)) {
            $this->coords = preg_split("/[\n\r\s]+/", (string) $this->coordinates, null, PREG_SPLIT_NO_EMPTY);
        } else {
            $this->coords = preg_split("/[\s]+/", (string) $this->coordinates, null, PREG_SPLIT_NO_EMPTY);
        }
        if (count($this->coords) > 0) {
            foreach ($this->coords as $coord) {
                $p = preg_split("/[\s\(\)]*,[\s\(\)]*/", (string) $coord, null, PREG_SPLIT_NO_EMPTY);
                if ($this->is_valid_lng($p[0]) && $this->is_valid_lat($p[1])) {
                    $this->polygon[] = array(
                        'lng' => $p[0],
                        'lat' => $p[1],
                        'elv' => $p[2],
                    );
                }
            }
        }

        if (!is_array($this->polygon)) {
            LoggerManager::getLogger()->warn('Parameter must be an array or an object that implements Countable');
        }

        if (count((array)$this->polygon) > 0) {
            return $this->polygon;
        } else {
            return false;
        }
    }

    /**
     *
     * Define Area Location based on Centroid
     * (Center of Gravity or Balance Point)
     *
     */
    public function define_area_loc()
    {
        $loc = array();
        $loc['name'] = $this->name;
        if (!is_null($this->centroid)) {
            $loc['lng'] = $this->centroid['lng'];
            $loc['lat'] = $this->centroid['lat'];
        } else {
            $loc['lng'] = null;
            $loc['lat'] = null;
        }
        $loc = $this->define_loc($loc);

        return $loc;
    }

    /**
     * Define Centroid - Point
     * @return type
     */
    public function define_centroid()
    {
        if (!empty($this->centroid)) {
            return $this->centroid;
        }

        if (empty($this->polygon)) {
            $this->polygon = $this->define_polygon();
        }

        $n = is_countable($this->polygon) ? count($this->polygon) : 0;
        $a = $this->define_area($this->polygon);
        if (empty($a)) {
            return $this->centroid;
        }
        $cx = 0.0;
        $cy = 0.0;
        // Set $p as Polygon and Add Closing Point
        $p = $this->polygon;
        $p[] = $p[0];

        for ($i = 0; $i < $n; $i++) {
            $cx += ($p[$i]['lng'] + $p[$i+1]['lng']) * (($p[$i]['lng'] * $p[$i+1]['lat']) - ($p[$i+1]['lng'] * $p[$i]['lat']));
            $cy += ($p[$i]['lat'] + $p[$i+1]['lat']) * (($p[$i]['lng'] * $p[$i+1]['lat']) - ($p[$i+1]['lng'] * $p[$i]['lat']));
        }
        $centroid_lng = -(1/(6*$a))*$cx;
        $centroid_lat = -(1/(6*$a))*$cy;

        if ($centroid_lng != 0 && $centroid_lat != 0) {
            $this->centroid = array(
                'lng' => $centroid_lng,
                'lat' => $centroid_lat,
                'elv' => 0
            );
        }

        return $this->centroid;
    }

    /**
     * Define Polygon Area
     * @return type
     */
    public function define_area()
    {
        if (!empty($this->area)) {
            return $this->area;
        }

        if (empty($this->polygon)) {
            $this->polygon = $this->define_polygon();
        }

        // Based on: http://forums.devnetwork.net/viewtopic.php?f=1&t=44074
        $n = is_countable($this->polygon) ? count($this->polygon) : 0;
        $area = 0.0;
        // Set $p as Polygon and Add Closing Point
        $p = $this->polygon;
        $p[] = $p[0];

        for ($i = 0; $i < $n; $i++) {
            $j = ($i + 1);
            $area += $p[$i]['lng'] * $p[$j]['lat'];
            $area -= $p[$i]['lat'] * $p[$j]['lng'];
        }
        $area /= 2;
        $this->area = abs($area);

        return $this->area;
    }

    /**
     *
     * Define Marker Location
     * @param $marker mixed (array or object)
     */
    public function define_loc($marker = array())
    {
        $loc = array();
        if (is_object($marker)) {
            $loc['name'] = $marker->name;
            $loc['lat'] = $marker->jjwg_maps_lat;
            $loc['lng'] = $marker->jjwg_maps_lng;
        } elseif (is_array($marker) && !empty($marker)) {
            $loc['name'] = $marker['name'];
            $loc['lat'] = $marker['lat'];
            $loc['lng'] = $marker['lng'];
        } else {
            $loc['name'] = '';
            if (is_null($this->centroid)) {
                $loc['lat'] = null;
                $loc['lng'] = null;
            } else {
                $loc['lat'] = $this->centroid['lat'];
                $loc['lng'] = $this->centroid['lng'];
            }
        }

        if (empty($loc['name'])) {
            $loc['name'] = 'N/A';
        }
        if (!$this->is_valid_lat($loc['lat'])) {
            $loc['lat'] = $this->settings['map_default_center_latitude'];
        }
        if (!$this->is_valid_lng($loc['lng'])) {
            $loc['lng'] = $this->settings['map_default_center_longitude'];
        }
        return $loc;
    }

    /**
     *
     * Check for valid longitude
     * @param $lng float
     */
    public function is_valid_lng($lng)
    {
        return (is_numeric($lng) && $lng >= -180 && $lng <= 180);
    }

    /**
     *
     * Check for valid latitude
     * @param $lat float
     */
    public function is_valid_lat($lat)
    {
        return (is_numeric($lat) && $lat >= -90 && $lat <= 90);
    }

    /**
     * Determine if Marker Object is in Area (Polygon)
     * @param object $marker
     */
    public function is_marker_in_area($marker)
    {
        $loc = array();
        if (is_object($marker)) {
            $loc['lat'] = $marker->jjwg_maps_lat;
            $loc['lng'] = $marker->jjwg_maps_lng;
        } elseif (is_array($marker)) {
            $loc['lat'] = $marker['lat'];
            $loc['lng'] = $marker['lng'];
        }

        return $this->is_point_in_area($loc['lng'], $loc['lat']);
    }

    /**
     * Determine if lng/lat point is in Area Polygon
     *
     * @param float $lng
     * @param float $lat
     */
    public function is_point_in_area($lng, $lat)
    {

        // lng,lat,elv
        $point = $lng.','.$lat.',0.0';

        return $this->point_in_polygon($point);
    }

    /**
     * Determine if Point lies within Polygon
     *
     * Polygon Algorithm: http://assemblysys.com/php-point-in-polygon-algorithm/
     * Modified by JJWDesign 07/16/2013
     *
     * @param string $point
     * @param boolean $point_on_vertex
     * @return boolean
     */
    public function point_in_polygon($point, $point_on_vertex = true)
    {
        $this->point_on_vertex = $point_on_vertex;
        $polygon = preg_split('/[\s]+/', (string) $this->coordinates);

        // Chek $polygon count
        if (!((is_countable($polygon) ? count($polygon) : 0) > 1)) {
            return false;
        }
        // Add the first point to the end, in order to properly close the loop completely
        if ($polygon[(is_countable($polygon) ? count($polygon) : 0)-1] !== $polygon[0]) {
            $polygon[] = $polygon[0];
        }

        // Transform string coordinates into arrays with x and y values
        $point = $this->point_string_to_coordinates($point);
        $vertices = array();
        foreach ($polygon as $vertex) {
            $vertices[] = $this->point_string_to_coordinates($vertex);
        }

        // Check if the point sits exactly on a vertex
        if ($this->point_on_vertex == true && $this->point_on_vertex($point, $vertices) == true) {
            return true;
        }

        // Check if the point is inside the polygon or on the boundary
        $intersections = 0;
        $vertices_count = count($vertices);

        for ($i=1; $i < $vertices_count; $i++) {
            $vertex1 = $vertices[$i-1];
            $vertex2 = $vertices[$i];
            if ($vertex1['y'] == $vertex2['y'] && $vertex1['y'] == $point['y'] && $point['x'] > min($vertex1['x'], $vertex2['x']) && $point['x'] < max($vertex1['x'], $vertex2['x'])) { // Check if point is on an horizontal polygon boundary
                return true;
            }
            if ($point['y'] > min($vertex1['y'], $vertex2['y']) && $point['y'] <= max($vertex1['y'], $vertex2['y']) && $point['x'] <= max($vertex1['x'], $vertex2['x']) && $vertex1['y'] != $vertex2['y']) {
                $xinters = ($point['y'] - $vertex1['y']) * ($vertex2['x'] - $vertex1['x']) / ($vertex2['y'] - $vertex1['y']) + $vertex1['x'];
                if ($xinters == $point['x']) { // Check if point is on the polygon boundary (other than horizontal)
                    return true;
                }
                if ($vertex1['x'] == $vertex2['x'] || $point['x'] <= $xinters) {
                    $intersections++;
                }
            }
        }
        // If the number of edges we passed through is odd, then it's in the polygon.
        if ($intersections % 2 != 0) {
            return true;
        } else {
            return false;
        }
    }

    public function point_on_vertex($point, $vertices)
    {
        foreach ($vertices as $vertex) {
            if ($point == $vertex) {
                return true;
            }
        }
    }

    public function point_string_to_coordinates($pointString)
    {

        // Coordinate Results (lng,lat,elv)
        $coordinates = preg_split('/[,]+/', (string) $pointString);
        return array("x" => $coordinates[0], "y" => $coordinates[1]);
    }
}
