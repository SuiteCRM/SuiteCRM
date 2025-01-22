<?php

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('modules/jjwg_Markers/jjwg_Markers_sugar.php');
require_once('modules/jjwg_Maps/jjwg_Maps.php');


#[\AllowDynamicProperties]
class jjwg_Markers extends jjwg_Markers_sugar
{

    /**
     * @var settings array
     */
    public $settings = array();

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
     *
     * Define Marker Location
     * @param $marker mixed (array or object)
     */
    public function define_loc($marker = array())
    {
        if (empty($marker)) {
            $marker = $this;
        }
        $loc = array();
        if (is_object($marker)) {
            $loc['name'] = $marker->name;
            $loc['lat'] = $marker->jjwg_maps_lat;
            $loc['lng'] = $marker->jjwg_maps_lng;
        } elseif (is_array($marker)) {
            $loc['name'] = $marker['name'];
            $loc['lat'] = $marker['lat'];
            $loc['lng'] = $marker['lng'];
        }
        if (empty($loc['name'])) {
            $loc['name'] = 'N/A';
            $loc['lat'] = null;
            $loc['lng'] = null;
        }
        if (!$this->is_valid_lat($loc['lat'])) {
            $loc['lat'] = $this->settings['map_default_center_latitude'];
        }
        if (!$this->is_valid_lng($loc['lng'])) {
            $loc['lng'] = $this->settings['map_default_center_longitude'];
        }

        if (!isset($marker->marker_image)) {
            LoggerManager::getLogger()->warn('jjwg_Markers define_loc: Trying to get property of non-object ($marker->marker_image)');
            $markerMarkerImage = null;
        } else {
            $markerMarkerImage = $marker->marker_image;
        }

        $loc['image'] = $markerMarkerImage;
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
}
