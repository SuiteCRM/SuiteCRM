<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('modules/jjwg_Maps/jjwg_Maps_sugar.php');
require_once('modules/Administration/Administration.php');

#[\AllowDynamicProperties]
class jjwg_Maps extends jjwg_Maps_sugar
{

    /**
     * @var settings array
     *
     */
    public $settings = array(
        'google_maps_api_key' => '',
        /**
         * 'valid_geocode_modules' defines the valid module names used with geocoding.
         * @var array
         */
        'valid_geocode_modules' => array(
            'Accounts', 'Contacts', 'Leads', 'Opportunities', 'Cases', 'Project', 'Meetings', 'Prospects'
        ),
        /**
         * 'valid_geocode_tables' defines the valid table names used with geocoding.
         * @var array
         */
        'valid_geocode_tables' => array(
            'accounts', 'contacts', 'leads', 'opportunities', 'cases', 'project', 'meetings', 'prospects'
        ),
        /**
         * 'geocode_modules_to_address_type' defines the modules address types to be used with geocoding.
         * Acceptable Values: 'billing', 'shipping', 'primary', 'alt', 'flex_relate', 'custom', 'address'
         * @var array
         */
        'geocode_modules_to_address_type' => array(
            'Accounts' => 'billing',
            'Contacts' => 'primary',
            'Leads' => 'primary',
            'Opportunities' => 'billing',
            'Cases' => 'billing',
            'Project' => 'billing',
            'Meetings' => 'flex_relate',
            'Prospects' => 'primary',
            'Users' => 'address'
        ),
        /**
         * 'geocoding_api_url' sets the URL used for geocoding (Google or Proxy)
         * @var string
         */
        'geocoding_api_url' => 'https://maps.googleapis.com/maps/api/geocode/json?sensor=false',
        /**
         * 'geocoding_api_secret' sets a secret phrase to be used by a Proxy for hash comparison
         * @var string
         * 'hash' is added to the URL parameters as a MD5 of the Concatenation of the Address and the Secret
         */
        'geocoding_api_secret' => '',
        /**
         * 'geocoding_limit' sets the query limit when selecting records to geocode.
         * @var integer
         */
        'geocoding_limit' => 250,
        /**
         * 'google_geocoding_limit' sets the request limit when geocoding using the Google Maps API.
         * @var integer
         */
        'google_geocoding_limit' => 100,
        /**
         * 'allow_approximate_location_type' allows a Geocoding 'location_type' of 'APPROXIMATE' to be considered an 'OK' Status
         */
        'allow_approximate_location_type' => false,
        /**
         * 'map_markers_limit' sets the query limit when selecting records to display on a map.
         * @var integer
         */
        'map_markers_limit' => 1000,
        /**
         * 'map_default_unit_type' sets the default unit measurement type for distance calculations.
         * @var string
         * Values: 'mi' (miles) or 'km' (kilometers)
         */
        'map_default_unit_type' => 'mi',
        /**
         * 'map_default_distance' sets the default distance used for distance based maps.
         * @var float
         */
        'map_default_distance' => 250,
        /**
         * 'map_duplicate_marker_adjustment' sets an offset to be added to
         * longitude and latitude in case of duplicate marker position.
         * @var float
         */
        'map_duplicate_marker_adjustment' => 0.00002,
        /**
         * 'export_addresses_limit' sets the query limit when selecting records to export.
         * @var integer
         */
        'export_addresses_limit' => 50000,
        /**
         * 'map_markers_grouping_field' sets the field to be used as the group parameter when display on a map.
         * @var array
         * Examples: assigned_user_name, industry, status, sales_stage, priority
         */
        'map_markers_grouping_field' => array(
            'Accounts' => 'industry',
            'Contacts' => 'assigned_user_name',
            'Leads' => 'status',
            'Opportunities' => 'sales_stage',
            'Cases' => 'priority',
            'Project' => 'assigned_user_name',
            'Meetings' => 'assigned_user_name',
            'Prospects' => 'assigned_user_name',
            'Users' => 'user_name'
        ),
        /**
         * 'map_clusterer_grid_size' is used to set the grid size for calculating map clusterers.
         * @var integer
         */
        'map_clusterer_grid_size' => 50,
        /**
         * 'map_clusterer_max_zoom' is used to set the maximum zoom level at which clustering will not be applied.
         * @var integer
         */
        'map_clusterer_max_zoom' => 14,
        /**
         * 'map_default_center_latitude' sets the default center latitude position for maps.
         * @var float
         */
        'map_default_center_latitude' => 39.5,
        /**
         * 'map_default_center_longitude' sets the default center longitude position for maps.
         * @var float
         */
        'map_default_center_longitude' => -99.5,
        /**
         * 'address_cache_get_enabled' allows for the getAddressCacheGeocode()
         * method to retrieve data from cache table: jjwg_Address_Cache
         * @var boolean 1/0
         */
        'address_cache_get_enabled' => true,

        /**
         * 'address_cache_save_enabled' allows for the saveAddressCacheGeocode()
         * method to save data to cache table: jjwg_Address_Cache
         * @var boolean 1/0
         */
        'address_cache_save_enabled' => true,

        /**
         * 'logic_hooks_enabled' allows admins to disable the logic hooks functionality
         * which is very useful during upgrading processes
         */
        'logic_hooks_enabled' => false
    );

    /**
     * relate_object - related module's object (flex relate field)
     * @var object
     */
    public $relate_object;

    /**
     * jjwg_Address_Cache - Address cache module's object
     * @var object
     */
    public $jjwg_Address_Cache;


    /**
     * geocoded_counts - Geocoding totals
     * @var array
     */
    public $geocoded_counts = null;

    /**
     * geocoded_headings - Display headings
     * @var array
     */
    public $geocoded_headings = null;

    /**
     * geocoded_module_totals - Geocoded module totals
     * @var array
     */
    public $geocoded_module_totals = null;

    /**
     * geocoding_results - Google Geocoding API Results
     * @var array
     */
    public $geocoding_results = null;

    /**
     * map_center - Map Center (Related)
     * @var array
     */
    public $map_center = null;

    /**
     * map_markers - Map Marker Data (Display)
     * @var array
     */
    public $map_markers = null;

    /**
     * map_markers_groups - Sets the array of map groups
     * @var array
     */
    public $map_markers_groups = array();

    /**
     * map_markers - Custom Markers Data (jjwg_Markers)
     * @var array
     */
    public $custom_markers = null;

    /**
     * custom_areas - Custom Areas Data (jjwg_Areas)
     * @var array
     */
    public $custom_areas = null;



    /**
     * Constructor
     */
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
     *
     */
    public function configuration()
    {

        // Set defaults
        $GLOBALS['jjwg_config_defaults'] = $this->settings;

        // Adjust Query Limit
        if ($GLOBALS['sugar_config']['resource_management']['default_limit'] < 20000) {
            $GLOBALS['sugar_config']['resource_management']['default_limit'] = 20000;
        }

        $admin = BeanFactory::newBean('Administration');
        $admin->retrieveSettings('jjwg', true);
        $settings = $admin->settings;

        $rev = array();
        if (!empty($settings) && (is_countable($settings) ? count($settings) : 0) > 0) {
            foreach ($settings as $category_name => $value) {
                if (substr((string) $category_name, 0, 5) == 'jjwg_') {
                    $name = substr((string) $category_name, 5);
                    // Set revised settings array
                    $rev[$name] = $value;
                }
            }
        }

        if (!empty($rev) && count($rev) > 0) {
            foreach ($rev as $name => $value) {

                // Set geocode_modules_to_address_type
                if (substr($name, 0, 13) == 'address_type_') {
                    $module = substr($name, 13);
                    if (in_array($value, array('billing', 'shipping', 'primary', 'alt', 'flex_relate', 'custom', 'address'))) {
                        $this->settings['geocode_modules_to_address_type'][$module] = $value;
                    }
                }

                // Set map_markers_grouping_field
                if (substr($name, 0, 15) == 'grouping_field_') {
                    $module = substr($name, 15);
                    if (!empty($value)) {
                        $this->settings['map_markers_grouping_field'][$module] = $value;
                    }
                }
            }

            if (!empty($rev['valid_geocode_modules'])) {
                if (!is_array($rev['valid_geocode_modules'])) {
                    $this->settings['valid_geocode_modules'] = preg_split('/[\s,]+/', (string) $rev['valid_geocode_modules']);
                }
            }
            if (!empty($rev['valid_geocode_tables'])) {
                if (!is_array($rev['valid_geocode_tables'])) {
                    $this->settings['valid_geocode_tables'] = preg_split('/[\s,]+/', (string) $rev['valid_geocode_tables']);
                }
            }

            // Integer Settings
            $int_settings = array('geocoding_limit', 'google_geocoding_limit',
                'map_markers_limit', 'map_default_distance', 'export_addresses_limit',
                'map_clusterer_grid_size', 'map_clusterer_max_zoom');
            foreach ($int_settings as $setting) {
                if (isset($rev[$setting]) && is_numeric($rev[$setting])) {
                    $this->settings[$setting] = (int) $rev[$setting];
                }
            }

            // Float/Language Settings
            if (isset($rev['map_default_unit_type']) && in_array($rev['map_default_unit_type'], array('mi', 'km'))) {
                $this->settings['map_default_unit_type'] = $rev['map_default_unit_type'];
            }
            if (isset($rev['map_duplicate_marker_adjustment']) && is_numeric($rev['map_duplicate_marker_adjustment'])) {
                $this->settings['map_duplicate_marker_adjustment'] = (float) $rev['map_duplicate_marker_adjustment'];
            }
            if (isset($rev['map_default_center_latitude']) && is_numeric($rev['map_default_center_latitude'])) {
                $this->settings['map_default_center_latitude'] = (float) $rev['map_default_center_latitude'];
            }
            if (isset($rev['map_default_center_longitude']) && is_numeric($rev['map_default_center_longitude'])) {
                $this->settings['map_default_center_longitude'] = (float) $rev['map_default_center_longitude'];
            }

            // Address Cache Settings: true/false or 1/0
            $this->settings['address_cache_get_enabled'] = (!empty($rev['address_cache_get_enabled'])) ? true : false;
            $this->settings['address_cache_save_enabled'] = (!empty($rev['address_cache_save_enabled'])) ? true : false;
            // Logic Hooks: true/false or 1/0
            $this->settings['logic_hooks_enabled'] = (!empty($rev['logic_hooks_enabled'])) ? true : false;
            // Allow APPROXIMATE 'location_type'
            $this->settings['allow_approximate_location_type'] = (!empty($rev['allow_approximate_location_type'])) ? true : false;

            // Set Geocoding API URL or Proxy URL
            if (isset($rev['geocoding_api_url'])) {
                $this->settings['geocoding_api_url'] = $rev['geocoding_api_url'];
            }
            // Set Google Maps API Secret
            if (isset($rev['geocoding_api_secret'])) {
                $this->settings['geocoding_api_secret'] = $rev['geocoding_api_secret'];
            }
            // Set Google Maps API Key
            if (!isset($rev['google_maps_api_key'])) {
                $GLOBALS['log']->warn('Undefined index: google_maps_api_key');
                $this->settings['google_maps_api_key'] = null;
            } else {
                $this->settings['google_maps_api_key'] = $rev['google_maps_api_key'];
            }
        }

        // Set for Global Use
        $GLOBALS['jjwg_config'] = $this->settings;
    }

    /**
     * Save Configuration Settings using Administration Module
     *
     * @param $data array of post data
     */
    public function saveConfiguration($data = array())
    {
        $admin = BeanFactory::newBean('Administration');
        //$admin->retrieveSettings('jjwg', true);
        //$settings = $admin->settings;
        $category = 'jjwg';

        if (!empty($data) && (is_countable($data) ? count($data) : 0) > 0) {
            if (isset($data['google_maps_api_key'])) {
                $admin->saveSetting($category, 'google_maps_api_key', $data['google_maps_api_key']);
            }

            if (isset($data['valid_geocode_modules'])) {
                if (is_array($data['valid_geocode_modules'])) {
                    $data['valid_geocode_modules'] = implode(', ', $data['valid_geocode_modules']);
                }
                $admin->saveSetting($category, 'valid_geocode_modules', $data['valid_geocode_modules']);
            }
            if (isset($data['valid_geocode_tables'])) {
                if (is_array($data['valid_geocode_tables'])) {
                    $data['valid_geocode_tables'] = implode(', ', $data['valid_geocode_tables']);
                }
                $admin->saveSetting($category, 'valid_geocode_tables', $data['valid_geocode_tables']);
            }

            foreach ($data as $name => $value) {

                // Trim white space on each
                $value = trim($value);

                // Set geocode_modules_to_address_type
                if (substr((string) $name, 0, 13) == 'address_type_') {
                    $module = substr((string) $name, 13);
                    if (in_array($module, $this->settings['valid_geocode_modules'])) {
                        if (in_array($value, array('billing', 'shipping', 'primary', 'alt', 'flex_relate', 'custom', 'address'))) {
                            $admin->saveSetting($category, $name, $value);
                        }
                    }
                }

                // Set map_markers_grouping_field
                if (substr((string) $name, 0, 15) == 'grouping_field_') {
                    $module = substr((string) $name, 15);
                    if (in_array($module, $this->settings['valid_geocode_modules'])) {
                        if (!empty($value)) {
                            $admin->saveSetting($category, $name, $value);
                        }
                    }
                }
            }

            // Integer Settings
            $int_settings = array('geocoding_limit', 'google_geocoding_limit',
                'map_markers_limit', 'map_default_distance', 'export_addresses_limit',
                'map_clusterer_grid_size', 'map_clusterer_max_zoom',
                'address_cache_get_enabled', 'address_cache_save_enabled',
                'logic_hooks_enabled', 'allow_approximate_location_type');
            foreach ($int_settings as $setting) {
                if (isset($data[$setting]) && is_numeric(trim($data[$setting]))) {
                    $admin->saveSetting($category, $setting, (int) trim($data[$setting]));
                }
            }

            // Float/Language Settings
            if (isset($data['map_default_unit_type']) && in_array(trim($data['map_default_unit_type']), array('mi', 'km'))) {
                $admin->saveSetting($category, 'map_default_unit_type', trim($data['map_default_unit_type']));
            }
            if (empty($data['map_duplicate_marker_adjustment'])) {
                $data['map_duplicate_marker_adjustment'] = 0.00002;
            }
            if (isset($data['map_duplicate_marker_adjustment']) && is_numeric(trim($data['map_duplicate_marker_adjustment']))) {
                $admin->saveSetting($category, 'map_duplicate_marker_adjustment', (float) trim($data['map_duplicate_marker_adjustment']));
            }

            if (!isset($data['map_default_center_latitude'])) {
                LoggerManager::getLogger()->warn('jjwg_Maps saveConfiguration: Undefined index: map_default_center_latitude');
                $dataMapDefaultCenterLatitude = null;
            } else {
                $dataMapDefaultCenterLatitude = $data['map_default_center_latitude'];
            }

            if (!$this->is_valid_lat($dataMapDefaultCenterLatitude)) {
                $data['map_default_center_latitude'] = 39.5;
            }

            if (isset($data['map_default_center_latitude']) && is_numeric(trim($data['map_default_center_latitude']))) {
                $admin->saveSetting($category, 'map_default_center_latitude', (float) trim($data['map_default_center_latitude']));
            }

            if (!isset($data['map_default_center_longitude'])) {
                LoggerManager::getLogger()->warn('jjwg_Maps saveConfiguration: Undefined index: map_default_center_longitude');
                $dataMapDefaultCenterLongitude = null;
            } else {
                $dataMapDefaultCenterLongitude = $data['map_default_center_longitude'];
            }


            if (!$this->is_valid_lng($dataMapDefaultCenterLongitude)) {
                $data['map_default_center_longitude'] = -99.5;
            }

            if (isset($data['map_default_center_longitude']) && is_numeric(trim($data['map_default_center_longitude']))) {
                $admin->saveSetting($category, 'map_default_center_longitude', (float) trim($data['map_default_center_longitude']));
            }

            // Set Geocoding API URL or Proxy URL

            if (!isset($data['geocoding_api_url'])) {
                LoggerManager::getLogger()->warn('jjwg_Maps saveConfiguration: Undefined index: geocoding_api_url ');
                $dataGeocodingApiUrl = null;
            } else {
                $dataGeocodingApiUrl = $data['geocoding_api_url'];
            }

            if (substr((string) $dataGeocodingApiUrl, 0, 4) != 'http' && substr((string) $dataGeocodingApiUrl, 0, 2) != '//') {
                $data['geocoding_api_url'] = $this->settings['geocoding_api_url'];
            }

            if (isset($data['geocoding_api_url'])) {
                $admin->saveSetting($category, 'geocoding_api_url', trim($data['geocoding_api_url']));
            }
            // Set Google Maps API Secret
            if (empty($data['geocoding_api_secret'])) {
                $data['geocoding_api_secret'] = '';
            }
            if (isset($data['geocoding_api_secret'])) {
                $admin->saveSetting($category, 'geocoding_api_secret', trim($data['geocoding_api_secret']));
            }

            return true;
        }

        return false;
    }

    /**
     * Update Geocode Information for $bean
     *
     * This is method is executed as a before_save logic hook.
     * With before_save, only the values are updated.
     * See before_save logic hooks
     *
     * defineMapsAddress() should find the address by the parent relationship if needed.
     * That is, the parent relationship should define the new maps address.
     *
     * $bean passed by reference
     *
     */
    public function updateGeocodeInfo(&$bean, $after_save = false)
    {
        $GLOBALS['log']->info(__METHOD__.' START');
        if (empty($bean->id) || empty($bean->object_name) || empty($bean->module_name)) {
            return false;
        }
        $GLOBALS['log']->info(__METHOD__.' $bean->object_name: '.$bean->object_name);

        // Make sure we have the custom fields in our $bean
        if ($after_save === true) {
            $bean->custom_fields->retrieve();
        }
        $this->logGeocodeInfo($bean);

        // Define the Address Info based on the new bean data
        // $bean_data is not fetched_row data, but rather the properties of the bean as an array
        $bean_data = get_object_vars($bean);
        $aInfo = $this->defineMapsAddress($bean->object_name, $bean_data);
        $GLOBALS['log']->debug(__METHOD__.' $aInfo: '.print_r($aInfo, true));

        // If the related Account address has changed then reset the geocode values
        if (isset($aInfo['address']) && $aInfo['address'] != $bean->fetched_row['jjwg_maps_address_c']) {

            // If needed, check the Address Cache Module for Geocode Info
            if (!is_object($this->jjwg_Address_Cache)) {
                $this->jjwg_Address_Cache = get_module_info('jjwg_Address_Cache');
            }
            // Check Cache, if address is set
            if (!empty($aInfo['address']) && is_object($this->jjwg_Address_Cache)) {
                $aInfoCache = $this->jjwg_Address_Cache->getAddressCacheInfo($aInfo);
                $GLOBALS['log']->debug(__METHOD__.' $aInfoCache: '.print_r($aInfoCache, true));
                if (!empty($aInfoCache['address'])) {
                    $aInfo = $aInfoCache;
                }
            }
            // Update/Reset the Geocode fields
            $bean->jjwg_maps_lat_c = (!empty($aInfo['lat'])) ? $aInfo['lat'] : 0;
            $bean->jjwg_maps_lng_c = (!empty($aInfo['lng'])) ? $aInfo['lng'] : 0;
            $bean->jjwg_maps_geocode_status_c = (!empty($aInfo['status'])) ? $aInfo['status'] : '';
            $bean->jjwg_maps_address_c = (!empty($aInfo['address'])) ? $aInfo['address'] : '';
        }
    }

    /**
     * Update Geocode Information for $bean's Related Meetings
     *
     * This is done typically after the $bean is already saved.
     * See after_save logic hooks
     * Meetings related to: accounts, cases, contacts, leads
     *      opportunities, projects and prospects (targets)
     *
     * This method updates the meeting's address info based on
     * the current related bean address info.
     *
     * Note, it was decided to not define the Meeting beans to
     * improve performance and not trigger additional logic hooks.
     *
     * @param $bean
     */
    public function updateRelatedMeetingsGeocodeInfo(&$bean)
    {
        $GLOBALS['log']->info(__METHOD__.' START');
        if (empty($bean->id) || empty($bean->object_name) || empty($bean->module_name)) {
            return false;
        }
        $GLOBALS['log']->info(__METHOD__.' $bean->object_name: '.$bean->object_name);

        // Check to see if address info is already set, or redefine
        $bean_data = get_object_vars($bean);
        $aInfo = $this->defineMapsAddress($bean->object_name, $bean_data);
        $GLOBALS['log']->debug(__METHOD__.' $bean_data: '.print_r($bean_data, true));
        $GLOBALS['log']->debug(__METHOD__.' $aInfo: '.$aInfo);

        // If needed, check the Address Cache Module for Geocode Info
        if (!is_object($this->jjwg_Address_Cache)) {
            $this->jjwg_Address_Cache = get_module_info('jjwg_Address_Cache');
        }
        // Check Cache, if address is set
        if (!empty($aInfo['address']) && is_object($this->jjwg_Address_Cache)) {
            $aInfoCache = $this->jjwg_Address_Cache->getAddressCacheInfo($aInfo);
            $GLOBALS['log']->debug(__METHOD__.' $aInfoCache: '.$aInfoCache);
            if (!empty($aInfoCache['address'])) {
                $aInfo = $aInfoCache;
            }
        }

        $lngQ = (!empty($aInfo['lng'])) ? $this->db->quote($aInfo['lng']) : 0;
        $latQ = (!empty($aInfo['lat'])) ? $this->db->quote($aInfo['lat']) : 0;
        $statusQ = (!empty($aInfo['status'])) ? $this->db->quote($aInfo['status']) : '';
        $addressQ = (!empty($aInfo['address'])) ? $this->db->quote($aInfo['address']) : '';

        // Find the Related Meetings by parent_type & parent_id (this relationship is special)
        $query = "SELECT meetings.id, meetings_cstm.id_c FROM meetings " .
                " LEFT JOIN meetings_cstm ON meetings.id = meetings_cstm.id_c " .
                " WHERE meetings.deleted = 0 AND " .
                " meetings.parent_type = '" . $this->db->quote($bean->module_name) . "' AND " .
                " meetings.parent_id = '" . $this->db->quote($bean->id) . "'";
        $result = $this->db->query($query);

        while ($row = $this->db->fetchByAssoc($result)) {
            $idQ = $this->db->quote($row['id']);
            if (!empty($row['id_c'])) {
                // Update Custom Fields
                $query = "UPDATE meetings_cstm SET" .
                        " jjwg_maps_lat_c = '" . $latQ . "'," .
                        " jjwg_maps_lng_c = '" . $lngQ . "'," .
                        " jjwg_maps_geocode_status_c = '".$statusQ."'," .
                        " jjwg_maps_address_c = '" . $addressQ . "'" .
                        " WHERE id_c = '" . $idQ . "'";
                $update_result = $this->db->query($query);
            } else {
                // Insert Custom Fields
                $query = "INSERT INTO meetings_cstm" .
                        " (id_c, jjwg_maps_lat_c, jjwg_maps_lng_c, jjwg_maps_geocode_status_c, jjwg_maps_address_c)" .
                        " VALUES ('" . $idQ . "', '" . $latQ . "', '" . $lngQ . "', '".$statusQ."', '" . $addressQ . "') ";
                $insert_result = $this->db->query($query);
            }
        }
    }

    /**
     * Update Geocode Information for Meeting $bean
     *
     * Meeting Only. This method is executed as an after_save logic hook
     * due to the timing that parent_type and parent_id are saved.
     * The values are updated by queries in the updateGeocodeInfoByBeanQuery method.
     *
     * defineMapsAddress() should find the address by the parent relationship if needed.
     * That is, the parent relationship should define the new maps address.
     *
     * $bean passed by reference
     */
    public function updateMeetingGeocodeInfo(&$bean)
    {
        $GLOBALS['log']->info(__METHOD__.' START');
        if (empty($bean->object_name)) {
            return false;
        }
        $GLOBALS['log']->info(__METHOD__.' $bean->object_name: '.$bean->object_name);

        // Make sure we have the custom fields in our $bean
        $bean->custom_fields->retrieve();
        $this->logGeocodeInfo($bean);

        // Define the Address Info based on the new bean data
        // $bean_data is not fetched_row data, but rather the properties of the bean as an array
        $bean_data = get_object_vars($bean);
        $aInfo = $this->defineMapsAddress($bean->object_name, $bean_data);
        $GLOBALS['log']->debug(__METHOD__.' $aInfo: '.print_r($aInfo, true));

        // If the related Account address has changed then reset the geocode values
        if (isset($aInfo['address']) && $aInfo['address'] != $bean->fetched_row['jjwg_maps_address_c']) {

            // If needed, check the Address Cache Module for Geocode Info
            if (!is_object($this->jjwg_Address_Cache)) {
                $this->jjwg_Address_Cache = get_module_info('jjwg_Address_Cache');
            }
            // Check Cache, if address is set
            if (!empty($aInfo['address']) && is_object($this->jjwg_Address_Cache)) {
                $aInfoCache = $this->jjwg_Address_Cache->getAddressCacheInfo($aInfo);
                $GLOBALS['log']->debug(__METHOD__.' $aInfoCache: '.print_r($aInfoCache, true));
                if (!empty($aInfoCache['address'])) {
                    $aInfo = $aInfoCache;
                }
            }
            // Update/Reset the Geocode fields using Queries (not save() bean method)
            $update_result = $this->updateGeocodeInfoByBeanQuery($bean, $aInfo);
        }
    }

    /**
     * Used to Update or Remove Geocoding Information from Custom Table
     * Simple Query Approach
     *
     * @param type $table_name string
     * @param type $display array (fetched_row)
     * @param type $aInfo   array
     */
    public function updateGeocodeInfoByAssocQuery($table_name, $display, $aInfo = array())
    {
        $GLOBALS['log']->info(__METHOD__.' START');
        if (empty($display['id']) || empty($table_name)) {
            return false;
        }
        $GLOBALS['log']->info(__METHOD__.' $display[\'id\']: '.$display['id']);

        if (!(in_array($table_name, $this->settings['valid_geocode_tables']))) {
            return false;
        }
        $idQ = $this->db->quote($display['id']);
        $lngQ = (!empty($aInfo['lng'])) ? $this->db->quote($aInfo['lng']) : 0;
        $latQ = (!empty($aInfo['lat'])) ? $this->db->quote($aInfo['lat']) : 0;
        $statusQ = (!empty($aInfo['status'])) ? $this->db->quote($aInfo['status']) : '';
        $addressQ = (!empty($aInfo['address'])) ? $this->db->quote($aInfo['address']) : '';

        // Find record by id
        $query = "SELECT ".$table_name.".id, ".$table_name."_cstm.id_c FROM ".$table_name." " .
                " LEFT JOIN ".$table_name."_cstm ON ".$table_name.".id = ".$table_name."_cstm.id_c " .
                " WHERE ".$table_name.".deleted = 0 AND ".$table_name.".id = '" . $idQ . "'";
        $result = $this->db->query($query);
        $row = $this->db->fetchByAssoc($result);

        if (!empty($row['id_c'])) {
            // Update Custom Fields
            $query = "UPDATE " . $table_name . "_cstm SET" .
                    " jjwg_maps_lat_c = '" . $latQ . "'," .
                    " jjwg_maps_lng_c = '" . $lngQ . "'," .
                    " jjwg_maps_geocode_status_c = '".$statusQ."'," .
                    " jjwg_maps_address_c = '" . $addressQ . "'" .
                    " WHERE id_c = '" . $idQ . "'";
            $update_result = $this->db->query($query);
        } else {
            // Insert Custom Fields
            $query = "INSERT INTO " . $table_name . "_cstm" .
                    " (id_c, jjwg_maps_lat_c, jjwg_maps_lng_c, jjwg_maps_geocode_status_c, jjwg_maps_address_c)" .
                    " VALUES ('" . $idQ . "', '" . $latQ . "', '" . $lngQ . "', '".$statusQ."', '" . $addressQ . "') ";
            $insert_result = $this->db->query($query);
        }
    }

    /**
     * Used to Update or Remove Geocoding Information from Custom Table
     * Simple Query Approach
     *
     * @param type $bean    object
     * @param type $aInfo   array
     */
    public function updateGeocodeInfoByBeanQuery(&$bean, $aInfo = array())
    {
        $GLOBALS['log']->info(__METHOD__.' START');
        if (empty($bean->id) || empty($bean->object_name) || empty($bean->table_name)) {
            return false;
        }
        $GLOBALS['log']->info(__METHOD__.' $bean->object_name: '.$bean->object_name);

        $table_name = $bean->table_name;
        if (!(in_array($table_name, $this->settings['valid_geocode_tables']))) {
            return false;
        }
        $idQ = $this->db->quote($bean->id);
        $lngQ = (!empty($aInfo['lng'])) ? $this->db->quote($aInfo['lng']) : 0;
        $latQ = (!empty($aInfo['lat'])) ? $this->db->quote($aInfo['lat']) : 0;
        $statusQ = (!empty($aInfo['status'])) ? $this->db->quote($aInfo['status']) : '';
        $addressQ = (!empty($aInfo['address'])) ? $this->db->quote($aInfo['address']) : '';

        // Find record by id
        $query = "SELECT ".$table_name.".id, ".$table_name."_cstm.id_c FROM ".$table_name." " .
                " LEFT JOIN ".$table_name."_cstm ON ".$table_name.".id = ".$table_name."_cstm.id_c " .
                " WHERE ".$table_name.".deleted = 0 AND ".$table_name.".id = '" . $idQ . "'";
        $result = $this->db->query($query);
        $row = $this->db->fetchByAssoc($result);

        if (!empty($row['id_c'])) {
            // Update Custom Fields
            $query = "UPDATE " . $table_name . "_cstm SET" .
                    " jjwg_maps_lat_c = '" . $latQ . "'," .
                    " jjwg_maps_lng_c = '" . $lngQ . "'," .
                    " jjwg_maps_geocode_status_c = '".$statusQ."'," .
                    " jjwg_maps_address_c = '" . $addressQ . "'" .
                    " WHERE id_c = '" . $idQ . "'";
            $update_result = $this->db->query($query);
        } else {
            // Insert Custom Fields
            $query = "INSERT INTO " . $table_name . "_cstm" .
                    " (id_c, jjwg_maps_lat_c, jjwg_maps_lng_c, jjwg_maps_geocode_status_c, jjwg_maps_address_c)" .
                    " VALUES ('" . $idQ . "', '" . $latQ . "', '" . $lngQ . "', '".$statusQ."', '" . $addressQ . "') ";
            $insert_result = $this->db->query($query);
        }
    }

    /**
     * Remove All Geocoding Information from Custom Table for Module
     * Simple Query Approach
     *
     * @param type $bean    object
     */
    public function deleteAllGeocodeInfoByBeanQuery(&$bean)
    {
        $GLOBALS['log']->info(__METHOD__.' START');
        if (empty($bean->object_name) || empty($bean->table_name)) {
            return false;
        }
        $GLOBALS['log']->info(__METHOD__.' $bean->object_name: '.$bean->object_name);

        $table_name = $bean->table_name;
        if (!(in_array($table_name, $this->settings['valid_geocode_tables']))) {
            return false;
        }

        // Update Fields to NULL
        $query = "UPDATE " . $table_name . "_cstm SET" .
                " jjwg_maps_lat_c = NULL," .
                " jjwg_maps_lng_c = NULL," .
                " jjwg_maps_geocode_status_c = NULL," .
                " jjwg_maps_address_c = NULL" .
                " WHERE 1=1";
        $update_result = $this->db->query($query);
    }

    /**
     * Get $db result of records (addresses) in need of geocoding
     * @param $table_name string
     * @param $limit integer
     * @param $id string
     */
    public function getGeocodeAddressesResult($table_name, $limit = 0, $id = '')
    {
        if (!(in_array($table_name, $this->settings['valid_geocode_tables']))) {
            return false;
        }
        if (empty($limit) || !is_int($limit)) {
            $limit = $this->settings['geocoding_limit'];
        }
        // Find the Items to Geocode
        // Assume there is no address at 0,0; it's in the Atlantic Ocean
        $where_conds = "(" .
                "(" . $table_name . "_cstm.jjwg_maps_lat_c = 0 AND " .
                "" . $table_name . "_cstm.jjwg_maps_lng_c = 0)" .
                " OR " .
                "(" . $table_name . "_cstm.jjwg_maps_lat_c IS NULL AND " .
                "" . $table_name . "_cstm.jjwg_maps_lng_c IS NULL)" .
                ")" .
                " AND " .
                "(" . $table_name . "_cstm.jjwg_maps_geocode_status_c = '' OR " .
                "" . $table_name . "_cstm.jjwg_maps_geocode_status_c IS NULL)";
        // Limit to only one result
        if (@is_guid($id)) {
            $where_conds .= " AND id = '".$id."'";
        }
        // Create Simple Query
        // Note: Do not use create_new_list_query() or process_list_query()
        // as they will typically exceeded memory allowed.
        $query = "SELECT " . $table_name . ".*, " . $table_name . "_cstm.* FROM " . $table_name .
                " LEFT JOIN " . $table_name . "_cstm " .
                " ON " . $table_name . ".id = " . $table_name . "_cstm.id_c " .
                " WHERE " . $table_name . ".deleted = 0 AND " . $where_conds;
        $display_result = $this->db->limitQuery($query, 0, $limit);

        return $display_result;
    }

    /**
     * getGoogleMapsGeocode - Get Lng/Lat using Google Maps V3
     * @var $address
     * @var $return_full_array boolean
     * @var $allow_approximate boolean
     */
    public function getGoogleMapsGeocode($address, $return_full_array = false, $allow_approximate = true)
    {
        $GLOBALS['log']->debug(__METHOD__.' START');
        $GLOBALS['log']->info(__METHOD__.' $address: '.$address);

        /* allow_approximate_location_type - overrides only to true */
        if (!empty($this->settings['allow_approximate_location_type'])) {
            $allow_approximate = true;
        }

        /**
         * Google Maps API v3 - The new v3 Google Maps API no longer requires a Maps API Key!
         * Old Default: https://maps.google.com/maps/api/geocode/json?sensor=false
         * New Default: https://maps.googleapis.com/maps/api/geocode/json?sensor=false
         */
        $base_url = $this->settings['geocoding_api_url'];
        if (!(strpos((string) $base_url, '?') > 0)) {
            $base_url .= '?';
        }
        // Add Address Parameter
        $request_url = $base_url . "&address=" . urlencode($address);
        $request_url.="&key=".urlencode($this->settings['google_maps_api_key']);

        $GLOBALS['log']->info(__METHOD__.' cURL Request URL: '.$request_url);

        $ch = curl_init();
        curl_setopt($ch, CURLOPT_URL, $request_url);
        curl_setopt($ch, CURLOPT_USERAGENT, "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.9.0.1) Gecko/2008070208 Firefox/3.0.1");
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_CONNECTTIMEOUT, 15);
        curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, false);

        // Add proxy option if user enabled proxy
        $admin_config = BeanFactory::newBean('Administration');
        $admin_config->retrieveSettings('proxy');
        if (!empty($admin_config->settings['proxy_on'])) {
            $proxy_host = $admin_config->settings['proxy_host'];
            $proxy_port = $admin_config->settings['proxy_port'];

            curl_setopt($ch, CURLOPT_PROXY, $proxy_host);
            curl_setopt($ch, CURLOPT_PROXYPORT, $proxy_port);

            // Check if use proxy auth
            if (!empty($admin_config->settings['proxy_auth'])) {
                $proxy_userpwd = $admin_config->settings['proxy_username'] . ':' . $admin_config->settings['proxy_password'];
                curl_setopt($ch, CURLOPT_PROXYUSERPWD, $proxy_userpwd);
            }
        }
        // End

        $json_contents = curl_exec($ch);

        // Debug: Error Handling
        $ch_errno = curl_errno($ch);
        $ch_error = curl_error($ch);
        if (!empty($ch_errno) || !empty($ch_error)) {
            $GLOBALS['log']->error(__METHOD__.' cURL Error: #'.$ch_errno.' - '.$ch_error);
        }

        curl_close($ch);
        $GLOBALS['log']->debug(__METHOD__.' $json_contents: '.$json_contents);
        $googlemaps = json_decode($json_contents, true);
        $GLOBALS['log']->debug(__METHOD__.' $googlemaps: '. (is_array($googlemaps) ? '[Array]' : $googlemaps));

        /**
         * https://developers.google.com/maps/documentation/geocoding/#Results
         * Status: "OK" : geocoding was successful
         * "ZERO_RESULTS" : indicates that the geocode was successful but returned no results
         * "OVER_QUERY_LIMIT" : indicates that you are over your quota
         * "REQUEST_DENIED" : lack of sensor parameter
         * "INVALID_REQUEST" generally indicates that the query (address or lat/lng) is missing.
         * Limit to location_type = 'ROOFTOP', 'RANGE_INTERPOLATED' or 'GEOMETRIC_CENTER' but not 'APPROXIMATE'
         */
        $aInfo = array('address' => $address);
        if (!empty($googlemaps) && isset($googlemaps['status'])) {
            $resultsGeography = $googlemaps['results'][0]['geometry'] ?? [];
            if ($googlemaps['status'] == 'OVER_QUERY_LIMIT') {
                // Debug: Log Over Limit
                $GLOBALS['log']->warn(__METHOD__.' Google Maps API Status of OVER_QUERY_LIMIT: Over Your Quota');
            } elseif (!$allow_approximate && ($resultsGeography['location_type'] ?? '') == 'APPROXIMATE') {
                // Consider 'APPROXIMATE' to be similar to 'ZERO_RESULTS'
                @$aInfo = array(
                    'address' => $address,
                    'status' => 'APPROXIMATE',
                    'lat' => $resultsGeography['location']['lat'],
                    'lng' => $resultsGeography['location']['lng']
                );
            } else {
                // Return address info
                @$aInfo = array(
                    'address' => $address,
                    'status' => $googlemaps['status'],
                    'lat' => $resultsGeography['location']['lat'],
                    'lng' => $resultsGeography['location']['lng']
                );
            }
        }

        if ($return_full_array) {
            $GLOBALS['log']->debug(__METHOD__.' $googlemaps: '.print_r($googlemaps, true));
            return $googlemaps;
        } else {
            $GLOBALS['log']->debug(__METHOD__.' $aInfo: '.print_r($aInfo, true));
            return $aInfo;
        }
    }

    /**
     * Define Maps Address
     *
     * Address Relationship Notes:
     * Account(address)
     * Contact(address)
     * Lead(address)
     * Taget/Prospect(address)
     * Opportunity to Account(address)
     * Case 'account_id' to Account(address)
     *   or Case to Account(address)
     * Project to Account(address)
     *   or Project to Opportunity to Account(address)
     * Meeting - based on Flex Relate
     *
     * @param $object_name  signular object name
     * @param $display      fetched row
     */
    public function defineMapsAddress($object_name, $display)
    {
        $address = false;
        $fields = false;
        $parent = null;
        $parent_type = '';
        $parent_id = '';

        $GLOBALS['log']->debug(__METHOD__.' START');
        $GLOBALS['log']->debug(__METHOD__.' $object_name: '.$object_name);
        $GLOBALS['log']->debug(__METHOD__.' $display: '.print_r($display, true));

        // Field naming is different in some modules.
        // Some modules do not have an address, so a related account needs to be found first.

        if ($object_name == 'Account') {
            $address = $this->defineMapsFormattedAddress($display, $this->settings['geocode_modules_to_address_type']['Accounts']);
        } elseif ($object_name == 'Contact') {
            $address = $this->defineMapsFormattedAddress($display, $this->settings['geocode_modules_to_address_type']['Contacts']);
        } elseif ($object_name == 'Lead') {
            $address = $this->defineMapsFormattedAddress($display, $this->settings['geocode_modules_to_address_type']['Leads']);
        } elseif ($object_name == 'Prospect') {
            $address = $this->defineMapsFormattedAddress($display, $this->settings['geocode_modules_to_address_type']['Prospects']);
        } elseif ($object_name == 'User') {
            $address = $this->defineMapsFormattedAddress($display, $this->settings['geocode_modules_to_address_type']['Users']);
        } elseif ($object_name == 'Opportunity') {

            // Find Account - Assume only one related Account
            $query = "SELECT accounts.*, accounts_cstm.* FROM accounts LEFT JOIN accounts_cstm ON accounts.id = accounts_cstm.id_c " .
                    " LEFT JOIN accounts_opportunities ON accounts.id = accounts_opportunities.account_id AND accounts_opportunities.deleted = 0 " .
                    " WHERE accounts.deleted = 0 AND accounts_opportunities.opportunity_id = '" . $display['id'] . "'";
            $GLOBALS['log']->debug(__METHOD__.' Opportunity to Account');
            $result = $this->db->limitQuery($query, 0, 1);
            $fields = $this->db->fetchByAssoc($result);

            if (!empty($fields)) {
                $address = $this->defineMapsFormattedAddress($fields, $this->settings['geocode_modules_to_address_type']['Opportunities']);
            }
        } elseif (in_array($object_name, array('aCase', 'Case'))) {

            // Find Account from Case (account_id field)

            if (!isset($display['account_id'])) {
                LoggerManager::getLogger()->warn('jjwg_Maps defineMapsAddress: Undefined index: account_id  ');
                $displayAccountId = null;
            } else {
                $displayAccountId = $display['account_id'];
            }

            $query = "SELECT accounts.*, accounts_cstm.* FROM accounts LEFT JOIN accounts_cstm ON accounts.id = accounts_cstm.id_c " .
                    " WHERE accounts.deleted = 0 AND id = '" . $displayAccountId . "'";
            $GLOBALS['log']->debug(__METHOD__.' Case to Account');
            $result = $this->db->limitQuery($query, 0, 1);
            $fields = $this->db->fetchByAssoc($result);

            // If Account is not found; Find many to many Account - Assume only one related Account
            if (empty($fields)) {
                $query = "SELECT accounts.*, accounts_cstm.* FROM accounts LEFT JOIN accounts_cstm ON accounts.id = accounts_cstm.id_c " .
                        " LEFT JOIN accounts_cases ON accounts.id = accounts_cases.account_id AND accounts_cases.deleted = 0 " .
                        " WHERE accounts.deleted = 0 AND accounts_cases.case_id = '" . $display['id'] . "'";
                $GLOBALS['log']->debug(__METHOD__.' Case to Accounts');
                $result = $this->db->limitQuery($query, 0, 1);
                $fields = $this->db->fetchByAssoc($result);
            }

            if (!empty($fields)) {
                $address = $this->defineMapsFormattedAddress($fields, $this->settings['geocode_modules_to_address_type']['Cases']);
            }
        } elseif ($object_name == 'Project') {

            // Check relationship from Project to Account - Assume only one related Account
            $query = "SELECT accounts.*, accounts_cstm.* FROM accounts LEFT JOIN accounts_cstm ON accounts.id = accounts_cstm.id_c " .
                    " LEFT JOIN projects_accounts ON accounts.id = projects_accounts.account_id AND projects_accounts.deleted = 0 " .
                    " WHERE accounts.deleted = 0 AND projects_accounts.project_id = '" . $display['id'] . "'";
            $GLOBALS['log']->debug(__METHOD__.' Project to Account');
            $result = $this->db->limitQuery($query, 0, 1);
            $fields = $this->db->fetchByAssoc($result);

            if (empty($fields)) {
                // Find Opportunity - Assuming that the Project was created from an Opportunity (Closed Won) Detial View
                $query = "SELECT opportunities.*, opportunities_cstm.* FROM opportunities LEFT JOIN opportunities_cstm ON opportunities.id = opportunities_cstm.id_c " .
                        " LEFT JOIN projects_opportunities ON opportunities.id = projects_opportunities.opportunity_id AND projects_opportunities.deleted = 0 " .
                        " WHERE opportunities.deleted = 0 AND projects_opportunities.project_id = '" . $display['id'] . "'";
                $GLOBALS['log']->debug(__METHOD__.' Project to Opportunity');
                $result = $this->db->limitQuery($query, 0, 1);
                $opportunity = $this->db->fetchByAssoc($result);
                if ($opportunity === false) {
                    $result = null;
                    $fields = null;
                } else {
                    // Find Account - Assume only one related Account for the Opportunity
                    $query = "SELECT accounts.*, accounts_cstm.* FROM accounts LEFT JOIN accounts_cstm ON accounts.id = accounts_cstm.id_c " .
                            " LEFT JOIN accounts_opportunities ON accounts.id = accounts_opportunities.account_id AND accounts_opportunities.deleted = 0 " .
                            " WHERE accounts.deleted = 0 AND accounts_opportunities.opportunity_id = '" . $opportunity['id'] . "'";
                    $GLOBALS['log']->debug(__METHOD__.' Opportunity to Account');
                    $result = $this->db->limitQuery($query, 0, 1);
                    $fields = $this->db->fetchByAssoc($result);
                }
            }

            if (!empty($fields)) {
                $address = $this->defineMapsFormattedAddress($fields, $this->settings['geocode_modules_to_address_type']['Project']);
            }
        } elseif ($object_name == 'Meeting') {

            // Find Meeting - Flex Relate Fields: meetings.parent_type and meetings.parent_id
            $query = "SELECT meetings.*, meetings_cstm.* FROM meetings LEFT JOIN meetings_cstm ON meetings.id = meetings_cstm.id_c " .
                    " WHERE meetings.deleted = 0 AND meetings.id = '" . $display['id'] . "'";
            $GLOBALS['log']->debug(__METHOD__.' Meeting');
            $result = $this->db->limitQuery($query, 0, 1);
            $meeting = $this->db->fetchByAssoc($result);

            if ($meeting === false) {
                $parent_type = null;
                $parent_id = null;
            } else {
                $parent_type = $meeting['parent_type'];
                $parent_id = $meeting['parent_id'];
            }
            $GLOBALS['log']->debug(__METHOD__.' Meeting $parent_type: '.$parent_type);
            $GLOBALS['log']->debug(__METHOD__.' Meeting $parent_id: '.$parent_id);

            // If the parent_type is valid module to geocode
            if (array_key_exists($parent_type, $this->settings['valid_geocode_modules'])
                    && !empty($parent_id) && $parent_type != 'Meeting') {

                // Define parent object
                $parent = get_module_info($parent_type);
                $parent->retrieve($parent_id);
                $parent->custom_fields->retrieve();
                $fields = $parent->fetched_row;

                $GLOBALS['log']->debug(__METHOD__.' Meeting $parent->object_name: '.$parent->object_name);
                $GLOBALS['log']->debug(__METHOD__.' Meeting $parent->fetched_row: '.print_r($parent->fetched_row, true));

                // Call this defineMapsAddress for parent which will look at other relationships
                $aInfo = $this->defineMapsAddress($parent->object_name, $parent->fetched_row);
                // return $aInfo
                $GLOBALS['log']->debug(__METHOD__.' Meeting $address Found $aInfo: '.print_r($aInfo, true));
                return $aInfo;
            }
        }


        // If related account address has already been geocoded
        if (!empty($address) && !is_bool($fields) && $fields['jjwg_maps_geocode_status_c'] == 'OK' &&
                !empty($fields['jjwg_maps_lat_c']) && !empty($fields['jjwg_maps_lng_c'])) {
            $aInfo = array(
                'address' => $address,
                'status' => 'OK',
                'lat' => $fields['jjwg_maps_lat_c'],
                'lng' => $fields['jjwg_maps_lng_c']
            );
            $GLOBALS['log']->debug(__METHOD__.' OK Array Found $aInfo: '.print_r($aInfo, true));
            return $aInfo;
        // elseif return address only - if defined
        } elseif (!empty($address)) {
            $aInfo = array(
                'address' => $address,
            );
            $GLOBALS['log']->debug(__METHOD__.' $address Found $aInfo: '.print_r($aInfo, true));
            return $aInfo;
        } else {
            return false;
        }
    }

    /**
     * Define the formatted address line based on address type and field names
     * @param $display bean fields array
     * @param $type type of address: 'billing', 'shipping', 'primary', 'alt', 'custom', 'address'
     */
    public function defineMapsFormattedAddress($display, $type = 'billing')
    {
        $type = strtolower($type);
        if (!in_array($type, array('billing', 'shipping', 'primary', 'alt', 'custom', 'address'))) {
            $type = 'billing';
        }
        $GLOBALS['log']->debug(__METHOD__.' $type: '.print_r($type, true));
        $address_fields = array('billing_address_street', 'billing_address_city', 'billing_address_state', 'billing_address_postalcode', 'billing_address_country');
        $address_parts = array();
        switch ($type) {
            case 'billing':
                $address_fields = array('billing_address_street', 'billing_address_city', 'billing_address_state', 'billing_address_postalcode', 'billing_address_country');
                break;
            case 'shipping':
                $address_fields = array('shipping_address_street', 'shipping_address_city', 'shipping_address_state', 'shipping_address_postalcode', 'shipping_address_country');
                break;
            case 'primary':
                $address_fields = array('primary_address_street', 'primary_address_city', 'primary_address_state', 'primary_address_postalcode', 'primary_address_country');
                break;
            case 'alt':
                $address_fields = array('alt_address_street', 'alt_address_city', 'alt_address_state', 'alt_address_postalcode', 'alt_address_country');
                break;
            case 'address':
                $address_fields = array('address_street', 'address_city', 'address_state', 'address_postalcode', 'address_country');
                break;
        }
        $GLOBALS['log']->debug(__METHOD__.' $address_fields: '.print_r($address_fields, true));
        foreach ($address_fields as $field) {
            if (!isset($display[$field])) {
                $display[$field] = '';
            }
            if (!empty($display[$field])) {
                $address_parts[] = trim($display[$field]);
            }
        }
        if (strlen(implode('', $address_parts)) > 3) {
            $address = implode(', ', $address_parts);
            $address = preg_replace('/[\n\r]+/', ' ', trim($address));
            $address = preg_replace("/[\t\s]+/", ' ', $address);
            $GLOBALS['log']->debug(__METHOD__.' $address: '.print_r($address, true));
            return trim($address);
        } else {
            return false;
        }
    }

    /**
     * Check for valid longitude
     * @param $lng float
     */
    public function is_valid_lng($lng)
    {
        return (is_numeric($lng) && $lng >= -180 && $lng <= 180);
    }

    /**
     * Check for valid latitude
     * @param $lat float
     */
    public function is_valid_lat($lat)
    {
        return (is_numeric($lat) && $lat >= -90 && $lat <= 90);
    }

    /**
     * Bean Log Special
     * This log method filters the $bean into a more readable array
     */
    public function logGeocodeInfo($bean)
    {
        $log_keys = array(
            'jjwg_maps_lat_c', 'jjwg_maps_lng_c', 'jjwg_maps_address_c', 'jjwg_maps_geocode_status_c',
            'fetched_row', 'parent_id', 'parent_type', 'last_parent_id', 'rel_fields_before_value'
        );
        $log_output = array();
        foreach (get_object_vars($bean) as $key=>$value) {
            if (in_array($key, $log_keys)) {
                $log_output[$key] = $value;
            }
        }
        ksort($log_output);
        $GLOBALS['log']->info(__METHOD__.' $log_output: '.print_r($log_output, true));
    }
}



/******************************************************************************/


/**
 * Function for Custom Dropdown of Target Lists
 */

function getProspectLists()
{
    $query = "SELECT id, name, list_type FROM prospect_lists WHERE deleted = 0 ORDER BY name ASC";
    $result = DBManagerFactory::getInstance()->query($query, false);

    $list = array();
    $list['']='';
    while (($row = DBManagerFactory::getInstance()->fetchByAssoc($result)) != null) {
        $list[$row['id']] = $row['name'];
    }

    return $list;
}
