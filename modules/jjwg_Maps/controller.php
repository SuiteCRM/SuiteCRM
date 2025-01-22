<?php

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

// modules/jjwg_Maps/controller.php

require_once('include/utils.php');
require_once('include/export_utils.php');
require_once("include/Sugar_Smarty.php");
require_once('modules/jjwg_Maps/jjwg_Maps.php');

#[\AllowDynamicProperties]
class jjwg_MapsController extends SugarController
{

    /**
     * @var settings array
     */
    public $settings = array();

    /**
     * $map_marker_data_points is used to store temporary data and prevent duplicate points
     * @var array
     */
    public $map_marker_data_points = array();

    /**
     * @var google_maps_response_codes
     *
     */
    public $google_maps_response_codes = array('OK', 'ZERO_RESULTS', 'INVALID_REQUEST', 'OVER_QUERY_LIMIT', 'REQUEST_DENIED');

    /**
     * Last Geocoding Status Message
     * @var string
     */
    public $last_status = '';

    /**
     * display_object - display module's object (dom field)
     * @var object
     */
    public $display_object;

    /**
     * relate_object - relate module's object
     * @var object
     */
    public $relate_object;

    /**
     * jjwg_Maps - Maps module's object
     * @var object
     */
    public $bean;
    public $jjwg_Maps; // Deprecated reference

    /**
     * jjwg_Address_Cache - Address cache module's object
     * @var object
     */
    public $jjwg_Address_Cache;

    /**
     * smarty object for the generic configuration template
     * @var object
     */
    public $sugarSmarty;


    /**
     * Constructor
     */
    public function __construct()
    {
        parent::__construct();
        // Admin Config Setting
        $this->configuration();
    }




    /**
     * Load Configuration Settings using Administration Module
     * See jjwg_Maps module for settings
     *
     * $GLOBALS['jjwg_config_defaults']
     * $GLOBALS['jjwg_config']
     */
    public function configuration()
    {
        $this->bean = BeanFactory::newBean('jjwg_Maps');
        $this->jjwg_Maps = &$this->bean; // Set deprecated reference
        $this->settings = $GLOBALS['jjwg_config'];
    }

    /**
     * action geocoded_counts
     * Google Maps - Geocode the Addresses
     */
    public function action_geocoded_counts()
    {
        $this->view = 'geocoded_counts';
        $GLOBALS['log']->debug(__METHOD__.' START');

        $this->bean->geocoded_counts = array();
        $this->bean->geocoded_headings = array('N/A');
        $this->bean->geocoded_module_totals = array();

        $responses = array('N/A' => '');
        foreach ($this->google_maps_response_codes as $code) {
            if (!in_array($code, array('OVER_QUERY_LIMIT', 'REQUEST_DENIED'))) {
                $responses[$code] = $code;
                $this->bean->geocoded_headings[] = $code;
            }
        }
        $responses['APPROXIMATE'] = 'APPROXIMATE';
        $responses['EMPTY'] = 'EMPTY';
        $this->bean->geocoded_headings[] = 'APPROXIMATE';
        $this->bean->geocoded_headings[] = 'EMPTY';

        // foreach module
        foreach ($this->settings['valid_geocode_modules'] as $module_type) {
            if (!isset($this->bean->geocoded_counts[$module_type])) {
                $this->bean->geocoded_module_totals[$module_type] = 0;
            }

            // Define display object from the necessary classes (utils.php)
            $this->display_object = get_module_info($module_type);

            foreach ($responses as $response => $code) {

                // Create Simple Count Query
                // 09/23/2010: Do not use create_new_list_query() and process_list_query()
                // as they will typically exceeded memory allowed.
                $query = "SELECT count(*) c FROM " . $this->display_object->table_name .
                        " LEFT JOIN " . $this->display_object->table_name . "_cstm " .
                        " ON " . $this->display_object->table_name . ".id = " . $this->display_object->table_name . "_cstm.id_c " .
                        " WHERE " . $this->display_object->table_name . ".deleted = 0 AND ";
                if ($response == 'N/A') {
                    $query .= "(" . $this->display_object->table_name . "_cstm.jjwg_maps_geocode_status_c = '' OR " .
                            $this->display_object->table_name . "_cstm.jjwg_maps_geocode_status_c IS NULL)";
                } else {
                    $query .= $this->display_object->table_name . "_cstm.jjwg_maps_geocode_status_c = '" . $code . "'";
                }
                //var_dump($query);
                $count_result = $this->bean->db->query($query);
                $count = $this->bean->db->fetchByAssoc($count_result);
                if (empty($count)) {
                    $count['c'] = 0;
                }
                $this->bean->geocoded_counts[$module_type][$response] = $count['c'];
            } // end foreach response type
            // Get Totals
            $this->bean->geocoded_module_totals[$module_type]++;
            $query = "SELECT count(*) c FROM " . $this->display_object->table_name . " WHERE " . $this->display_object->table_name . ".deleted = 0";
            //var_dump($query);
            $count_result = $this->bean->db->query($query);
            $count = $this->bean->db->fetchByAssoc($count_result);
            $this->bean->geocoded_module_totals[$module_type] = $count['c'];
        } // end each module type
    }

    /**
     * action geocode_addresses
     * Google Maps - Geocode the Addresses
     */
    public function action_geocode_addresses()
    {
        $GLOBALS['log']->debug(__METHOD__.' START');

        if (!empty($_REQUEST['display_module']) && in_array($_REQUEST['display_module'], $this->settings['valid_geocode_modules'])) {
            $geocode_modules = array($_REQUEST['display_module']);
        } else {
            $geocode_modules = $this->settings['valid_geocode_modules'];
        }
        $geocoding_inc = 0;
        $google_geocoding_inc = 0;
        // Define Address Cache Object
        $this->jjwg_Address_Cache = get_module_info('jjwg_Address_Cache');


        foreach ($geocode_modules as $module_type) {
            $GLOBALS['log']->debug(__METHOD__.' $module_type: '.$module_type);
            // Define display object from the necessary classes (utils.php)
            $this->display_object = get_module_info($module_type);

            // Find the Items to Geocode - Get Geocode Addresses Result
            $display_result = $this->bean->getGeocodeAddressesResult($this->display_object->table_name);

            /*
             * Iterate through the display rows
             * We build up an array here to prevent locking issues on some DBs (looking at you MSSQL)
             */
            $tmpDisplayResults = array();
            while ($display = $this->bean->db->fetchByAssoc($display_result)) {
                $tmpDisplayResults[] = $display;
            }
            foreach ($tmpDisplayResults as $display) {
                $GLOBALS['log']->debug(__METHOD__.' $display[\'id\': '.$display['id']);
                $geocoding_inc++;
                $aInfo = array();
                $cache_found = false;

                // Get address info array (address, status, lat, lng) from defineMapsAddress()
                // This will provide a related address & optionally a status, lat and lng from an account or other object
                $aInfo = $this->bean->defineMapsAddress($this->display_object->object_name, $display);
                //var_dump($aInfo);

                // Call Controller Method to Define Custom Address Logic
                $aInfo = $this->defineMapsAddressCustom($aInfo, $this->display_object->object_name, $display);
                //var_dump($aInfo);

                // If needed, check the Address Cache Module for Geocode Info
                if (!empty($aInfo['address']) && is_object($this->jjwg_Address_Cache)) {
                    $aInfoCache = $this->jjwg_Address_Cache->getAddressCacheInfo($aInfo);
                    if (!empty($aInfoCache['address'])) {
                        $cache_found = true;
                        $aInfo = $aInfoCache;
                    }
                }

                // If needed, Google Maps V3. Geocode the current address (status not set)
                if (!empty($aInfo['address']) && empty($aInfo['status'])) {
                    // Limit Geocode Requests to Google based on $this->settings['google_geocoding_limit']
                    if ($google_geocoding_inc < $this->settings['google_geocoding_limit']) {
                        $aInfoGoogle = $this->bean->getGoogleMapsGeocode($aInfo['address'], false, false);
                        if (!empty($aInfoGoogle)) {
                            $aInfo = $aInfoGoogle;
                            // Set last status
                            $this->last_status = $aInfo['status'];
                        }
                        $google_geocoding_inc++;
                    }
                }

                if (empty($aInfo['status'])) {
                    $aInfo['status'] = '';
                }
                if (empty($aInfo['lat']) || !is_numeric($aInfo['lat'])) {
                    $aInfo['lat'] = 0;
                }
                if (empty($aInfo['lng']) || !is_numeric($aInfo['lng'])) {
                    $aInfo['lng'] = 0;
                }

                // Successful geocode
                // 'OK' Status
                if (!empty($aInfo['address']) && $aInfo['status'] == 'OK' &&
                        !($aInfo['lng'] == 0 && $aInfo['lat'] == 0)) {

                    // Save Geocode $aInfo to custom fields
                    $update_result = $this->bean->updateGeocodeInfoByAssocQuery($this->display_object->table_name, $display, $aInfo);

                    // Save address, lng and lat to cache module - if not already found from cache
                    if (!$cache_found) {
                        $cache_save_result = $this->jjwg_Address_Cache->saveAddressCacheInfo($aInfo);
                    }

                    // Bad Geocode Results - Recorded
                // Empty Address - indicates no address, no geocode response
                // 'ZERO_RESULTS' - indicates that the geocode was successful but returned no results.
                //     This may occur if the geocode was passed a non-existent address.
                // 'INVALID_REQUEST' - generally indicates that the query (address) is missing.
                // Also, capture empty $aInfo or address.
                } elseif (empty($aInfo) || empty($aInfo['address']) || (!empty($aInfo['address']) &&
                        ($aInfo['status'] == 'ZERO_RESULTS' || $aInfo['status'] == 'INVALID_REQUEST' ||
                        $aInfo['status'] == 'APPROXIMATE'))) {
                    if (empty($aInfo['status'])) {
                        $aInfo['status'] = 'Empty';
                    }
                    // Save Geocode $aInfo to custom fields
                    $update_result = $this->bean->updateGeocodeInfoByAssocQuery($this->display_object->table_name, $display, $aInfo);

                // Bad Geocode Results - Stop
                // 'OVER_QUERY_LIMIT' - indicates that you are over your quota.
                // 'REQUEST_DENIED' - indicates that your request was denied, generally because of lack of a sensor parameter.
                } elseif (!empty($aInfo['address']) &&
                        ($aInfo['status'] == 'OVER_QUERY_LIMIT' || $aInfo['status'] == 'REQUEST_DENIED')) {

                    // Set above limit to break/stop processing
                    $geocoding_inc = $this->settings['geocoding_limit'] + 1;
                } // end if/else

                // Wait 1 Second to Throttle Requests: Rate limit of 10 geocodings per second
                if ($geocoding_inc % 10 == 0) {
                    sleep(1);
                }

                if ($geocoding_inc > $this->settings['geocoding_limit']) {
                    break;
                }
            } // while

            if ($geocoding_inc > $this->settings['geocoding_limit']) {
                break;
            }
        } // end each module type

        // If not cron processing, then redirect.
        if (!isset($_REQUEST['cron'])) {
            // Redirect to the Geocoded Counts Display
            // contains header and exit
            $url = 'index.php?module=jjwg_Maps&action=geocoded_counts';
            if (!empty($this->last_status)) {
                $url .= '&last_status=' . urlencode($this->last_status);
            }
            SugarApplication::redirect($url);
        }
    }


    /**
     *  Add a number of display_module objects to a target list
     *  Return JSON encoded result count
     */
    public function action_add_to_target_list()
    {
        $result = array('post' => $_POST);

        // Target List
        $list_id = (!empty($_POST['list_id'])) ? $_POST['list_id'] : '';
        /** @var ProspectList $list */
        $list = get_module_info('ProspectLists');
        if (!empty($list_id) && is_guid($list_id)) {
            $list->retrieve($list_id);
            $result['list']['name'] = $list->name;
        }
        // Selected Area IDs - Validate
        if (is_string($_POST['selected_ids'])) {
            $_POST['selected_ids'] = explode(',', $_POST['selected_ids']);
        }
        $selected_ids = array();
        foreach ($_POST['selected_ids'] as $sel_id) {
            if (is_guid($sel_id)) {
                $selected_ids[] = $sel_id;
            }
        }
        $result['selected_ids'] = $selected_ids;

        // Display Module Type
        $module_type = '';
        if (!empty($_POST['display_module']) && in_array($_POST['display_module'], $this->settings['valid_geocode_modules'])) {
            $module_type = $_POST['display_module'];
            $result['module_type'] = $module_type;
            // Define display object
            $this->display_object = get_module_info($module_type);
        }

        if (!empty($list) && $list_id == $list->id && !empty($selected_ids) && !empty($this->display_object) &&
                in_array($this->display_object->module_name, array('Accounts', 'Contacts', 'Leads', 'Prospects', 'Users'))) {
            $object_name = $this->display_object->object_name;
            $result['object_name'] = $object_name;

            if ($object_name == 'Account') {
                $list->load_relationship('accounts');
                foreach ($selected_ids as $sel_id) {
                    $list->accounts->add($sel_id);
                }
            } elseif ($object_name == 'Contact') {
                $list->load_relationship('contacts');
                foreach ($selected_ids as $sel_id) {
                    $list->contacts->add($sel_id);
                }
            } elseif ($object_name == 'Lead') {
                $list->load_relationship('leads');
                foreach ($selected_ids as $sel_id) {
                    $list->leads->add($sel_id);
                }
            } elseif ($object_name == 'Prospect') {
                $list->load_relationship('prospects');
                foreach ($selected_ids as $sel_id) {
                    $list->prospects->add($sel_id);
                }
            } elseif ($object_name == 'User') {
                $list->load_relationship('users');
                foreach ($selected_ids as $sel_id) {
                    $list->users->add($sel_id);
                }
            }
            $result['message'] = 'Target List Updated';
        } else {
            $result['message'] = 'Target List NOT Updated';
        }

        // JSON Encoded $result
        $out = [];
        $out['message'] = $result['message'];
        $out['list']['name'] = $result['object_name'];
        header('Content-Type: application/json');
        echo @json_encode($out);
    }

    /**
     * export addresses in need of geocoding
     */
    public function action_export_geocoding_addresses()
    {
        $address_data = array();
        $addresses = array();

        if (!empty($_REQUEST['display_module']) && in_array($_REQUEST['display_module'], $this->settings['valid_geocode_modules'])) {
            $module_type = $_REQUEST['display_module'];
        } else {
            $module_type = $this->settings['valid_geocode_modules'][0];
        }
        // Define display object
        $this->display_object = get_module_info($module_type);

        // Find the Items to Geocode - Get Geocode Addresses Result
        $display_result = $this->bean->getGeocodeAddressesResult($this->display_object->table_name, $this->settings['export_addresses_limit']);

        $address_data[] = array('address', 'lat', 'lng');
        // Iterate through the display rows
        while ($display = $this->bean->db->fetchByAssoc($display_result)) {

            // Get address info array (address, status, lat, lng) from defineMapsAddress()
            // This will provide a related address & optionally a status, lat and lng from an account or other object
            $aInfo = $this->bean->defineMapsAddress($this->display_object->object_name, $display);
            //var_dump($aInfo);

            // Call Method to Define Custom Address Logic
            $aInfo = $this->defineMapsAddressCustom($aInfo, $this->display_object->object_name, $display);
            //var_dump($aInfo);

            if (!empty($aInfo['address'])) {
                $addresses[] = trim($aInfo['address'], ' ,;."\'');
            }
        }

        $addresses = array_unique($addresses);
        foreach ($addresses as $address) {
            $address_data[] = array($address, '', '');
        }

        $filename = $module_type . '_Addresses_' . date("Ymd") . ".csv";
        $this->do_list_csv_output($address_data, $filename);
        exit;
    }

    /**
     * Custom Override for Defining Maps Address
     *
     * @param $aInfo        address info array(address, status, lat, lng)
     * @param $object_name  signular object name
     * @param $display      fetched row array
     */
    public function defineMapsAddressCustom($aInfo, $object_name, $display)
    {

        // Use custom contoller.php with custom logic
        return $aInfo;
    }

    /**
     *
     * Export rows of data as a CSV file
     * @param unknown_type $rows
     * @param unknown_type $filename
     */
    private function do_list_csv_output($rows, $filename)
    {
        header("Content-type: application/octet-stream");
        header("Content-Disposition: attachment; filename=\"$filename\"");
        header("Content-Transfer-Encoding: binary");
        if (strpos((string) $_SERVER['HTTP_USER_AGENT'], 'MSIE')) {
            // IE cannot download from sessions without a cache
            header('Cache-Control: public');
        }
        foreach (array_keys($rows) as $key) {
            $row = $rows[$key];
            echo $this->list_row_to_csv($row);
        }
    }

    /**
     *
     * Create CSV row for export view
     * @param $fields name value pairs
     * @param $delimiter
     * @param $enclosure
     */
    private function list_row_to_csv($fields, $delimiter = ',', $enclosure = '"')
    {
        $delimiter_esc = preg_quote((string) $delimiter, '/');
        $enclosure_esc = preg_quote((string) $enclosure, '/');
        $output = array();
        foreach ($fields as $field) {
            $output[] = preg_match("/(?:{$delimiter_esc}|{$enclosure_esc}|\s)/", (string) $field) ? (
                $enclosure . str_replace($enclosure, $enclosure . $enclosure, (string) $field) . $enclosure
                    ) : $field;
        }

        return (implode($delimiter, $output) . "\n");
    }

    /**
     * action geocoding_test
     * Google Maps - Geocoding Test
     */
    public function action_geocoding_test()
    {
        $this->view = 'geocoding_test';

        if (!empty($_REQUEST['geocoding_address']) && !empty($_REQUEST['process_trigger']) &&
                strlen((string) $_REQUEST['geocoding_address']) <= 255) {
            $this->bean->geocoding_results = $this->bean->getGoogleMapsGeocode($_REQUEST['geocoding_address'], true, true);
        }
    }

    /**
     * action config
     * Google Maps - Config
     */
    public function action_config()
    {

        // Admin Only
        if (!empty($GLOBALS['current_user']->is_admin)) {
            if (!empty($_REQUEST['submit'])) {
                // Post-Get-Redirect
                $save_result = $this->bean->saveConfiguration($_REQUEST);
                $config_save_notice = ($save_result == true) ? '1' : '0';
                SugarApplication::redirect('index.php?module=jjwg_Maps&action=config&config_save_notice='.$config_save_notice);
            } else {
                $this->view = 'config';
            }
        } else {
            SugarApplication::redirect('index.php?module=jjwg_Maps&action=index');
        }
    }

    /**
     * action reset module geocode info
     * Google Maps - geocoded_counts
     */
    public function action_reset_geocoding()
    {
        $display_module = $_REQUEST['display_module'];

        // Define display object from the necessary classes (utils.php)
        $this->display_object = get_module_info($display_module);

        // Admin Only
        if (!empty($GLOBALS['current_user']->is_admin)) {
            if (is_object($this->display_object)) {
                $delete_result = $this->bean->deleteAllGeocodeInfoByBeanQuery($this->display_object);
                SugarApplication::redirect('index.php?module=jjwg_Maps&action=geocoded_counts');
            } else {
                $this->view = 'geocoded_counts';
            }
        } else {
            SugarApplication::redirect('index.php?module=jjwg_Maps&action=index');
        }
    }

    /**
     * delete all address cache
     * Google Maps - geocoded_counts
     */
    public function action_delete_all_address_cache()
    {

        // Define Address Cache Object
        $this->jjwg_Address_Cache = get_module_info('jjwg_Address_Cache');

        // Admin Only
        if (!empty($GLOBALS['current_user']->is_admin)) {
            if (is_object($this->jjwg_Address_Cache)) {
                // Post-Get-Redirect
                $delete_result = $this->jjwg_Address_Cache->deleteAllAddressCache();
                SugarApplication::redirect('index.php?module=jjwg_Maps&action=geocoded_counts');
            } else {
                $this->view = 'geocoded_counts';
            }
        } else {
            SugarApplication::redirect('index.php?module=jjwg_Maps&action=index');
        }
    }

    /**
     * action quick_radius
     * Google Maps - Quick Radius Map
     */
    public function action_quick_radius()
    {
        $this->view = 'quick_radius';

        if (!isset($_REQUEST['distance'])) {
            $_REQUEST['distance'] = $this->settings['map_default_distance'];
        }
        if (!isset($_REQUEST['unit_type'])) {
            $_REQUEST['unit_type'] = $this->settings['map_default_unit_type'];
        }
    }

    /**
     * action map_display
     * Google Maps - Output the Page with IFrame to Map Markers
     */
    public function action_quick_radius_display()
    {
        $this->view = 'quick_radius_display';
    }

    /**
     * action map_display
     * Google Maps - Output the Page with IFrame to Map Markers
     */
    public function action_map_display()
    {
        $this->view = 'map_display';
        if (!isset($_REQUEST['current_post'])) {
            $_REQUEST['current_post'] = '';
        }

        // Bug: 'current_post' too large for iFrame URL used in Google Library calls
        $_SESSION['jjwg_Maps']['current_post'] = $_REQUEST['current_post'];
        $_REQUEST['current_post'] = 'session';
    }

    /**
     * action donate
     * Google Maps - Output the Donate Page
     */
    public function action_donate()
    {
        $this->view = 'donate';
    }

    /**
     * action map_markers
     * Google Maps - Output the Map Markers
     */
    public function action_map_markers()
    {
        $map_module_type = '';
        $map_unit_type = 'mi';
        $map_distance = 250;
        $map = null;
        $search_array = [];
        header_remove('X-Frame-Options');
        $this->view = 'map_markers';

        // Define globals for use in the view.
        $this->bean->map_center = array();
        $this->bean->map_markers = array();
        $this->bean->map_markers_groups = array();
        $this->bean->custom_markers = array();
        $this->bean->custom_areas = array();

        // Create New Sugar_Smarty Object
        $this->sugarSmarty = new Sugar_Smarty();
        $this->sugarSmarty->assign("mod_strings", $GLOBALS['mod_strings']);
        $this->sugarSmarty->assign("app_strings", $GLOBALS['app_strings']);
        $this->sugarSmarty->assign('app_list_strings', $GLOBALS['app_list_strings']);
        $this->sugarSmarty->assign('moduleListSingular', $GLOBALS['app_list_strings']['moduleListSingular']);
        $this->sugarSmarty->assign('moduleList', $GLOBALS['app_list_strings']['moduleList']);
        //echo '<pre>';
        //var_dump($_REQUEST);

        // Related Map Record Defines the Map
        if (!empty($_REQUEST['record']) ||
                (!empty($_REQUEST['relate_id']) && !empty($_REQUEST['relate_module'])) ||
                (!empty($_REQUEST['quick_address']) && !empty($_REQUEST['display_module']))) {

            // If map 'record' then define map details from current module.
            if (@is_guid($_REQUEST['record'])) {
                // Get the map object
                $map = get_module_info($GLOBALS['currentModule']);
                $map->retrieve($_REQUEST['record']);
                // Define map variables
                $map_parent_type = $map->parent_type;
                $map_parent_id = $map->parent_id;
                $map_module_type = $map->module_type;
                $map_unit_type = $map->unit_type;
                $map_distance = $map->distance;
            }
            // Else if a 'relate_id' use it as the Relate Center Point (Lng/Lat)
            else {
                if (@(is_guid($_REQUEST['relate_id']) && !empty($_REQUEST['relate_module']))) {
                    // Define map variables
                    $map_parent_type = $_REQUEST['relate_module'];
                    $map_parent_id = $_REQUEST['relate_id'];
                    $map_module_type = (!empty($_REQUEST['display_module'])) ? $_REQUEST['display_module'] : $_REQUEST['relate_module'];
                    $map_distance = (!empty($_REQUEST['distance'])) ? $_REQUEST['distance'] : $this->settings['map_default_distance'];
                    $map_unit_type = (!empty($_REQUEST['unit_type'])) ? $_REQUEST['unit_type'] : $this->settings['map_default_unit_type'];
                }
                // Else if a 'quick_address' use it as the Center Point (Lng/Lat)
                else {
                    if (!empty($_REQUEST['quick_address']) && !empty($_REQUEST['display_module'])) {
                        // Define map variables / No Parent
                        $map_parent_type = null;
                        $map_parent_id = null;
                        $map_module_type = (!empty($_REQUEST['display_module'])) ? $_REQUEST['display_module'] : $_REQUEST['relate_module'];
                        $map_distance = (!empty($_REQUEST['distance'])) ? $_REQUEST['distance'] : $this->settings['map_default_distance'];
                        $map_unit_type = (!empty($_REQUEST['unit_type'])) ? $_REQUEST['unit_type'] : $this->settings['map_default_unit_type'];
                    }
                }
            }

            // Define display object, note - 'Accounts_Members' is a special display type
            $this->display_object = ($map_module_type == 'Accounts_Members') ? get_module_info('Accounts') : get_module_info($map_module_type);
            $mod_strings_display = return_module_language($GLOBALS['current_language'], $this->display_object->module_name);
            $mod_strings_display = array_merge($mod_strings_display, $GLOBALS['mod_strings']);

            // If relate module/id object
            if (!empty($map_parent_type) && !empty($map_parent_id)) {

                // Define relate objects
                $this->relate_object = get_module_info($map_parent_type);
                $this->relate_object->retrieve($map_parent_id);
                $mod_strings_related = return_module_language($GLOBALS['current_language'], $this->relate_object->module_name);
                $mod_strings_related = array_merge($mod_strings_related, $GLOBALS['mod_strings']);

                // Get the Relate object Assoc Data
                $where_conds = $this->relate_object->table_name . ".id = '" . $map_parent_id . "'";
                $query = $this->relate_object->create_new_list_query("" . $this->relate_object->table_name . ".assigned_user_id", $where_conds, array(), array(), 0, '', false, $this->relate_object, false);
                //var_dump($query);
                $relate_result = $this->bean->db->query($query);
                $relate = $this->bean->db->fetchByAssoc($relate_result);
                // Add Relate (Center Point) Marker
                $this->bean->map_center = $this->getMarkerData($map_parent_type, $relate, true, $mod_strings_related);
                // Define Center Point
                $center_lat = $this->relate_object->jjwg_maps_lat_c;
                $center_lng = $this->relate_object->jjwg_maps_lng_c;
            }
            // Use Quick Address as Center Point
            else {
                // Geocode 'quick_address'
                $aInfo = $this->bean->getGoogleMapsGeocode($_REQUEST['quick_address'], false, true);
                // If not status 'OK', then fail here and exit. Note: Inside of iFrame
                if (!empty($aInfo['status']) && $aInfo['status'] != 'OK' && preg_match('/[A-Z\_]/', (string) $aInfo['status'])) {
                    echo '<br /><br /><div><b>'.$GLOBALS['mod_strings']['LBL_MAP_LAST_STATUS'].': '.$aInfo['status'].'</b></div><br /><br />';
                    exit;
                }
                //var_dump($aInfo);
                // Define Marker Data
                $aInfo['name'] = $_REQUEST['quick_address'];
                $aInfo['id'] = 0;
                $aInfo['module'] = ($map_module_type == 'Accounts_Members') ? 'Accounts' : $map_module_type;
                $aInfo['address'] = $_REQUEST['quick_address'];
                $aInfo['jjwg_maps_address_c'] = $_REQUEST['quick_address'];
                $aInfo['jjwg_maps_lat_c'] = $aInfo['lat'];
                $aInfo['jjwg_maps_lng_c'] = $aInfo['lng'];
                $this->bean->map_center = $this->getMarkerData($map_parent_type, $aInfo, true);
                // Define Center Point
                $center_lat = $aInfo['lat'];
                $center_lng = $aInfo['lng'];
            }
            //var_dump($aInfo);
            // Define $x and $y expressions
            $x = '(69.1*((' . $this->display_object->table_name . '_cstm.jjwg_maps_lat_c)-(' . $center_lat . ')))';
            $y = '(53.0*((' . $this->display_object->table_name . '_cstm.jjwg_maps_lng_c)-(' . $center_lng . ')) * COS((' . $center_lat . ')/57.1))';
            $calc_distance_expression = 'SQRT(' . $x . '*' . $x . '+' . $y . '*' . $y . ')';
            if (strtolower($map_unit_type) == 'km' || strtolower($map_unit_type) == 'kilometer') {
                $calc_distance_expression .= '*1.609'; // 1 mile = 1.609 km
            }

            // Find the Items to Display
            // Assume there is no address at 0,0; it's in the Atlantic Ocean!
            $where_conds = "(" . $this->display_object->table_name . "_cstm.jjwg_maps_lat_c != 0 OR " .
                    "" . $this->display_object->table_name . "_cstm.jjwg_maps_lng_c != 0) " .
                    " AND " .
                    "(" . $this->display_object->table_name . "_cstm.jjwg_maps_geocode_status_c = 'OK')" .
                    " AND " .
                    "(" . $calc_distance_expression . " < " . $map_distance . ")";
            $query = $this->display_object->create_new_list_query('display_object_distance', $where_conds, array(), array(), 0, '', false, $this->display_object, false);
            // Add the disply_object_distance into SELECT list
            $query = str_replace('SELECT ', 'SELECT (' . $calc_distance_expression . ') AS display_object_distance, ', (string) $query);
            if ($map_module_type == 'Contacts') { // Contacts - Account Name
                $query = str_replace(' FROM contacts ', ' ,accounts.name AS account_name, accounts.id AS account_id  FROM contacts  ', $query);
                $query = str_replace(' FROM contacts ', ' FROM contacts LEFT JOIN accounts_contacts ON contacts.id=accounts_contacts.contact_id and accounts_contacts.deleted = 0 LEFT JOIN accounts ON accounts_contacts.account_id=accounts.id AND accounts.deleted=0 ', $query);
            } elseif ($map_module_type == 'Opportunities') { // Opps - Account Name
                $query = str_replace(' FROM opportunities ', ' ,accounts.name AS account_name, accounts.id AS account_id  FROM opportunities  ', $query);
                $query = str_replace(' FROM opportunities ', ' FROM opportunities LEFT JOIN accounts_opportunities ON opportunities.id=accounts_opportunities.opportunity_id and accounts_opportunities.deleted = 0 LEFT JOIN accounts ON accounts_opportunities.account_id=accounts.id AND accounts.deleted=0 ', $query);
            } elseif ($map_module_type == 'Accounts_Members') { // 'Accounts_Members' is a special display type
                $query = str_replace(' AND accounts.deleted=0', ' AND accounts.deleted=0 AND accounts.parent_id = \''.$this->bean->db->quote($map_parent_id).'\'', $query);
            }
            //var_dump($query);
            $display_result = $this->bean->db->limitQuery($query, 0, $this->settings['map_markers_limit']);
            while ($display = $this->bean->db->fetchByAssoc($display_result)) {
                if (!empty($map_distance) && !empty($display['id'])) {
                    $marker_data_module_type = ($map_module_type == 'Accounts_Members') ? 'Accounts' : $map_module_type;
                    $marker_data = $this->getMarkerData($marker_data_module_type, $display, false, $mod_strings_display);
                    if (!empty($marker_data)) {
                        $this->bean->map_markers[] = $marker_data;
                    }
                }
            }
            //var_dump($this->bean->map_markers);
            // Next define the Custom Markers and Areas related to this Map
            // Define relate and display objects from the necessary classes (utils.php)
            @$markers_object = get_module_info('jjwg_Markers');
            @$areas_object = get_module_info('jjwg_Areas');

            // Relationship Names: jjwg_maps_jjwg_areas and jjwg_maps_jjwg_markers
            // Find the Related Beans: Maps to Markers
            if (@(is_object($markers_object) && is_object($map))) {
                $related_custom_markers = $map->get_linked_beans('jjwg_maps_jjwg_markers', 'jjwg_Markers');
                if ($related_custom_markers) {
                    foreach ($related_custom_markers as $marker_bean) {
                        $marker_data = $this->getMarkerDataCustom($marker_bean);
                        if (!empty($marker_data)) {
                            $this->bean->custom_markers[] = $marker_data;
                        }
                    }
                }
            }

            // Find the Related Beans: Maps to Areas
            if (@(is_object($areas_object) && is_object($map))) {
                $related_custom_areas = $map->get_linked_beans('jjwg_maps_jjwg_areas', 'jjwg_Areas');
                if ($related_custom_areas) {
                    foreach ($related_custom_areas as $area_bean) {
                        $area_data = $this->getAreaDataCustom($area_bean);
                        if (!empty($area_data)) {
                            $this->bean->custom_areas[] = $area_data;
                        }
                    }
                }
            }


            // Map Target List (ProspectLists)
        } elseif (!empty($_REQUEST['list_id'])) {
            $this->bean->map_markers = array();
            $this->display_object = get_module_info('ProspectLists');
            // Use the Export Query
            if (!empty($_REQUEST['list_id'])) {
                $this->display_object->retrieve($_REQUEST['list_id']);
                if ($this->display_object->id == $_REQUEST['list_id']) {
                    $prospect_list_object = $this->display_object;
                    $list_id = $this->display_object->id;
                }
            }

            if (!empty($list_id)) {
                $list_modules = array('Accounts', 'Contacts', 'Leads', 'Users', 'Prospects');
                $temp_marker_groups = array();

                foreach ($list_modules as $display_module) {
                    $this->display_object = get_module_info($display_module);
                    $mod_strings_display = return_module_language($GLOBALS['current_language'], $this->display_object->module_name);
                    $mod_strings_display = array_merge($mod_strings_display, $GLOBALS['mod_strings']);

                    // Find the Items to Display
                    // Assume there is no address at 0,0; it's in the Atlantic Ocean!
                    $where_conds = "(" . $this->display_object->table_name . "_cstm.jjwg_maps_lat_c != 0 OR " .
                            "" . $this->display_object->table_name . "_cstm.jjwg_maps_lng_c != 0) " .
                            " AND " .
                            "(" . $this->display_object->table_name . "_cstm.jjwg_maps_geocode_status_c = 'OK')";
                    $query = $this->display_object->create_new_list_query('', $where_conds, array(), array(), 0, '', false, $this->display_object, false);
                    if ($display_module == 'Contacts') { // Contacts - Account Name
                        $query = str_replace(' FROM contacts ', ' ,accounts.name AS account_name, accounts.id AS account_id  FROM contacts  ', (string) $query);
                        $query = str_replace(' FROM contacts ', ' FROM contacts LEFT JOIN accounts_contacts ON contacts.id=accounts_contacts.contact_id and accounts_contacts.deleted = 0 LEFT JOIN accounts ON accounts_contacts.account_id=accounts.id AND accounts.deleted=0 ', $query);
                    }
                    // Add List JOIN
                    $query = str_replace(
                        ' FROM '.$this->display_object->table_name.' ',
                        ' FROM '.$this->display_object->table_name.' '.
                            'LEFT JOIN prospect_lists_prospects ON prospect_lists_prospects.related_id = '.$this->display_object->table_name.'.id AND prospect_lists_prospects.deleted=0 '.
                            'LEFT JOIN prospect_lists ON prospect_lists_prospects.prospect_list_id = prospect_lists.id AND prospect_lists.deleted=0 ',
                        (string) $query
                    );
                    // Restrict WHERE to related type and $list_id
                    $query .= ' AND prospect_lists_prospects.related_type = \''.$this->display_object->module_name.'\' AND '.
                            'prospect_lists.id = \''.$this->bean->db->quote($list_id).'\'';
                    //var_dump($query);
                    $display_result = $this->bean->db->limitQuery($query, 0, $this->settings['map_markers_limit']);
                    $display_type_found = false;
                    while ($display = $this->bean->db->fetchByAssoc($display_result)) {
                        if (!empty($display['id'])) {
                            $marker_data = $this->getMarkerData($display_module, $display, false, $mod_strings_display);
                            $marker_data['group'] = $GLOBALS['app_list_strings']['moduleList'][$display_module];
                            if (!empty($marker_data)) {
                                $this->bean->map_markers[] = $marker_data;
                            }
                            $display_type_found = true;
                        }
                    }
                    if ($display_type_found) {
                        $temp_marker_groups[] = $GLOBALS['app_list_strings']['moduleList'][$display_module];
                    }
                }

                $this->bean->map_markers_groups = $temp_marker_groups;
            }


            // Map Records
        } elseif (!empty($_REQUEST['uid']) || !empty($_REQUEST['current_post'])) {
            if (in_array($_REQUEST['display_module'], $this->settings['valid_geocode_modules'])) {
                $display_module = $_REQUEST['display_module'];
            } else {
                $display_module = 'Accounts';
            }
            if ($_REQUEST['current_post'] == 'session') {
                $current_post = $_SESSION['jjwg_Maps']['current_post'];
            } else {
                $current_post = $_REQUEST['current_post'];
            }
            $query = '';
            $selected_query = '';
            $records = array();
            $order_by = '';

            $this->display_object = get_module_info($display_module);
            $mod_strings_display = return_module_language($GLOBALS['current_language'], $this->display_object->module_name);
            $mod_strings_display = array_merge($mod_strings_display, $GLOBALS['mod_strings']);

            if (!empty($_REQUEST['uid'])) {
                // Several records selected or this page
                $records = explode(',', $_REQUEST['uid']);
            } elseif (!empty($current_post)) {
                // Select all records (advanced search)
                $search_array = generateSearchWhere($display_module, $current_post);
                //var_dump($search_array);
                if (!empty($search_array['where'])) {
                    // Related Field Bug: Get relate/link patched 'where' and 'join'
                    @$ret_array = create_export_query_relate_link_patch($display_module, $search_array['searchFields'], $search_array['where']);
                    if (!empty($ret_array['join'])) {
                        @$selected_query = $this->display_object->create_export_query($order_by, $ret_array['where'], $ret_array['join']);
                    } else {
                        @$selected_query = $this->display_object->create_export_query($order_by, $ret_array['where']);
                    }
                    // SugarOnDemand JOIN Bug: If $ret_array['join'] is not included in query, force it in!
                    if (strpos((string) $ret_array['join'], (string) $selected_query) === false) {
                        $selected_query = str_replace(' where ', $ret_array['join'].' where ', (string) $selected_query);
                    }
                    // Avoiding subquery. Let's just record the record ID's for later
                    $selected_result = $this->bean->db->limitQuery($selected_query, 0, $this->settings['map_markers_limit']);
                    while ($display = $this->bean->db->fetchByAssoc($selected_result)) {
                        $records[] = $display['id'];
                    }
                }
            }
            //var_dump($records);

            // Find the Items to Display
            // Assume there is no address at 0,0; it's in the Atlantic Ocean!
            $where_conds = "(" . $this->display_object->table_name . "_cstm.jjwg_maps_lat_c != 0 OR " .
                    "" . $this->display_object->table_name . "_cstm.jjwg_maps_lng_c != 0) " .
                    " AND " .
                    "(" . $this->display_object->table_name . "_cstm.jjwg_maps_geocode_status_c = 'OK')";
            $query = $this->display_object->create_new_list_query('', $where_conds, array(), array(), 0, '', false, $this->display_object, false);
            if ($display_module == 'Contacts') { // Contacts - Account Name
                $query = str_replace(' FROM contacts ', ' ,accounts.name AS account_name, accounts.id AS account_id  FROM contacts  ', (string) $query);
                $query = str_replace(' FROM contacts ', ' FROM contacts LEFT JOIN accounts_contacts ON contacts.id=accounts_contacts.contact_id and accounts_contacts.deleted = 0 LEFT JOIN accounts ON accounts_contacts.account_id=accounts.id AND accounts.deleted=0 ', $query);
            } elseif ($display_module == 'Opportunities') { // Opps - Account Name
                $query = str_replace(' FROM opportunities ', ' ,accounts.name AS account_name, accounts.id AS account_id  FROM opportunities  ', (string) $query);
                $query = str_replace(' FROM opportunities ', ' FROM opportunities LEFT JOIN accounts_opportunities ON opportunities.id=accounts_opportunities.opportunity_id and accounts_opportunities.deleted = 0 LEFT JOIN accounts ON accounts_opportunities.account_id=accounts.id AND accounts.deleted=0 ', $query);
            }
            //var_dump($query);

            $display_result = $this->bean->db->limitQuery($query, 0, $this->settings['map_markers_limit']);
            $this->bean->map_markers = array();
            while ($display = $this->bean->db->fetchByAssoc($display_result)) {
                if (!empty($search_array['where'])) { // Select all records (advanced search) with where clause
                    if (in_array($display['id'], $records)) {
                        $this->bean->map_markers[] = $this->getMarkerData($display_module, $display, false, $mod_strings_display);
                    }
                } elseif (!empty($_REQUEST['uid'])) { // Several records selected or this page selected
                    if (in_array($display['id'], $records)) {
                        $this->bean->map_markers[] = $this->getMarkerData($display_module, $display, false, $mod_strings_display);
                    }
                } else { // All
                    $this->bean->map_markers[] = $this->getMarkerData($display_module, $display, false, $mod_strings_display);
                }
            }
        }

        // Sort marker groups for the view
        sort($this->bean->map_markers_groups);

        // Set display object for later use
        $this->bean->display_object = $this->display_object;

        // Get Prospect List Array Dropdown
        $list = get_module_info('ProspectLists');
        $list_query = $list->create_list_query('prospect_lists.name', '1=1', 0);
        $list_result = $list->db->query($list_query);
        $list_array = array();
        while ($alist = $list->db->fetchByAssoc($list_result)) {
            if (!empty($alist['name']) && !empty($alist['id'])) {
                $list_array[$alist['id']] = $alist['name'];
            }
        }
        $this->bean->list_array = $list_array;
    }

    // end function action_map_markers

    /**
     * Define marker data for marker display view
     * @param $module_type bean name
     * @param $display bean fields array
     * $param $mod_strings_display mod_strings from display module
     * TODO: Use a custom defined field for the $marker['group']
     */
    public function getMarkerData($module_type, $display, $center_marker = false, $mod_strings_display = array())
    {

//        echo "<pre>";
//        print_r($display);
//        print_r($mod_strings_display);
//        echo "</pre>";

        // Define Marker
        $marker = array();
        // Set only partial display data for efficiency
        $marker['name'] = $display['name'];
        // Or, Set all display data for flexibility
        //$marker = $display;
        if (empty($marker['name'])) {
            $marker['name'] = 'N/A';
        }
        $marker['id'] = $display['id'];
        $marker['module'] = $module_type;
        $marker['address'] = $display['jjwg_maps_address_c'];
        $marker['lat'] = $display['jjwg_maps_lat_c'];
        if (!$this->is_valid_lat($marker['lat'])) {
            $marker['lat'] = '0';
        }
        $marker['lng'] = $display['jjwg_maps_lng_c'];
        if (!$this->is_valid_lng($marker['lng'])) {
            $marker['lng'] = '0';
        }
        // Define a phone field: phone_office, phone_work, phone_mobile
        if (!empty($display['phone_office'])) {
            $marker['phone'] = $display['phone_office'];
        } elseif (!empty($display['phone_work'])) {
            $marker['phone'] = $display['phone_work'];
        } elseif (!empty($display['phone_mobile'])) {
            $marker['phone'] = $display['phone_mobile'];
        } else {
            $marker['phone'] = '';
        }

        if ($marker['lat'] != '0' && $marker['lng'] != '0') {

            // Check to see if marker point already exists and apply offset if needed
            // This often occurs when an address is only defined by city, state, zip.
            $i = 0;
            while (isset($this->map_marker_data_points[(string) $marker['lat']][(string) $marker['lng']]) &&
            $i < $this->settings['map_markers_limit']) {
                $marker['lat'] = (float) $marker['lat'] + (float) $this->settings['map_duplicate_marker_adjustment'];
                $marker['lng'] = (float) $marker['lng'] + (float) $this->settings['map_duplicate_marker_adjustment'];
                $i++;
            }
            // Set Marker Point as Used (true)
            $this->map_marker_data_points[(string) $marker['lat']][(string) $marker['lng']] = true;

            if (isset($display['account_name'])) {
                $marker['account_name'] = $display['account_name'];
            }
            if (isset($display['account_id'])) {
                $marker['account_id'] = $display['account_id'];
            }
            $marker['assigned_user_name'] = (isset($display['assigned_user_name'])) ? $display['assigned_user_name'] : '';
            $marker['image'] = (isset($display['marker_image'])) ? $display['marker_image'] : '';

            // Define Marker Group
            if (!$center_marker) {
                // Group Field for the Display Module
                $group_field_name = $this->settings['map_markers_grouping_field'][$module_type];
                $group_field_value = $display[$group_field_name];
                // Check for DOM field types (enum type)
                if (isset($this->display_object->field_name_map[$group_field_name]['type']) &&
                        $this->display_object->field_name_map[$group_field_name]['type'] == 'enum') {
                    $group_field_dom = $this->display_object->field_name_map[$group_field_name]['options'];
                    $marker['group'] = $GLOBALS['app_list_strings'][$group_field_dom][$group_field_value];
                } elseif (!empty($display[$group_field_name])) {
                    $marker['group'] = $display[$group_field_name];
                } else {
                    $marker['group'] = $GLOBALS['mod_strings']['LBL_MAP_NULL_GROUP_NAME']; // null group
                }
                if (!in_array($marker['group'], $this->bean->map_markers_groups)) {
                    $this->bean->map_markers_groups[] = $marker['group'];
                }
            }

            /**
             *  Define Dates for Meetings
             *  TimeDate.php to_display_date_time()
             *  Note, date time fields are converted to the User's date time settings
             */
            if ($module_type == 'Meetings') {
                require_once('modules/Meetings/Meeting.php');
                require_once('include/TimeDate.php');
                if (!isset($meetingTimeDate) || !is_object($meetingTimeDate)) {
                    $meetingTimeDate = new TimeDate();
                }
                $display['date_start'] = $meetingTimeDate->to_display_date_time($display['date_start'], true, true, $GLOBALS['current_user']);
                $display['date_end'] = $meetingTimeDate->to_display_date_time($display['date_end'], true, true, $GLOBALS['current_user']);
            }
            $current_user_data = get_object_vars($GLOBALS['current_user']);
            $this->sugarSmarty->assign('current_user', $current_user_data);
            $this->sugarSmarty->assign('current_user_address', $this->bean->defineMapsFormattedAddress($current_user_data, 'address'));
            $this->sugarSmarty->assign("mod_strings", $mod_strings_display);
            // Define Maps Info Window HTML by Sugar Smarty Template
            $this->sugarSmarty->assign("module_type", $module_type);
            $this->sugarSmarty->assign("address", $display['jjwg_maps_address_c']);
            $this->sugarSmarty->assign("fields", $display); // display fields array
            // Use @ error suppression to avoid issues with SugarCRM On-Demand
            if (file_exists('./custom/modules/jjwg_Maps/tpls/' . $module_type . 'InfoWindow.tpl')) {
                $marker['html'] = $this->sugarSmarty->fetch('./custom/modules/jjwg_Maps/tpls/' . $module_type . 'InfoWindow.tpl');
            }
            if (empty($marker['html'])) {
                $marker['html'] = $this->sugarSmarty->fetch('./modules/jjwg_Maps/tpls/' . $module_type . 'InfoWindow.tpl');
            }
            $marker['html'] = preg_replace('/\n\r/', ' ', (string) $marker['html']);
            //var_dump($marker['html']);
            return $marker;
        } else {
            return false;
        }
    }

    /**
     * Get Marker Data Custom for Mapping
     * @param $marker_object
     */
    public function getMarkerDataCustom($marker_object)
    {

        // Define Marker
        $marker = array();
        $marker['name'] = $marker_object->name;
        if (empty($marker['name'])) {
            $marker['name'] = 'N/A';
        }
        $marker['id'] = $marker_object->id;
        $marker['lat'] = $marker_object->jjwg_maps_lat;
        if (!$this->is_valid_lat($marker['lat'])) {
            $marker['lat'] = '0';
        }
        $marker['lng'] = $marker_object->jjwg_maps_lng;
        if (!$this->is_valid_lng($marker['lng'])) {
            $marker['lng'] = '0';
        }
        $marker['image'] = $marker_object->marker_image;
        if (empty($marker['image'])) {
            $marker['image'] = 'None';
        }

        if ($marker['lat'] != '0' || $marker['lng'] != '0') {
            $fields = array();
            foreach ($marker_object->column_fields as $field) {
                $fields[$field] = $marker_object->$field;
            }
            // Define Maps Info Window HTML by Sugar Smarty Template
            $this->sugarSmarty->assign("module_type", 'jjwg_Markers');
            $this->sugarSmarty->assign("fields", $fields); // display fields array
            // Use @ error suppression to avoid issues with SugarCRM On-Demand
            if (file_exists('./custom/modules/jjwg_Markers/tpls/MarkersInfoWindow.tpl')) {
                $marker['html'] = $this->sugarSmarty->fetch('./custom/modules/jjwg_Markers/tpls/MarkersInfoWindow.tpl');
            }
            if (empty($marker['html'])) {
                $marker['html'] = $this->sugarSmarty->fetch('./modules/jjwg_Markers/tpls/MarkersInfoWindow.tpl');
            }
            $marker['html'] = preg_replace('/\n\r/', ' ', (string) $marker['html']);
            //var_dump($marker['html']);
            return $marker;
        } else {
            return false;
        }
    }

    /**
     * Get Area Data Custom for Mapping
     * @param $area_object
     */
    public function getAreaDataCustom($area_object)
    {

        // Define Area
        $area = array();
        $area['name'] = $area_object->name;
        if (empty($area['name'])) {
            $area['name'] = 'N/A';
        }
        $area['id'] = $area_object->id;
        $area['coordinates'] = $area_object->coordinates;

        // Check for proper coordinates pattern
        if (preg_match('/^[0-9\s\(\)\,\.\-]+$/', (string) $area_object->coordinates)) {
            $fields = array();
            foreach ($area_object->column_fields as $field) {
                $fields[$field] = $area_object->$field;
            }
            // Define Maps Info Window HTML by Sugar Smarty Template
            $this->sugarSmarty->assign("module_type", 'jjwg_Areas');
            $this->sugarSmarty->assign("fields", $fields); // display fields array
            // Use @ error suppression to avoid issues with SugarCRM On-Demand
            if (file_exists('./custom/modules/jjwg_Areas/tpls/AreasInfoWindow.tpl')) {
                $area['html'] = $this->sugarSmarty->fetch();
            }
            if (empty($area['html'])) {
                $area['html'] = $this->sugarSmarty->fetch('./modules/jjwg_Areas/tpls/AreasInfoWindow.tpl');
            }
            $area['html'] = preg_replace('/\n\r/', ' ', (string) $area['html']);
            //var_dump($marker['html']);
            return $area;
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
}
