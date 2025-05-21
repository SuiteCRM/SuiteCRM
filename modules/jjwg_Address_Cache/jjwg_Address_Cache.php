<?php

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('modules/jjwg_Maps/jjwg_Maps.php');
require_once('modules/jjwg_Address_Cache/jjwg_Address_Cache_sugar.php');

#[\AllowDynamicProperties]
class jjwg_Address_Cache extends jjwg_Address_Cache_sugar
{

    /**
     * @var settings array
     */
    public $settings = array();


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
     * Get the Address Info from the Address Cache Module
     * @param $aInfo array of geocode info (lng, lat, status, address)
     */
    public function getAddressCacheInfo($aInfo = array())
    {
        if (is_array($aInfo)) {
            if (!isset($aInfo['address'])) {
                LoggerManager::getLogger()->warn('address info not found');
                $aInfoAddress = null;
            } else {
                $aInfoAddress = $aInfo['address'];
            }

            $address = $aInfoAddress;
        } else {
            $address = (string)$aInfo;
        }

        if (!empty($this->settings['address_cache_get_enabled']) && !empty($address)) {
            $query = "SELECT " . $this->table_name . ".* FROM " . $this->table_name . " WHERE " .
                    $this->table_name . ".deleted = 0 AND " .
                    $this->table_name . ".name='" . $this->db->quote($address) . "'";
            //var_dump($query);
            $cache_result = $this->db->limitQuery($query, 0, 1);

            if ($cache_result) {
                $address_cache = $this->db->fetchByAssoc($cache_result);

                // Note: In the jjwg_Address_Cache module the 'name' field is used for the 'address'
                if (!empty($address_cache['name']) && !($address_cache['lng'] == 0 && $address_cache['lat'] == 0) &&
                        $this->is_valid_lng($address_cache['lng']) && $this->is_valid_lat($address_cache['lat'])) {
                    $aInfo['address'] = $address_cache['name'];
                    $aInfo['lat'] = $address_cache['lat'];
                    $aInfo['lng'] = $address_cache['lng'];
                    $aInfo['status'] = 'OK';

                    return $aInfo;
                }
            }
        }
        return false;
    }

    /**
     * Save New Address Info to the Address Cache Module / Table
     * @param $aInfo array of geocode info (lng, lat, status, address)
     */
    public function saveAddressCacheInfo($aInfo = array())
    {

        // Bug: $current_user object not properly set for some reason
        if (empty($GLOBALS['current_user']->id) && !empty($_SESSION['authenticated_user_id'])) {
            $GLOBALS['current_user']->id = $_SESSION['authenticated_user_id'];
        }

        if (!empty($this->settings['address_cache_save_enabled'])) {

            // Check for existing address cache data (prevent duplicates)
            $address_cache = $this->getAddressCacheInfo($aInfo);

            if (empty($address_cache)) {
                if (!empty($aInfo['address']) && !($aInfo['lng'] == 0 && $aInfo['lat'] == 0) &&
                        $this->is_valid_lng($aInfo['lng']) && $this->is_valid_lat($aInfo['lat'])) {

                    // Note: The modules 'name' field is used for the 'address'
                    // 'status' is not saved in the cache table.
                    $cache = BeanFactory::newBean('jjwg_Address_Cache');
                    $cache->update_modified_by = true;
                    $cache->name = $aInfo['address'];
                    $cache->lat = $aInfo['lat'];
                    $cache->lng = $aInfo['lng'];
                    $cache->description = '';
                    $cache->assigned_user_id = $GLOBALS['current_user']->id;
                    $cache->save(false);
                    return true;
                }
            }
        }
        return false;
    }

    /**
     *
     * Delete all Address Cache records
     * Complete delete, not a soft delete
     */
    public function deleteAllAddressCache()
    {

        // Delete all from jjwg_address_cache
        $query = "TRUNCATE TABLE jjwg_address_cache;";
        $delete_result = $this->db->query($query);
        $query = "TRUNCATE TABLE jjwg_address_cache_audit;";
        $delete_audit_result = $this->db->query($query);
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
