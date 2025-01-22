<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
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

require_once('modules/Trackers/monitor/Monitor.php');



#[\AllowDynamicProperties]
class TrackerManager
{
    private static $instance;
    private static $monitor_id;
    private $metadata = array();
    private $monitors = array();
    private $disabledMonitors = array();
    private static $paused = false;

    /**
     * Constructor for TrackerManager.  Declared private for singleton pattern.
     *
     */
    private function __construct()
    {
        require('modules/Trackers/config.php');
        $this->metadata = $tracker_config;
        self::$monitor_id = create_guid();
    }

    /**
     * setup
     * This is a private method used to load the configuration settings whereby
     * monitors may be disabled via the Admin settings interface
     *
     */
    private function setup()
    {
        if (!empty($this->metadata) && empty($GLOBALS['installing'])) {
            $admin = BeanFactory::newBean('Administration');
            $admin->retrieveSettings('tracker');
            foreach ($this->metadata as $key=>$entry) {
                if (isset($entry['bean'])) {
                    if (!empty($admin->settings['tracker_'. $entry['name']])) {
                        $this->disabledMonitors[$entry['name']] = true;
                    }
                }
            }
        }
    }

    public function setMonitorId($id)
    {
        self::$monitor_id = $id;
        foreach ($this->monitors as $monitor) {
            $monitor->monitor_id = self::$monitor_id;
        }
    }

    /**
     * getMonitorId
     * Returns the monitor id associated with this TrackerManager instance
     * @returns String id value
     */
    public function getMonitorId()
    {
        return self::$monitor_id;
    }

    /**
     * getInstance
     * Singleton method to return static instance of TrackerManager
     * @returns static TrackerManager instance
     */
    public static function getInstance()
    {
        if (!isset(self::$instance)) {
            self::$instance = new TrackerManager();
            //Set global variable for tracker monitor instances that are disabled
            self::$instance->setup();
        } // if
        return self::$instance;
    }

    /**
     * getMonitor
     * This method returns a Monitor instance based on the monitor name.
     * @param string $name value of the monitor's name to retrieve
     * @return Monitor instance corresponding to name or a BlankMonitor instance if one could not be found
     */
    public function getMonitor($name)
    {
        //don't waste our time on disabled monitors
        if ($name!='tracker_sessions' && !empty($this->disabledMonitors[$name])) {
            return false;
        }
        if (isset($this->monitors[$name])) {
            return $this->monitors[$name];
        }

        if (isset($this->metadata) && isset($this->metadata[$name])) {
            try {
                $instance = $this->_getMonitor(
                    $this->metadata[$name]['name'], //name
                                      self::$monitor_id, //monitor_id
                                   $this->metadata[$name]['metadata'],
                    $this->metadata[$name]['store'] //store
                                   );
                $this->monitors[$name] = $instance;
                return $this->monitors[$name];
            } catch (Exception $ex) {
                $GLOBALS['log']->error($ex->getMessage());
                $GLOBALS['log']->error($ex->getTraceAsString());
                require_once('modules/Trackers/monitor/BlankMonitor.php');
                $this->monitors[$name] = new BlankMonitor();
                return $this->monitors[$name];
            }
        } else {
            $GLOBALS['log']->error($GLOBALS['app_strings']['ERR_MONITOR_NOT_CONFIGURED'] . "($name)");
            require_once('modules/Trackers/monitor/BlankMonitor.php');
            $this->monitors[$name] = new BlankMonitor();
            return $this->monitors[$name];
        }
    }

    private function _getMonitor($name='', $monitorId='', $metadata='', $store='')
    {
        $class = strtolower($name.'_monitor');
        $monitor = null;
        if (file_exists('custom/modules/Trackers/monitor/'.$class.'.php')) {
            require_once('custom/modules/Trackers/monitor/'.$class.'.php');
            if (class_exists($class)) {
                $monitor = new $class($name, $monitorId, $metadata, $store);
            }
        } elseif (file_exists('modules/Trackers/monitor/'.$class.'.php')) {
            require_once('modules/Trackers/monitor/'.$class.'.php');
            if (class_exists($class)) {
                $monitor = new $class($name, $monitorId, $metadata, $store);
            }
        } else {
            $monitor = new Monitor($name, $monitorId, $metadata, $store);
        }


        $monitor->setEnabled(empty($this->disabledMonitors[$monitor->name]));
        return $monitor;
    }

    /**
     * save
     * This method handles saving the monitors and their metrics to the mapped Store implementations
     */
    public function save()
    {

    // Session tracker always saves.
        if (isset($this->monitors['tracker_sessions'])) {
            $this->monitors['tracker_sessions']->save();
            unset($this->monitors['tracker_sessions']);
        }

        if (!$this->isPaused()) {
            foreach ($this->monitors as $monitor) {
                if (array_key_exists('Trackable', class_implements($monitor))) {
                    $monitor->save();
                }
            }
        }
    }

    /**
     * saveMonitor
     * Saves the monitor instance and then clears it
     * If ignoreDisabled is set the ignore the fact of this monitor being disabled
     */
    public function saveMonitor($monitor, $flush=true, $ignoreDisabled = false)
    {
        if (!$this->isPaused() && !empty($monitor)) {
            if ((empty($this->disabledMonitors[$monitor->name]) || $ignoreDisabled) && array_key_exists('Trackable', class_implements($monitor))) {
                $monitor->save($flush);

                if ($flush) {
                    $monitor->clear();
                    unset($this->monitors[strtolower($monitor->name)]);
                }
            }
        }
    }

    /**
     * unsetMonitor
     * Method to unset the monitor so that it will not be saved
     */
    public function unsetMonitor($monitor)
    {
        if (!empty($monitor)) {
            unset($this->monitors[strtolower($monitor->name)]);
        }
    }

    /**
     * pause
     * This function is to be called by a client in order to pause tracking through the lifetime of a Request.
     * Tracking can be started again by calling unPauseTracking
     *
     * Usage: TrackerManager::getInstance()->pauseTracking();
     */
    public function pause()
    {
        self::$paused = true;
    }

    /**
     * unPause
     * This function is to be called by a client in order to unPause tracking through the lifetime of a Request.
     * Tracking can be paused by calling pauseTracking
     *
     *  * Usage: TrackerManager::getInstance()->unPauseTracking();
     */
    public function unPause()
    {
        self::$paused = false;
    }


    /**
     * isPaused
     * This function returns the current value of the private paused variable.
     * The result indicates whether or not the TrackerManager is paused.
     *
     * @return boolean value indicating whether or not TrackerManager instance is paused.
     */
    public function isPaused()
    {
        return self::$paused;
    }

    /**
     * getDisabledMonitors
     * Returns an Array of Monitor's name(s) that hhave been set to disabled in the
     * Administration section.
     *
     * @return Array of disabled Monitor's name(s) that hhave been set to disabled in the
     * Administration section.
     */
    public function getDisabledMonitors()
    {
        return $this->disabledMonitors;
    }

    /**
     * Set the disabled monitors
     *
     * @param array $disabledMonitors
     */
    public function setDisabledMonitors($disabledMonitors)
    {
        $this->disabledMonitors = $disabledMonitors;
    }

    /**
     * unsetMonitors
     * Function to unset all Monitors loaded for a TrackerManager instance
     *
     */
    public function unsetMonitors()
    {
        $mons = $this->monitors;
        foreach ($mons as $key=>$m) {
            $this->unsetMonitor($m);
        }
    }
}
