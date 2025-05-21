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
require_once('modules/Trackers/Metric.php');
require_once('modules/Trackers/Trackable.php');

#[\AllowDynamicProperties]
class BlankMonitor extends Monitor implements Trackable
{

    /**
     * BlankMonitor constructor
     */
    public function __construct()
    {
    }




    /**
     * setValue
     * Sets the value defined in the monitor's metrics for the given name
     * @param $name String value of metric name
     * @param $value Mixed value
     * @throws Exception Thrown if metric name is not configured for monitor instance
     */
    public function setValue($name, $value)
    {
    }

    /**
     * getStores
     * Returns Array of store names defined for monitor instance
     * @return Array of store names defined for monitor instance
     */
    public function getStores()
    {
        return null;
    }

    /**
     * getMetrics
     * Returns Array of metric instances defined for monitor instance
     * @return Array of metric instances defined for monitor instance
     */
    public function getMetrics()
    {
        return null;
    }

    /**
     * save
     * This method retrieves the Store instances associated with monitor and calls
     * the flush method passing with the montior ($this) instance.
     *
     */
    public function save($flush = \true)
    {
    }


    /**
     * getStore
     * This method checks if the Store implementation has already been instantiated and
     * will return the one stored; otherwise it will create the Store implementation and
     * save it to the Array of Stores.
     * @param $store The name of the store as defined in the 'modules/Trackers/config.php' settings
     * @return An instance of a Store implementation
     * @throws Exception Thrown if $store class cannot be loaded
     */
    protected function getStore($store)
    {
        return null;
    }


    /**
     * clear
     * This function clears the metrics data in the monitor instance
     */
    public function clear()
    {
    }
}
