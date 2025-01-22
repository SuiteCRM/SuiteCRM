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



if (!class_exists('Tracker')) {
    require_once 'data/SugarBean.php';

    #[\AllowDynamicProperties]
    class Tracker extends SugarBean
    {
        public $module_dir = 'Trackers';
        public $table_name = 'tracker';
        public $object_name = 'Tracker';
        public $disable_var_defs = true;
        public $acltype = 'Tracker';
        public $acl_category = 'Trackers';
        public $disable_custom_fields = true;
        public $column_fields = array(
        "id",
        "monitor_id",
        "user_id",
        "module_name",
        "item_id",
        "item_summary",
        "date_modified",
        "action",
        "session_id",
        "visible"
    );

        public function __construct()
        {
            global $dictionary;
            if (isset($this->module_dir) && isset($this->object_name) && !isset($GLOBALS['dictionary'][$this->object_name])) {
                $path = 'modules/Trackers/vardefs.php';
                if (defined('TEMPLATE_URL')) {
                    $path = SugarTemplateUtilities::getFilePath($path);
                }
                require_once($path);
            }
            parent::__construct();
        }

        /*
         * Return the most recently viewed items for this user.
         * The number of items to return is specified in sugar_config['history_max_viewed']
         * @param uid user_id
         * @param mixed module_name Optional - return only items from this module, a string of the module or array of modules
         * @return array list
         */
        public function get_recently_viewed($user_id, $modules = '')
        {
            $path = 'modules/Trackers/BreadCrumbStack.php';
            if (defined('TEMPLATE_URL')) {
                $path = SugarTemplateUtilities::getFilePath($path);
            }
            require_once($path);
            if (empty($_SESSION['breadCrumbs'])) {
                $breadCrumb = new BreadCrumbStack($user_id, $modules);
                $_SESSION['breadCrumbs'] = $breadCrumb;
                $GLOBALS['log']->info(string_format($GLOBALS['app_strings']['LBL_BREADCRUMBSTACK_CREATED'], array($user_id)));
            } else {
                $breadCrumb = $_SESSION['breadCrumbs'];
                $module_query = '';
                if (!empty($modules)) {
                    $history_max_viewed = 10;
                    $module_query = is_array($modules) ? ' AND module_name IN (\'' . implode("','", $modules) . '\')' :  ' AND module_name = \'' . $modules . '\'';
                } else {
                    $history_max_viewed = (!empty($GLOBALS['sugar_config']['history_max_viewed']))? $GLOBALS['sugar_config']['history_max_viewed'] : 50;
                }

                $query = 'SELECT item_id, item_summary, module_name, id FROM ' . $this->table_name . ' WHERE id = (SELECT MAX(id) as id FROM ' . $this->table_name . ' WHERE user_id = \'' . $user_id . '\' AND deleted = 0 AND visible = 1' . $module_query . ')';
                $result = $this->db->limitQuery($query, 0, $history_max_viewed, true, $query);
                while (($row = $this->db->fetchByAssoc($result))) {
                    $breadCrumb->push($row);
                }
            }

            $list = $breadCrumb->getBreadCrumbList($modules);
            $GLOBALS['log']->info("Tracker: retrieving ".(is_countable($list) ? count($list) : 0)." items");
            return $list;
        }

        public function makeInvisibleForAll($item_id)
        {
            $query = "UPDATE $this->table_name SET visible = 0 WHERE item_id = '$item_id' AND visible = 1";
            $this->db->query($query, true);
            $path = 'modules/Trackers/BreadCrumbStack.php';
            if (defined('TEMPLATE_URL')) {
                $path = SugarTemplateUtilities::getFilePath($path);
            }
            require_once($path);
            if (!empty($_SESSION['breadCrumbs'])) {
                $breadCrumbs = $_SESSION['breadCrumbs'];
                $breadCrumbs->popItem($item_id);
            }
        }

        public static function logPage()
        {
            $time_on_last_page = 0;
            //no need to calculate it if it is a redirection page
            if (empty($GLOBALS['app']->headerDisplayed)) {
                return;
            }
            if (!empty($_SESSION['lpage'])) {
                $time_on_last_page = time() - $_SESSION['lpage'];
            }
            $_SESSION['lpage']=time();
        }


        /**
         * bean_implements
         * Override method to support ACL roles
         */
        public function bean_implements($interface)
        {
            return false;
        }
    }
}
