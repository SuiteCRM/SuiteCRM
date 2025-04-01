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

require_once 'data/SugarBean.php';
require_once 'include/SugarObjects/templates/basic/Basic.php';
require_once 'include/externalAPI/ExternalAPIFactory.php';
require_once 'include/SugarOauth.php';

#[\AllowDynamicProperties]
class EAPM extends Basic
{
    public $new_schema = true;
    public $module_dir = 'EAPM';
    public $object_name = 'EAPM';
    public $table_name = 'eapm';
    public $importable = false;
    public $id;
    public $type;
    public $name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $modified_by_name;
    public $created_by;
    public $created_by_name;
    public $description;
    public $deleted;
    public $created_by_link;
    public $modified_user_link;
    public $assigned_user_id;
    public $assigned_user_name;
    public $assigned_user_link;
    public $password;
    public $url;
    public $validated = false;
    public $oauth_token;
    public $oauth_secret;
    public $application;
    public $consumer_key;
    public $consumer_secret;
    public $disable_row_level_security = true;
    public static $passwordPlaceholder = '::PASSWORD::';

    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL': return true;
        }

        return false;
    }

    public static function getLoginInfo($application, $includeInactive = false)
    {
        global $current_user;

        $eapmBean = new self();

        if (isset($_SESSION['EAPM'][$application]) && !$includeInactive) {
            if (is_array($_SESSION['EAPM'][$application])) {
                $eapmBean->fromArray($_SESSION['EAPM'][$application]);
            } else {
                return;
            }
        } else {
            $queryArray = array('assigned_user_id' => $current_user->id, 'application' => $application, 'deleted' => 0);
            if (!$includeInactive) {
                $queryArray['validated'] = 1;
            }
            $eapmBean = $eapmBean->retrieve_by_string_fields($queryArray, false);

            // Don't cache the include inactive results
            if (!$includeInactive) {
                if ($eapmBean != null) {
                    $_SESSION['EAPM'][$application] = $eapmBean->toArray();
                } else {
                    $_SESSION['EAPM'][$application] = '';

                    return;
                }
            }
        }

        if (isset($eapmBean->password)) {
            require_once 'include/utils/encryption_utils.php';
            $eapmBean->password = blowfishDecode(blowfishGetKey('encrypt_field'), $eapmBean->password);
        }

        return $eapmBean;
    }

    public function create_new_list_query($order_by, $where, $filter = array(), $params = array(), $show_deleted = 0, $join_type = '', $return_array = false, $parentbean = null, $singleSelect = false, $ifListForExport = false)
    {
        global $current_user;

        if (!is_admin($GLOBALS['current_user'])) {
            // Restrict this so only admins can see other people's records
            $owner_where = $this->getOwnerWhere($current_user->id);

            if (empty($where)) {
                $where = $owner_where;
            } else {
                $where .= ' AND '.$owner_where;
            }
        }

        return parent::create_new_list_query($order_by, $where, $filter, $params, $show_deleted, $join_type, $return_array, $parentbean, $singleSelect);
    }

    public function save($check_notify = false)
    {
        $this->fillInName();
        if (!is_admin($GLOBALS['current_user'])) {
            $this->assigned_user_id = $GLOBALS['current_user']->id;
        }

        if (!empty($this->password) && $this->password == self::$passwordPlaceholder) {
            $this->password = empty($this->fetched_row['password']) ? '' : $this->fetched_row['password'];
        }

        $parentRet = parent::save($check_notify);

        // Nuke the EAPM cache for this record
        if (isset($_SESSION['EAPM'][$this->application])) {
            unset($_SESSION['EAPM'][$this->application]);
        }

        return $parentRet;
    }

    public function mark_deleted($id)
    {
        // Nuke the EAPM cache for this record
        if (isset($_SESSION['EAPM'][$this->application])) {
            unset($_SESSION['EAPM'][$this->application]);
        }

        return parent::mark_deleted($id);
    }

    public function validated()
    {
        if (empty($this->id)) {
            return false;
        }
        // Don't use save, it will attempt to revalidate
        $adata = DBManagerFactory::getInstance()->quote(isset($this->api_data) ? $this->api_data : null);
        DBManagerFactory::getInstance()->query("UPDATE eapm SET validated=1,api_data='$adata'  WHERE id = '{$this->id}' AND deleted = 0");
        if (!$this->deleted && !empty($this->application)) {
            // deactivate other EAPMs with same app
            $sql = "UPDATE eapm SET deleted=1 WHERE application = '{$this->application}' AND id != '{$this->id}' AND deleted = 0 AND assigned_user_id = '{$this->assigned_user_id}'";
            DBManagerFactory::getInstance()->query($sql, true);
        }

        // Nuke the EAPM cache for this record
        if (isset($_SESSION['EAPM'][$this->application])) {
            unset($_SESSION['EAPM'][$this->application]);
        }
    }

    protected function fillInName()
    {
        if (!empty($this->application)) {
            $apiList = ExternalAPIFactory::loadFullAPIList(false, true);
        }
        if (!empty($apiList) && isset($apiList[$this->application]) && $apiList[$this->application]['authMethod'] == 'oauth') {
            $this->name = sprintf(translate('LBL_OAUTH_NAME', $this->module_dir), $this->application);
        }
    }

    public function fill_in_additional_detail_fields()
    {
        $this->fillInName();
        parent::fill_in_additional_detail_fields();
    }

    public function fill_in_additional_list_fields()
    {
        $this->fillInName();
        parent::fill_in_additional_list_fields();
    }

    public function save_cleanup()
    {
        $this->oauth_token = '';
        $this->oauth_secret = '';
        $this->api_data = '';
    }

    /**
     * Given a user remove their associated accounts. This is called when a user is deleted from the system.
     *
     * @param  $user_id
     */
    public function delete_user_accounts($user_id)
    {
        $sql = "DELETE FROM {$this->table_name} WHERE assigned_user_id = '{$user_id}'";
        DBManagerFactory::getInstance()->query($sql, true);
    }
}

// External API integration, for the dropdown list of what external API's are available
function getEAPMExternalApiDropDown()
{
    $apiList = ExternalAPIFactory::getModuleDropDown('', true, true);

    return $apiList;
}
