<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('modules/stic_Import_Validation/Forms.php');

#[\AllowDynamicProperties]
class stic_UsersLastImport extends SugarBean
{
    /**
     * Fields in the table
     */
    public $id;
    public $assigned_user_id;
    public $import_module;
    public $bean_type;
    public $bean_id;
    public $deleted;

    /**
     * Set the default settings from Sugarbean
     */
    public $module_dir = 'stic_Import_Validation';
    public $table_name = "stic_users_last_import";
    public $object_name = "stic_UsersLastImport";
    public $disable_custom_fields = true;
    public $column_fields = array(
        "id",
        "assigned_user_id",
        "bean_type",
        "bean_id",
        "deleted"
        );
    public $new_schema = true;
    public $additional_column_fields = array();

    /**
     * Constructor
     */
    public function __construct()
    {
        parent::__construct();
    }

    /**
     * Extends SugarBean::listviewACLHelper
     *
     * @return array
     */
    public function listviewACLHelper()
    {
        $array_assign = parent::listviewACLHelper();
        $is_owner = false;
        if (!ACLController::moduleSupportsACL('Accounts')
                || ACLController::checkAccess('Accounts', 'view', $is_owner)) {
            $array_assign['ACCOUNT'] = 'a';
        } else {
            $array_assign['ACCOUNT'] = 'span';
        }
        return $array_assign;
    }

    /**
     * Delete all the records for a particular user
     *
     * @param string $user_id user id of the user doing the import
     */
    public function mark_deleted_by_user_id($user_id)
    {
        $query = "DELETE FROM $this->table_name WHERE assigned_user_id = '$user_id'";
        $this->db->query($query, true, "Error marking last imported records deleted: ");
    }

    /**
     * Undo a single record
     *
     * @param string $id specific users_last_import id to undo
     */
    public function undoById($id)
    {
        global $current_user;

        $query1 = "SELECT bean_id, bean_type FROM users_last_import WHERE assigned_user_id = '$current_user->id'
                   AND id = '$id' AND deleted=0";

        $result1 = $this->db->query($query1);
        if (!$result1) {
            return false;
        }

        while ($row1 = $this->db->fetchByAssoc($result1)) {
            $this->_deleteRecord($row1['bean_id'], $row1['bean_type']);
        }

        return true;
    }

    /**
     * Undo an import
     *
     * @param string $module  module being imported into
     */
    public function undo($module)
    {
        global $current_user;

        $query1 = "SELECT bean_id, bean_type FROM users_last_import WHERE assigned_user_id = '$current_user->id'
                   AND import_module = '$module' AND deleted=0";

        $result1 = $this->db->query($query1);
        if (!$result1) {
            return false;
        }

        while ($row1 = $this->db->fetchByAssoc($result1)) {
            $this->_deleteRecord($row1['bean_id'], $row1['bean_type']);
        }

        return true;
    }

    /**
     * Deletes a record in a bean
     *
     * @param $bean_id
     * @param $module
     */
    protected function _deleteRecord($bean_id, $module)
    {
        static $focus;

        // load bean
        if (!($focus instanceof $module)) {
            require_once($GLOBALS['beanFiles'][$module]);
            $focus = new $module;
        }

        $focus->mark_relationships_deleted($bean_id);

        $result = $this->db->query(
            "DELETE FROM {$focus->table_name}
                WHERE id = '{$bean_id}'"
            );
        if (!$result) {
            return false;
        }
        // Bug 26318: Remove all created e-mail addresses ( from jchi )
        $result2 = $this->db->query(
            "SELECT email_address_id
                FROM email_addr_bean_rel
                WHERE email_addr_bean_rel.bean_id='{$bean_id}'
                    AND email_addr_bean_rel.bean_module='{$focus->module_dir}'"
        );
        $this->db->query(
            "DELETE FROM email_addr_bean_rel
                WHERE email_addr_bean_rel.bean_id='{$bean_id}'
                    AND email_addr_bean_rel.bean_module='{$focus->module_dir}'"
            );

        while ($row2 = $this->db->fetchByAssoc($result2)) {
            if (!$this->db->getOne(
                "SELECT email_address_id
                        FROM email_addr_bean_rel
                        WHERE email_address_id = '{$row2['email_address_id']}'"
            )) {
                $this->db->query(
                    "DELETE FROM email_addresses
                        WHERE id = '{$row2['email_address_id']}'"
                );
            }
        }

        if ($focus->hasCustomFields()) {
            $this->db->query(
                "DELETE FROM {$focus->table_name}_cstm
                    WHERE id_c = '{$bean_id}'"
            );
        }
    }

    /**
     * Get a list of bean types created in the import
     *
     * @param string $module  module being imported into
     */
    public static function getBeansByImport($module)
    {
        global $current_user;

        $query1 = "SELECT DISTINCT bean_type FROM users_last_import WHERE assigned_user_id = '$current_user->id'
                   AND import_module = '$module' AND deleted=0";

        $result1 = DBManagerFactory::getInstance()->query($query1);
        if (!$result1) {
            return array($module);
        }

        $returnarray = array();
        while ($row1 = DBManagerFactory::getInstance()->fetchByAssoc($result1)) {
            $returnarray[] = $row1['bean_type'];
        }

        return $returnarray;
    }
}
