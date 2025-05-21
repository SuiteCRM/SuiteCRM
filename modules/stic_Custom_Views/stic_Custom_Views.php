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

#[\AllowDynamicProperties]
class stic_Custom_Views extends Basic
{
    public $new_schema = true;
    public $module_dir = 'stic_Custom_Views';
    public $object_name = 'stic_Custom_Views';
    public $table_name = 'stic_custom_views';
    public $importable = false;

    public $id;
    public $customization_name;
    public $view_module;
    public $view_type;
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
    public $SecurityGroups;
    public $user_type;
    public $roles;
    public $security_groups;
    public $roles_exclude;
    public $security_groups_exclude;
    public $status;

    public $show_SubPanelTopButtonListView = false;

    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return true;
        }

        return false;
    }

    public function __construct()
    {
        parent::__construct();

        require_once 'modules/stic_Custom_Views/Utils.php';
        fillDynamicGenericLists();
    }

    private function before_save()
    {
        // Ensure name is correct
        global $app_list_strings;
        $this->name = $app_list_strings['moduleList'][$this->view_module] . ' - ' .
        $this->customization_name . ' - ' .
            $app_list_strings['stic_custom_views_views_list'][$this->view_type];
    }

    public function save($check_notify = false)
    {
        $this->before_save();

        $id = parent::save($check_notify);

        if (isset($_POST["duplicateSave"]) && $_POST["duplicateSave"] == "true") {
            // Duplicate Customizations
            $originalBean = BeanFactory::getBean("stic_Custom_Views", $_POST["duplicateId"]);
            $customizationBeans = getRelatedBeanObjectArray($originalBean, 'stic_custom_views_stic_custom_view_customizations');
            foreach ($customizationBeans as $customizationBean) {
                $customizationBean->duplicateTo($id);
            }
        }
        return $id;
    }

    public function mark_deleted($id)
    {
        // Delete all Customizations
        $relatedBeans = getRelatedBeanObjectArray($this, 'stic_custom_views_stic_custom_view_customizations');
        foreach ($relatedBeans as $relatedBean) {
            $relatedBean->mark_deleted($relatedBean->id);
        }

        parent::mark_deleted($id);
    }
}
