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
class stic_Custom_View_Customizations extends Basic
{
    public $new_schema = true;
    public $module_dir = 'stic_Custom_View_Customizations';
    public $object_name = 'stic_Custom_View_Customizations';
    public $table_name = 'stic_custom_view_customizations';
    public $importable = false;

    public $id;
    public $customization_order;
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
    public $SecurityGroups;
    public $status;
    public $conditions;
    public $actions;

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

    }

    public function save($check_notify = false)
    {
        require_once 'modules/stic_Custom_Views/Utils.php';
        require_once 'modules/stic_Custom_View_Customizations/Utils.php';
        require_once 'modules/stic_Custom_View_Conditions/stic_Custom_View_Conditions.php';
        require_once 'modules/stic_Custom_View_Actions/stic_Custom_View_Actions.php';

        // customization_order must be >0
        if ($this->customization_order <= 0) {
            $this->customization_order = 1;
        }

        $return_id = parent::save($check_notify);
        $viewBean = getCustomView($this);

        // Save Condition lines
        $condition = BeanFactory::newBean('stic_Custom_View_Conditions');
        $condition->save_lines($_POST, $viewBean->view_module, $this, 'sticCustomView_Condition');

        // Remove Condition lines in $_POST
        $conditionPostToDelete = array();
        foreach ($_POST as $postKey => $postValue) {
            if (str_starts_with($postKey, 'sticCustomView_Condition')) {
                $conditionPostToDelete[$postKey] = $postValue;
            }
        }
        $_POST = array_diff_key($_POST, $conditionPostToDelete);

        // Save Action lines
        $action = BeanFactory::newBean('stic_Custom_View_Actions');
        $action->save_lines($_POST, $viewBean->view_module, $this, 'sticCustomView_Action');

        // Remove Action lines in $_POST
        $actionPostToDelete = array();
        foreach ($_POST as $postKey => $postValue) {
            if (str_starts_with($postKey, 'sticCustomView_Action')) {
                $actionPostToDelete[$postKey] = $postValue;
            }
        }
        $_POST = array_diff_key($_POST, $actionPostToDelete);

        // Set Conditions field
        $conditionBeanArray = getRelatedBeanObjectArray($this, 'stic_custom_view_customizations_stic_custom_view_conditions');
        $conditions = array();
        foreach ($conditionBeanArray as $conditionBean) {
            $conditions[] = $conditionBean->field . "." . $conditionBean->operator . "." . $conditionBean->condition_type . "." . $conditionBean->value;
        }
        $this->conditions = implode(" + ", $conditions);

        // Set Actions field
        $actionsBeanArray = getRelatedBeanObjectArray($this, 'stic_custom_view_customizations_stic_custom_view_actions');
        $actions = array();
        foreach ($actionsBeanArray as $actionBean) {
            $actions[] = $actionBean->type . ":" . $actionBean->element . "." . $actionBean->action . "=" . $actionBean->value . "(" . $actionBean->element_section . ")";
        }
        $this->actions = implode(" + ", $actions);

        // Ensure customization_order is not set or change others
        $customizationBeanArray = getRelatedBeanObjectArray($viewBean, 'stic_custom_views_stic_custom_view_customizations');
        foreach ($customizationBeanArray as $customizationBean) {
            if ($customizationBean->id != $this->id &&
                $customizationBean->deleted == "0" &&
                $customizationBean->customization_order == $this->customization_order) {
                $customizationBean->customization_order = $customizationBean->customization_order + 1;
                $customizationBean->save();
            }
        }

        parent::save($check_notify);

        return $return_id;
    }

    public function duplicateTo($customViewId)
    {
        require_once 'modules/stic_Custom_Views/Utils.php';

        $conditionBeanArray = getRelatedBeanObjectArray($this, 'stic_custom_view_customizations_stic_custom_view_conditions');
        $fieldsToExclude = array(
            "id",
            "stic_custom_view_customizations_stic_custom_view_conditions",
            "stic_custom_view_customizations_stic_custom_view_conditions_name",
            "stic_custo233dzations_ida");

        $this->addToPost($conditionBeanArray, "sticCustomView_Condition", $fieldsToExclude);

        $actionsBeanArray = getRelatedBeanObjectArray($this, 'stic_custom_view_customizations_stic_custom_view_actions');
        $fieldsToExclude = array(
            "id",
            "stic_custom_view_customizations_stic_custom_view_actions",
            "stic_custom_view_customizations_stic_custom_view_actions_name",
            "stic_custo077ezations_ida");
        $this->addToPost($actionsBeanArray, "sticCustomView_Action", $fieldsToExclude);

        $clone = clone $this;
        $clone->id = null;
        $clone->customization_order++;
        $clone->name = $clone->name . " " . translate("LBL_NAME_COPY_SUFFIX");
        $clone->stic_custo45d1m_views_ida = $customViewId;

        $clone->save();
    }

    private function addToPost($beanArray, $keyPostBean, $fieldsToExclude)
    {
        foreach ($_POST as $key => $value) {
            if (str_starts_with($key, $keyPostBean)) {
                $_POST[$key] = array();
            }
        }
        foreach ($beanArray as $bean) {
            foreach ($bean->field_defs as $field_def) {
                $field_name = $field_def['name'];
                if (!in_array($field_name, $fieldsToExclude)) {
                    $_POST[$keyPostBean . $field_name][] = $bean->$field_name;
                }
            }
        }
    }

    public function mark_deleted($id)
    {
        // Delete all Conditions
        $relatedBeans = getRelatedBeanObjectArray($this, 'stic_custom_view_customizations_stic_custom_view_conditions');
        foreach ($relatedBeans as $relatedBean) {
            $relatedBean->mark_deleted($relatedBean->id);
        }

        // Delete all Actions
        $relatedBeans = getRelatedBeanObjectArray($this, 'stic_custom_view_customizations_stic_custom_view_actions');
        foreach ($relatedBeans as $relatedBean) {
            $relatedBean->mark_deleted($relatedBean->id);
        }

        parent::mark_deleted($id);
    }
}
