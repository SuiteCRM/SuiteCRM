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






#[\AllowDynamicProperties]
class SugarWidgetSubPanelTopButton extends SugarWidget
{
    public $module;
    public $title;
    public $access_key;
    public $form_value;
    public $additional_form_fields;
    public $acl;

    //TODO rename defines to layout defs and make it a member variable instead of passing it multiple layers with extra copying.

    /** Take the keys for the strings and look them up.  Module is literal, the rest are label keys
    */
    public function __construct($module='', $title='', $access_key='', $form_value='')
    {
        global $app_strings;

        if (is_array($module)) {
            // it is really the class details from the mapping
            $class_details = $module;

            // If keys were passed into the constructor, translate them from keys to values.
            if (!empty($class_details['module'])) {
                $this->module = $class_details['module'];
            }
            if (!empty($class_details['title'])) {
                $this->title = $app_strings[$class_details['title']];
            }
            if (!empty($class_details['access_key'])) {
                $this->access_key = $app_strings[$class_details['access_key']];
            }
            if (!empty($class_details['form_value'])) {
                $this->form_value = translate($class_details['form_value'], $this->module);
            }
            if (!empty($class_details['additional_form_fields'])) {
                $this->additional_form_fields = $class_details['additional_form_fields'];
            }
            if (!empty($class_details['ACL'])) {
                $this->acl = $class_details['ACL'];
            }
        } else {
            $this->module = $module;

            // If keys were passed into the constructor, translate them from keys to values.
            if (!empty($title)) {
                $this->title = $app_strings[$title];
            }
            if (!empty($access_key)) {
                $this->access_key = $app_strings[$access_key];
            }
            if (!empty($form_value)) {
                $this->form_value = translate($form_value, $module);
            }
        }
    }

    public function getWidgetId($buttonSuffix = true)
    {
        $widgetID = parent::getWidgetId() . '_'.preg_replace('[ ]', '', mb_strtolower($this->form_value ?? '', 'UTF-8'));
        if ($buttonSuffix) {
            $widgetID .= '_button';
        }
        return $widgetID;
    }

    public function &_get_form($defines, $additionalFormFields = null, $asUrl = false)
    {
        global $app_strings;
        global $currentModule;

        // Create the additional form fields with real values if they were not passed in
        if (empty($additionalFormFields) && $this->additional_form_fields) {
            foreach ($this->additional_form_fields as $key=>$value) {
                if (!empty($defines['focus']->$value)) {
                    $additionalFormFields[$key] = $defines['focus']->$value;
                } else {
                    $additionalFormFields[$key] = '';
                }
            }
        }


        if (!empty($this->module)) {
            $defines['child_module_name'] = $this->module;
        } else {
            $defines['child_module_name'] = $defines['module'];
        }

        $defines['parent_bean_name'] = get_class($defines['focus']);
        $relationship_name = $this->get_subpanel_relationship_name($defines);


        $formValues = array();

        //module_button is used to override the value of module name
        $formValues['module'] = $defines['child_module_name'];
        $formValues[strtolower($defines['parent_bean_name'])."_id"] = $defines['focus']->id;

        if (isset($defines['focus']->name)) {
            $formValues[strtolower($defines['parent_bean_name'])."_name"] = $defines['focus']->name;
            // #26451,add these fields for custom one-to-many relate field.
            if (!empty($defines['child_module_name'])) {
                $formValues[$relationship_name."_name"] = $defines['focus']->name;
                $childFocusName = !empty($GLOBALS['beanList'][$defines['child_module_name']]) ? $GLOBALS['beanList'][$defines['child_module_name']] : "";
                if (!empty($GLOBALS['dictionary'][ $childFocusName ]["fields"][$relationship_name .'_name']['id_name'])) {
                    $formValues[$GLOBALS['dictionary'][ $childFocusName ]["fields"][$relationship_name .'_name']['id_name']] = $defines['focus']->id;
                }
            }
        }

        $formValues['return_module'] = $currentModule;

        if ($currentModule == 'Campaigns') {
            $formValues['return_action'] = "DetailView";
        } else {
            $formValues['return_action'] = $defines['action'];
            if ($formValues['return_action'] == 'SubPanelViewer') {
                $formValues['return_action'] = 'DetailView';
            }
        }

        $formValues['return_id'] = $defines['focus']->id;
        $formValues['return_relationship'] = $relationship_name;
        switch (strtolower($currentModule)) {
            case 'prospects':
                $name = $defines['focus']->account_name ;
                break ;
            case 'documents':
                $name = $defines['focus']->document_name ;
                break ;
            case 'kbdocuments':
                $name = $defines['focus']->kbdocument_name ;
                break ;
            case 'leads':
            case 'contacts':
                $name = $defines['focus']->first_name . " " .$defines['focus']->last_name ;
                break ;
            default:
               $name = (isset($defines['focus']->name)) ? $defines['focus']->name : "";
        }
        $formValues['return_name'] = $name;

        // TODO: move this out and get $additionalFormFields working properly
        if (empty($additionalFormFields['parent_type'])) {
            if ($defines['focus']->object_name=='Contact') {
                $additionalFormFields['parent_type'] = 'Accounts';
            } else {
                $additionalFormFields['parent_type'] = $defines['focus']->module_dir;
            }
        }
        if (empty($additionalFormFields['parent_name'])) {
            if ($defines['focus']->object_name=='Contact') {
                $additionalFormFields['parent_name'] = $defines['focus']->account_name;
                $additionalFormFields['account_name'] = $defines['focus']->account_name;
            } else {
                $additionalFormFields['parent_name'] = $defines['focus']->name;
            }
        }
        if (empty($additionalFormFields['parent_id'])) {
            if ($defines['focus']->object_name=='Contact') {
                $additionalFormFields['parent_id'] = $defines['focus']->account_id;
                $additionalFormFields['account_id'] = $defines['focus']->account_id;
            } else {
                if ($defines['focus']->object_name=='Contract') {
                    $additionalFormFields['contract_id'] = $defines['focus']->id;
                } else {
                    $additionalFormFields['parent_id'] = $defines['focus']->id;
                }
            }
        }

        if ($defines['focus']->object_name=='Opportunity') {
            $additionalFormFields['account_id'] = $defines['focus']->account_id;
            $additionalFormFields['account_name'] = $defines['focus']->account_name;
        }

        if (
            !empty($defines['child_module_name']) &&
            $defines['child_module_name']=='Contacts' &&
            !empty($defines['parent_bean_name']) &&
            $defines['parent_bean_name']=='contact'
        ) {
            if (!empty($defines['focus']->id) && !empty($defines['focus']->name)) {
                $formValues['reports_to_id'] = $defines['focus']->id;
                $formValues['reports_to_name'] = $defines['focus']->name;
            }
        }
        $formValues['action'] = "EditView";

        if ($asUrl) {
            $returnLink = '';
            foreach ($formValues as $key => $value) {
                $returnLink .= $key.'='.$value.'&';
            }
            foreach ($additionalFormFields as $key => $value) {
                $returnLink .= $key.'='.$value.'&';
            }
            $returnLink = rtrim($returnLink, '&');

            return $returnLink;
        } else {
            $form = 'form' . $relationship_name;
            $button = '<form action="index.php" method="post" name="form" id="' . $form . "\">\n";
            foreach ($formValues as $key => $value) {
                $button .= "<input type='hidden' name='" . $key . "' value='" . $value . "' />\n";
            }

            // fill in additional form fields for all but action
            foreach ($additionalFormFields as $key => $value) {
                if ($key != 'action') {
                    $button .= "<input type='hidden' name='" . $key . "' value='" . $value . "' />\n";
                }
            }


            return $button;
        }
    }

    /** This default function is used to create the HTML for a simple button */
    public function display($defines, $additionalFormFields = null, $nonbutton = false)
    {
        $temp='';
        $inputID = $this->getWidgetId();

        if (!empty($this->acl) && ACLController::moduleSupportsACL($defines['module'])  &&  !ACLController::checkAccess($defines['module'], $this->acl, true)) {
            return $temp;
        }

        global $app_strings;

        if (isset($_REQUEST['layout_def_key']) && $_REQUEST['layout_def_key'] == 'UserEAPM') {
            // Subpanels generally don't go on the editview, so we have to handle this special
            $megaLink = $this->_get_form($defines, $additionalFormFields, true);
            $button = "<input title='$this->title' accesskey='$this->access_key' class='button' type='submit' name='$inputID' id='$inputID' value='$this->form_value' onclick='javascript:document.location=\"index.php?".$megaLink."\"; return false;'/>";
        } else {
            $button = $this->_get_form($defines, $additionalFormFields);
            $button .= "<input title='$this->title' accesskey='$this->access_key' class='button' type='submit' name='$inputID' id='$inputID' value='$this->form_value' />\n</form>";
        }

        if ($nonbutton) {
            $button = "<a onclick=''>$this->form_value";
        }
        return $button;
    }

    /**
     * Returns a string that is the JSON encoded version of the popup request.
     * Perhaps this function should be moved to a more globally accessible location?
     */
    public function _create_json_encoded_popup_request($popup_request_data)
    {
        return json_encode($popup_request_data);
    }

    /**
     * get_subpanel_relationship_name
     * Get the relationship name based on the subapnel definition
     * @param mixed $defines The subpanel definition
     */
    public function get_subpanel_relationship_name($defines)
    {
        $relationship_name = '';
        if (!empty($defines)) {
            $relationship_name = isset($defines['module']) ? $defines['module'] : '';
            $dataSource = $defines['subpanel_definition']->get_data_source_name(true);
            if (!empty($dataSource)) {
                $relationship_name = $dataSource;
                //Try to set the relationship name to the real relationship, not the link.
                if (!empty($defines['subpanel_definition']->parent_bean->field_defs[$dataSource])
                 && !empty($defines['subpanel_definition']->parent_bean->field_defs[$dataSource]['relationship'])) {
                    $relationship_name = $defines['subpanel_definition']->parent_bean->field_defs[$dataSource]['relationship'];
                }
            }
        }
        return $relationship_name;
    }
}
