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
class ACLJSController
{
    public function __construct($module, $form='', $is_owner=false)
    {
        $this->module = $module;
        $this->is_owner = $is_owner;
        $this->form = $form;
    }

    public function getJavascript()
    {
        global $action;
        if (!ACLController::moduleSupportsACL($this->module)) {
            return '';
        }
        $script = "<SCRIPT>\n//BEGIN ACL JAVASCRIPT\n";

        if ($action == 'DetailView') {
            if (!ACLController::checkAccess($this->module, 'edit', $this->is_owner)) {
                $script .= <<<EOQ
						if(typeof(document.DetailView) != 'undefined'){
							if(typeof(document.DetailView.elements['Edit']) != 'undefined'){
								document.DetailView.elements['Edit'].disabled = 'disabled';
							}
							if(typeof(document.DetailView.elements['Duplicate']) != 'undefined'){
								document.DetailView.elements['Duplicate'].disabled = 'disabled';
							}
						}
EOQ;
            }
            if (!ACLController::checkAccess($this->module, 'delete', $this->is_owner)) {
                $script .= <<<EOQ
						if(typeof(document.DetailView) != 'undefined'){
							if(typeof(document.DetailView.elements['Delete']) != 'undefined'){
								document.DetailView.elements['Delete'].disabled = 'disabled';
							}
						}
EOQ;
            }
        }
        if (file_exists('modules/'. $this->module . '/metadata/acldefs.php')) {
            include('modules/'. $this->module . '/metadata/acldefs.php');

            foreach ($acldefs[$this->module]['forms'] as $form_name=>$form) {
                foreach ($form as $field_name=>$field) {
                    if ($field['app_action'] == $action) {
                        switch ($form_name) {
                            case 'by_id':
                                $script .= $this->getFieldByIdScript($field_name, $field);
                                break;
                            case 'by_name':
                                $script .= $this->getFieldByNameScript($field_name, $field);
                                break;
                            default:
                                $script .= $this->getFieldByFormScript($form_name, $field_name, $field);
                                break;
                        }
                    }
                }
            }
        }
        $script .=  '</SCRIPT>';

        return $script;
    }

    public function getHTMLValues($def)
    {
        $return_array = array();
        switch ($def['display_option']) {
            case 'clear_link':
                $return_array['href']= "#";
                $return_array['className']= "nolink";
                break;
            default:
                $return_array[$def['display_option']] = $def['display_option'];
                break;

        }
        return $return_array;
    }

    public function getFieldByIdScript($name, $def)
    {
        $script = '';
        if (!ACLController::checkAccess($def['module'], $def['action_option'], true)) {
            foreach ($this->getHTMLValues($def) as $key=>$value) {
                $script .=  "\nif(document.getElementById('$name'))document.getElementById('$name')." . $key . '="' .$value. '";'. "\n";
            }
        }
        return $script;
    }

    public function getFieldByNameScript($name, $def)
    {
        $script = '';
        if (!ACLController::checkAccess($def['module'], $def['action_option'], true)) {
            foreach ($this->getHTMLValues($def) as $key=>$value) {
                $script .=  <<<EOQ
			var aclfields = document.getElementsByName('$name');
			for(var i in aclfields){
				aclfields[i].$key = '$value';
			}
EOQ;
            }
        }
        return $script;
    }

    public function getFieldByFormScript($form, $name, $def)
    {
        $script = '';


        if (!ACLController::checkAccess($def['module'], $def['action_option'], true)) {
            foreach ($this->getHTMLValues($def) as $key=>$value) {
                $script .= "\nif(typeof(document.$form.$name.$key) != 'undefined')\n document.$form.$name.".$key . '="' .$value. '";';
            }
        }
        return $script;
    }
}
