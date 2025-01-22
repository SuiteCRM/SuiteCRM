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
class SugarWidgetSubPanelDetailViewLink extends SugarWidgetField
{
    public function displayList(&$layout_def)
    {
        global $focus;

        $module = '';
        $record = '';

        if (isset($layout_def['varname'])) {
            $key = strtoupper($layout_def['varname']);
        } else {
            $key = $this->_get_column_alias($layout_def);
            $key = strtoupper($key);
        }
        if (empty($layout_def['fields'][$key])) {
            return "";
        } else {
            $value = $layout_def['fields'][$key];
        }


        if (empty($layout_def['target_record_key'])) {
            $record = $layout_def['fields']['ID'];
        } else {
            $record_key = strtoupper($layout_def['target_record_key']);
            $record = $layout_def['fields'][$record_key];
        }

        if (!empty($layout_def['target_module_key'])) {
            if (!empty($layout_def['fields'][strtoupper($layout_def['target_module_key'])])) {
                $module=$layout_def['fields'][strtoupper($layout_def['target_module_key'])];
            }
        }

        if (empty($module)) {
            if (empty($layout_def['target_module'])) {
                $module = $layout_def['module'];
            } else {
                $module = $layout_def['target_module'];
            }
        }

        //links to email module now need additional information.
        //this is to resolve the information about the target of the emails. necessitated by feature that allow
        //only on email record for the whole campaign.
        $parent='';
        if (!empty($layout_def['parent_info'])) {
            if (!empty($focus)) {
                $parent="&parent_id=".$focus->id;
                $parent.="&parent_module=".$focus->module_dir;
            }
        } else {
            if (!empty($layout_def['parent_id'])) {
                if (isset($layout_def['fields'][strtoupper($layout_def['parent_id'])])) {
                    $parent.="&parent_id=".$layout_def['fields'][strtoupper($layout_def['parent_id'])];
                }
            }
            if (!empty($layout_def['parent_module'])) {
                if (isset($layout_def['fields'][strtoupper($layout_def['parent_module'])])) {
                    $parent.="&parent_module=".$layout_def['fields'][strtoupper($layout_def['parent_module'])];
                }
            }
        }

        $action = 'DetailView';
        if($module === "Emails"){
            $email = BeanFactory::getBean($module, $record);
            if($email->status === "draft"){
                $action = 'DetailDraftView';
            }
        }
        $value = $layout_def['fields'][$key];
        global $current_user;

        $detailView = $layout_def['DetailView'] ?? '';
        $ownerId = $layout_def['owner_id'] ?? '';
        $ownerModule = $layout_def['owner_module'] ?? '';
        if (!empty($record) &&
            ($detailView && !$layout_def['owner_module']
            ||  $detailView && !ACLController::moduleSupportsACL($layout_def['owner_module'])
            || ACLController::checkAccess($ownerModule, 'view', $ownerId == $current_user->id))) {
            $link = ajaxLink("index.php?module=$module&action=$action&record={$record}{$parent}");
            if ($module == 'EAPM') {
                $link = "index.php?module=$module&action=$action&record={$record}{$parent}";
            }
            return '<a href="' . $link . '" >'."$value</a>";
        } else {
            return $value;
        }
    }
}
