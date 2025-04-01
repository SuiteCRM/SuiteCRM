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
class SugarWidgetSubPanelIcon extends SugarWidgetField
{
    public function displayHeaderCell($layout_def)
    {
        return '&nbsp;';
    }

    public function displayList(&$layout_def)
    {
        global $app_strings;
        global $app_list_strings;
        global $current_user;

        if (isset($layout_def['varname'])) {
            $key = strtoupper($layout_def['varname']);
        } else {
            $key = $this->_get_column_alias($layout_def);
            $key = strtoupper($key);
        }
        //add module image
        //add module image
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
        $action = 'DetailView';
        if (empty($layout_def['target_record_key'])) {
            $record = $layout_def['fields']['ID'];
        } else {
            $record_key = strtoupper($layout_def['target_record_key']);
            $record = $layout_def['fields'][$record_key];
        }
        $action_access = false;
        if (!empty($record) &&
            ($layout_def[$action] && !$layout_def['owner_module']
            ||  $layout_def[$action] && !ACLController::moduleSupportsACL($layout_def['owner_module'])
            || ACLController::checkAccess($layout_def['owner_module'], 'view', $layout_def['owner_id'] == $current_user->id))) {
            $action_access = true;
        }
        $icon_img_html = '<span class="suitepicon suitepicon-module-'.strtolower(str_replace('_', '-', (string) $module)).'"></span>';
        if (!empty($layout_def['attachment_image_only']) && $layout_def['attachment_image_only'] == true) {
            $ret="";
        } else {
            if ($action_access) {
                $ret = '<a href="index.php?module=' . $module . '&action=' . $action . '&record=' . $record	. '" >' . $icon_img_html . "</a>";
            } else {
                $ret = $icon_img_html;
            }
        }

        if (!empty($layout_def['image2']) &&  !empty($layout_def['image2_ext_url_field'])) {
            if (!empty($layout_def['fields'][strtoupper($layout_def['image2_ext_url_field'])])) {
                $link_url  = $layout_def['fields'][strtoupper($layout_def['image2_ext_url_field'])];
            }

            $imagePath = '';
            if ($layout_def['image2'] == '__VARIABLE') {
                if (!empty($layout_def['fields'][$key.'_ICON'])) {
                    $imagePath = $layout_def['fields'][$key.'_ICON'];
                }
            } else {
                $imagePath = $layout_def['image2'];
            }

            if (!empty($imagePath)) {
                $icon_img_html = SugarThemeRegistry::current()->getImage($imagePath . '', 'border="0"', null, null, '.gif', $imagePath);
                $ret.= (empty($link_url)) ? '' : '&nbsp;<a href="' . $link_url. '" TARGET = "_blank">' . "$icon_img_html</a>";
            }
        }
        //if requested, add attachment icon.
        if (!empty($layout_def['image2']) && !empty($layout_def['image2_url_field'])) {
            if (is_array($layout_def['image2_url_field'])) {
                //Generate file url.
                if (
                    !empty($layout_def['fields'][strtoupper($layout_def['image2_url_field']['id_field'])]) &&
                    !empty($layout_def['fields'][strtoupper($layout_def['image2_url_field']['filename_field'])])
                ) {
                    $key=$layout_def['fields'][strtoupper($layout_def['image2_url_field']['id_field'])];
                    $file=$layout_def['fields'][strtoupper($layout_def['image2_url_field']['filename_field'])];
                    $filepath="index.php?entryPoint=download&id=".$key."&type=".$layout_def['module'];
                }
            } else {
                if (!empty($layout_def['fields'][strtoupper($layout_def['image2_url_field'])])) {
                    $filepath="index.php?entryPoint=download&id=".$layout_def['fields']['ID']."&type=".$layout_def['module'];
                }
            }
            $icon_img_html = SugarThemeRegistry::current()->getImage($layout_def['image2'] . '', 'border="0"', null, null, '.gif', $layout_def['image2']);
            if ($action_access && !empty($filepath)) {
                $ret .= '<a href="' . $filepath. '" >' . "$icon_img_html</a>";
            } elseif (!empty($filepath)) {
                $ret .= $icon_img_html;
            }
        }
        // now handle attachments for Emails
        else {
            if (!empty($layout_def['module']) && $layout_def['module'] == 'Emails' && !empty($layout_def['fields']['ATTACHMENT_IMAGE'])) {
                $ret.= $layout_def['fields']['ATTACHMENT_IMAGE'];
            }
        }
        return $ret;
    }
}
