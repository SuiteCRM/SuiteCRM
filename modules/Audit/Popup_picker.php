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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once("include/upload_file.php");
require_once('include/utils/db_utils.php');
require_once('modules/Audit/Audit.php');

require_once __DIR__ . '/../../include/utils/layout_utils.php';

global $beanList, $beanFiles, $currentModule, $focus, $action, $app_strings, $app_list_strings, $current_language, $timedate, $mod_strings;
//we don't want the parent module's string file, but rather the string file specific to this subpanel

if (!isset($_REQUEST['module_name'])) {
    LoggerManager::getLogger()->warn("Popup picker needs requested module name but \$_REQUEST[module_name] is not set.");
} else {
    $bean = $beanList[$_REQUEST['module_name']];
    require_once($beanFiles[$bean]);
    $focus = new $bean;
}

#[\AllowDynamicProperties]
class Popup_Picker
{


    /*
     *
     */
    public function __construct()
    {
    }




    /**
     *
     */
    public function process_page()
    {
        global $theme;
        global $focus;
        global $mod_strings;
        global $app_strings;
        global $app_list_strings;
        global $currentModule;
        global $odd_bg;
        global $even_bg;

        global $audit;
        global $current_language;

        $auditObject = BeanFactory::newBean('Audit');
        $audit_list =  $auditObject->get_audit_list();
        $xtpl=new XTemplate('modules/Audit/Popup_picker.html');

        $xtpl->assign('MOD', $mod_strings);
        $xtpl->assign('APP', $app_strings);
        insert_popup_header($theme);

        //output header
        echo "<table width='100%' cellpadding='0' cellspacing='0'><tr><td>";

        if (!isset($focus->module_dir)) {
            LoggerManager::getLogger()->fatal("Popup picker needs module dir from focus bean but global focus is none.");
            throw new Exception('There is not selected focus bean for popup picker process page.');
        }

        $mod_strings = return_module_language($current_language, $focus->module_dir);

        $printImageURL = SugarThemeRegistry::current()->getImageURL('print.gif');

        $requestString = '';
        if (!isset($GLOBALS['request_string'])) {
            LoggerManager::getLogger()->warn("Popup picker needs focus bean but \$GLOBALS['request_string'] is not set.");
        } else {
            $requestString = $GLOBALS['request_string'];
        }

        $titleExtra = <<<EOHTML
<a href="javascript:void window.open('index.php?{$requestString}','printwin','menubar=1,status=0,resizable=1,scrollbars=1,toolbar=0,location=1')" class='utilsLink'>
<!--not_in_theme!--><img src="{$printImageURL}" alt="{$GLOBALS['app_strings']['LNK_PRINT']}"></a>
<a href="javascript:void window.open('index.php?{$requestString}','printwin','menubar=1,status=0,resizable=1,scrollbars=1,toolbar=0,location=1')" class='utilsLink'>
{$GLOBALS['app_strings']['LNK_PRINT']}
</a>
EOHTML;

        $params = array();
        $params[] = translate('LBL_MODULE_NAME', $focus->module_dir);
        $params[] = $focus->get_summary_text();
        $params[] = translate('LBL_CHANGE_LOG', 'Audit');
        echo str_replace('</div>', "<span class='utils'>$titleExtra</span></div>", (string) getClassicModuleTitle($focus->module_dir, $params, false));

        $oddRow = true;
        $audited_fields = $focus->getAuditEnabledFieldDefinitions();
        asort($audited_fields);
        $fields = '';
        $field_count = is_countable($audited_fields) ? count($audited_fields) : 0;
        $start_tag = "<table><tr><td >";
        $end_tag = "</td></tr></table>";

        if ($field_count > 0) {
            $index = 0;
            foreach ($audited_fields as $key=>$value) {
                $index++;
                $vname = '';
                if (isset($value['vname'])) {
                    $vname = $value['vname'];
                } elseif (isset($value['label'])) {
                    $vname = $value['label'];
                }
                $fields .= str_replace(':', '', (string) translate($vname, $focus->module_dir));

                if ($index < $field_count) {
                    $fields .= ", ";
                }
            }

            echo $start_tag.translate('LBL_AUDITED_FIELDS', 'Audit').$fields.$end_tag;
        } else {
            echo $start_tag.translate('LBL_AUDITED_FIELDS', 'Audit').$end_tag;
        }

        foreach ($audit_list as $audit) {
            if (empty($audit['before_value_string']) && empty($audit['after_value_string'])) {
                $before_value = $audit['before_value_text'] ?? '';
                $after_value = $audit['after_value_text'] ?? '';
            } else {
                $before_value = $audit['before_value_string'] ?? '';
                $after_value = $audit['after_value_string'] ?? '';
            }

            // Let's run the audit data through the sugar field system
            if (isset($audit['data_type'])) {
                require_once('include/SugarFields/SugarFieldHandler.php');
                $vardef = array('name'=>'audit_field','type'=>$audit['data_type']);
                $field = SugarFieldHandler::getSugarField($audit['data_type']);
                $before_value = $field->getChangeLogSmarty(array($vardef['name']=>$before_value), $vardef, array(), $vardef['name']);
                $after_value = $field->getChangeLogSmarty(array($vardef['name']=>$after_value), $vardef, array(), $vardef['name']);
            }

            $activity_fields = array(
                'ID' => $audit['id'],
                'NAME' => $audit['field_name'],
                'BEFORE_VALUE' => $before_value,
                'AFTER_VALUE' => $after_value,
                'CREATED_BY' => $audit['created_by'],
                'DATE_CREATED' => $audit['date_created'],
            );

            $xtpl->assign("ACTIVITY", $activity_fields);

            if ($oddRow) {
                //todo move to themes
                $xtpl->assign("ROW_COLOR", 'oddListRow');
                $xtpl->assign("BG_COLOR", $odd_bg);
            } else {
                //todo move to themes
                $xtpl->assign("ROW_COLOR", 'evenListRow');
                $xtpl->assign("BG_COLOR", $even_bg);
            }
            $oddRow = !$oddRow;

            $xtpl->parse("audit.row");
            // Put the rows in.
        }//end foreach

        $xtpl->parse("audit");
        $xtpl->out("audit");
        insert_popup_footer();
    }
} // end of class Popup_Picker
