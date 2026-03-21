<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2011 - 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */



require_once('modules/AOW_Actions/actions/actionCreateRecord.php');
class actionModifyRecord extends actionCreateRecord
{
    public function __construct($id = '')
    {
        parent::__construct($id);
    }

    public function loadJS()
    {
        return parent::loadJS();
    }

    public function edit_display($line, ?SugarBean $bean = null, $params = array())
    {
        require_once("modules/AOW_WorkFlow/aow_utils.php");

        $modules = getModuleRelationships($bean->module_dir, 'EditView', $params['rel_type'] ?? '');

        $html = "<input type='hidden' name='aow_actions_param[".$line."][record_type]' id='aow_actions_param_record_type".$line."' value='' />";
        $html .= "<table border='0' cellpadding='0' cellspacing='0' width='100%' data-workflow-action='modify-record'>";
        $html .= "<tr>";
        $html .= '<td id="name_label" scope="row" valign="top">'.translate("LBL_RECORD_TYPE", "AOW_Actions").':<span class="required">*</span>&nbsp;&nbsp;';
        $html .= "<select name='aow_actions_param[".$line."][rel_type]' id='aow_actions_param_rel_type".$line."'  onchange='show_mrModuleFields($line);'>".$modules."</select></td>";
        $html .= "</tr>";
        $html .= "<tr>";
        $html .= '<td colspan="4" scope="row"><table id="crLine' . $line . '_table" width="100%" class="lines"></table></td>';
        $html .= "</tr>";
        $html .= "<tr>";
        $html .= '<td colspan="4" scope="row"><input type="button" tabindex="116" class="button" value="'.translate("LBL_ADD_FIELD", "AOW_Actions").'" id="addcrline'.$line.'" onclick="add_crLine('.$line.')" /></td>';
        $html .= "</tr>";
        $html .= "<tr>";
        $html .= '<td colspan="4" scope="row"><table id="crRelLine'.$line.'_table" width="100%" class="relationship"></table></td>';
        $html .= "</tr>";
        $html .= "<tr>";
        $html .= '<td colspan="4" scope="row"><input type="button" tabindex="116" class="button" value="'.translate("LBL_ADD_RELATIONSHIP", "AOW_Actions").'" id="addcrrelline'.$line.'" onclick="add_crRelLine('.$line.')" /></td>';
        $html .= "</tr>";



        $html .= <<<EOS
        <script id ='aow_script$line'>
            function updateFlowModule(){
                var mod = document.getElementById('flow_module').value;
                document.getElementById('aow_actions_param_record_type$line').value = mod;
                //cr_module[$line] = mod;
                //show_crModuleFields($line);
            }
            document.getElementById('flow_module').addEventListener("change", updateFlowModule, false);
            updateFlowModule($line);
EOS;


        $module = getRelatedModule($bean->module_name, $params['rel_type'] ?? '');
        $html .= "cr_module[" . $line . "] = \"" . $module . "\";";
        $html .= "cr_fields[" . $line . "] = \"" . trim(preg_replace(
            '/\s+/',
            ' ',
            (string) getModuleFields($module, 'EditView', '', array(), array('email1', 'email2'))
        )) . "\";";
        $html .= "cr_relationships[".$line."] = \"".trim(preg_replace('/\s+/', ' ', (string) getModuleRelationships($module)))."\";";
        if ($params && array_key_exists('field', $params)) {
            foreach ($params['field'] as $key => $field) {
                if (is_array($params['value'][$key])) {
                    $params['value'][$key] = json_encode($params['value'][$key]);
                }

                $html .= "load_crline('".$line."','".$field."','".str_replace(array("\r\n","\r","\n"), " ", (string) $params['value'][$key])."','".$params['value_type'][$key]."');";
            }
        }
        if (isset($params['rel'])) {
            foreach ($params['rel'] as $key => $field) {
                if (is_array($params['rel_value'][$key])) {
                    $params['rel_value'][$key] = json_encode($params['rel_value'][$key]);
                }

                $html .= "load_crrelline('".$line."','".$field."','".$params['rel_value'][$key]."','".$params['rel_value_type'][$key]."');";
            }
        }
        $html .= "</script>";
        return $html;
    }

    public function run_action(SugarBean $bean, $params = array(), $in_save=false)
    {
        if (isset($params['rel_type']) && $params['rel_type'] != '' && $bean->module_dir != $params['rel_type']) {
            $relatedFields = $bean->get_linked_fields();
            $field = $relatedFields[$params['rel_type']];
            if (!isset($field['module']) || $field['module'] == '') {
                $field['module'] = getRelatedModule($bean->module_dir, $field['name']);
            }
            $linkedBeans = $bean->get_linked_beans($field['name'], $field['module']);
            if ($linkedBeans) {
                foreach ($linkedBeans as $linkedBean) {
                    $this->set_record($linkedBean, $bean, $params, false);
                    $this->set_relationships($linkedBean, $bean, $params);
                }
            }
        } else {
            $this->set_record($bean, $bean, $params, $in_save);
            $this->set_relationships($bean, $bean, $params);
        }
        return true;
    }
}
