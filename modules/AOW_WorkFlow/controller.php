<?php
/**
 * Advanced OpenWorkflow, Automating SugarCRM.
 * @package Advanced OpenWorkflow for SugarCRM
 * @copyright SalesAgility Ltd http://www.salesagility.com
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU AFFERO GENERAL PUBLIC LICENSE
 * along with this program; if not, see http://www.gnu.org/licenses
 * or write to the Free Software Foundation,Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301  USA
 *
 * @author SalesAgility <info@salesagility.com>
 */


require_once("modules/AOW_WorkFlow/aow_utils.php");

#[\AllowDynamicProperties]
class AOW_WorkFlowController extends SugarController
{
    protected function action_getModuleFields()
    {
        if (!empty($_REQUEST['aow_module']) && $_REQUEST['aow_module'] != '') {
            if (isset($_REQUEST['rel_field']) && $_REQUEST['rel_field'] != '') {
                $module = getRelatedModule($_REQUEST['aow_module'], $_REQUEST['rel_field']);
            } else {
                $module = $_REQUEST['aow_module'];
            }
            $override = array();
            if (isset($_REQUEST['override']) && is_array($_REQUEST['override'])) {
                $override = $_REQUEST['override'];
            }
            $view = isset($_REQUEST['view']) ? $_REQUEST['view'] : 'EditView';
            $value = isset($_REQUEST['aow_value']) ? $_REQUEST['aow_value'] : '';

            echo getModuleFields($module, $view, $value, array(), $override);
        }
        die;
    }

    protected function action_getRelatedModule()
    {
        if (!empty($_REQUEST['aow_module']) && $_REQUEST['aow_module'] != '') {
            if (isset($_REQUEST['rel_field']) &&  $_REQUEST['rel_field'] != '') {
                $module = getRelatedModule($_REQUEST['aow_module'], $_REQUEST['rel_field']);
            } else {
                $module = $_REQUEST['aow_module'];
            }
            echo htmlspecialchars((string) $module);
        }
        die;
    }

    protected function action_getModuleRelationships()
    {
        if (!empty($_REQUEST['aow_module']) && $_REQUEST['aow_module'] != '') {
            if (isset($_REQUEST['rel_field']) &&  $_REQUEST['rel_field'] != '') {
                $module = getRelatedModule($_REQUEST['aow_module'], $_REQUEST['rel_field']);
            } else {
                $module = $_REQUEST['aow_module'];
            }
            echo getModuleRelationships($module);
        }
        die;
    }


    protected function action_getModuleOperatorField()
    {
        global $app_list_strings, $beanFiles, $beanList;

        if (isset($_REQUEST['rel_field']) &&  $_REQUEST['rel_field'] != '') {
            $module = getRelatedModule($_REQUEST['aow_module'], $_REQUEST['rel_field']);
        } else {
            $module = $_REQUEST['aow_module'];
        }
        $fieldname = $_REQUEST['aow_fieldname'];
        $aow_field = $_REQUEST['aow_newfieldname'];

        if (isset($_REQUEST['view'])) {
            $view = $_REQUEST['view'];
        } else {
            $view= 'EditView';
        }

        if (isset($_REQUEST['aow_value'])) {
            $value = $_REQUEST['aow_value'];
        } else {
            $value = '';
        }


        require_once($beanFiles[$beanList[$module]]);
        $focus = new $beanList[$module];
        $vardef = $focus->getFieldDefinition($fieldname);

        if ($vardef) {
            switch ($vardef['type']) {
                case 'double':
                case 'decimal':
                case 'float':
                case 'currency':
                $valid_opp = array('Equal_To','Not_Equal_To','Greater_Than','Less_Than','Greater_Than_or_Equal_To','Less_Than_or_Equal_To','is_null');
                    break;
                case 'uint':
                case 'ulong':
                case 'long':
                case 'short':
                case 'tinyint':
                case 'int':
                $valid_opp = array('Equal_To','Not_Equal_To','Greater_Than','Less_Than','Greater_Than_or_Equal_To','Less_Than_or_Equal_To','is_null');
                    break;
                case 'date':
                case 'datetime':
                case 'datetimecombo':
                $valid_opp = array('Equal_To','Not_Equal_To','Greater_Than','Less_Than','Greater_Than_or_Equal_To','Less_Than_or_Equal_To','is_null');
                    break;
                case 'enum':
                case 'multienum':
                $valid_opp = array('Equal_To','Not_Equal_To','is_null');
                    break;
                default:
                $valid_opp = array('Equal_To','Not_Equal_To','Contains', 'Starts_With', 'Ends_With','is_null');
                    break;
            }

            foreach ($app_list_strings['aow_operator_list'] as $key => $keyValue) {
                if (!in_array($key, $valid_opp)) {
                    unset($app_list_strings['aow_operator_list'][$key]);
                }
            }



            $app_list_strings['aow_operator_list'];
            if ($view == 'EditView') {
                echo "<select type='text' name='$aow_field' id='$aow_field' title='' tabindex='116'>". get_select_options_with_id($app_list_strings['aow_operator_list'], $value) ."</select>";
            } else {
                echo $app_list_strings['aow_operator_list'][$value];
            }
        }
        die;
    }

    protected function action_getFieldTypeOptions()
    {
        global $app_list_strings, $beanFiles, $beanList;

        if (isset($_REQUEST['rel_field']) &&  $_REQUEST['rel_field'] != '') {
            $module = getRelatedModule($_REQUEST['aow_module'], $_REQUEST['rel_field']);
        } else {
            $module = $_REQUEST['aow_module'];
        }
        $fieldname = $_REQUEST['aow_fieldname'];
        $aow_field = $_REQUEST['aow_newfieldname'];

        if (isset($_REQUEST['view'])) {
            $view = $_REQUEST['view'];
        } else {
            $view= 'EditView';
        }

        if (isset($_REQUEST['aow_value'])) {
            $value = $_REQUEST['aow_value'];
        } else {
            $value = '';
        }


        require_once($beanFiles[$beanList[$module]]);
        $focus = new $beanList[$module];
        $vardef = $focus->getFieldDefinition($fieldname);

        switch ($vardef['type']) {
            case 'double':
            case 'decimal':
            case 'float':
            case 'currency':
                $valid_opp = array('Value','Field','Any_Change');
                break;
            case 'uint':
            case 'ulong':
            case 'long':
            case 'short':
            case 'tinyint':
            case 'int':
                $valid_opp = array('Value','Field','Any_Change');
                break;
            case 'date':
            case 'datetime':
            case 'datetimecombo':
                $valid_opp = array('Value','Field','Any_Change','Date');
                break;
            case 'enum':
            case 'dynamicenum':
            case 'multienum':
                $valid_opp = array('Value','Field','Any_Change', 'Multi');
                break;
            case 'relate':
            case 'id':
                $valid_opp = array('Value','Field','Any_Change', 'SecurityGroup');
                break;
            default:
                $valid_opp = array('Value','Field','Any_Change');
                break;
        }

        if (!file_exists('modules/SecurityGroups/SecurityGroup.php')) {
            unset($app_list_strings['aow_condition_type_list']['SecurityGroup']);
        }
        foreach ($app_list_strings['aow_condition_type_list'] as $key => $keyValue) {
            if (!in_array($key, $valid_opp)) {
                unset($app_list_strings['aow_condition_type_list'][$key]);
            }
        }

        if ($view == 'EditView') {
            echo "<select type='text'  name='$aow_field' id='$aow_field' title='' tabindex='116'>". get_select_options_with_id($app_list_strings['aow_condition_type_list'], $value) ."</select>";
        } else {
            echo $app_list_strings['aow_condition_type_list'][$value];
        }
        die;
    }

    protected function action_getActionFieldTypeOptions()
    {
        global $app_list_strings, $beanFiles, $beanList;

        $module = $_REQUEST['aow_module'];
        $fieldname = $_REQUEST['aow_fieldname'];
        $aow_field = $_REQUEST['aow_newfieldname'];

        if (isset($_REQUEST['view'])) {
            $view = $_REQUEST['view'];
        } else {
            $view= 'EditView';
        }

        if (isset($_REQUEST['aow_value'])) {
            $value = $_REQUEST['aow_value'];
        } else {
            $value = '';
        }


        require_once($beanFiles[$beanList[$module]]);
        $focus = new $beanList[$module];
        $vardef = $focus->getFieldDefinition($fieldname);

        $vardefType = $vardef['type'] ?? '';

        switch ($vardefType) {
            case 'double':
            case 'decimal':
            case 'float':
            case 'currency':
                $valid_opp = array('Value','Field');
                break;
            case 'uint':
            case 'ulong':
            case 'long':
            case 'short':
            case 'tinyint':
            case 'int':
                $valid_opp = array('Value','Field');
                break;
            case 'date':
            case 'datetime':
            case 'datetimecombo':
                $valid_opp = array('Value','Field', 'Date');
                break;
            case 'enum':
            case 'multienum':
                $valid_opp = array('Value','Field');
                break;
            case 'relate':
                $valid_opp = array('Value','Field');
                if ($vardef['module'] == 'Users') {
                    $valid_opp = array('Value','Field','Round_Robin','Least_Busy','Random');
                }
                break;
            default:
                $valid_opp = array('Value','Field');
                break;
        }

        foreach ($app_list_strings['aow_action_type_list'] as $key => $keyValue) {
            if (!in_array($key, $valid_opp)) {
                unset($app_list_strings['aow_action_type_list'][$key]);
            }
        }

        if ($view == 'EditView') {
            echo "<select type='text'  name='$aow_field' id='$aow_field' title='' tabindex='116'>". get_select_options_with_id($app_list_strings['aow_action_type_list'], $value) ."</select>";
        } else {
            echo $app_list_strings['aow_action_type_list'][$value];
        }
        die;
    }

    protected function action_getModuleFieldType()
    {
        if (isset($_REQUEST['rel_field']) &&  $_REQUEST['rel_field'] != '') {
            $rel_module = getRelatedModule($_REQUEST['aow_module'], $_REQUEST['rel_field']);
        } else {
            $rel_module = $_REQUEST['aow_module'];
        }
        $module = $_REQUEST['aow_module'];
        $fieldname = $_REQUEST['aow_fieldname'];
        $aow_field = $_REQUEST['aow_newfieldname'];

        if (isset($_REQUEST['view'])) {
            $view = $_REQUEST['view'];
        } else {
            $view= 'EditView';
        }

        if (isset($_REQUEST['aow_value'])) {
            $value = $_REQUEST['aow_value'];
        } else {
            $value = '';
        }

        $params = [];
        $isValueSet = $_REQUEST['is_value_set'] ?? 'false';
        if ($isValueSet === 'false'){
            $params['value_set'] = false;
        } else{
            $params['value_set'] = true;
        }

        switch ($_REQUEST['aow_type']) {
            case 'Field':
                if (isset($_REQUEST['alt_module']) && $_REQUEST['alt_module'] != '') {
                    $module = $_REQUEST['alt_module'];
                }
                if ($view == 'EditView') {
                    echo "<select type='text'  name='$aow_field' id='$aow_field ' title='' tabindex='116'>". getModuleFields($module, $view, $value, getValidFieldsTypes($module, $fieldname)) ."</select>";
                } else {
                    echo getModuleFields($module, $view, $value);
                }
                break;
            case 'Any_Change':
                echo '';
                break;
            case 'Date':
                echo getDateField($module, $aow_field, $view, $value, false);
                break;
            case 'Multi':
                echo getModuleField($rel_module, $fieldname, $aow_field, $view, $value, 'multienum');
                break;
            case 'SecurityGroup':
                $module = 'Accounts';
                $fieldname = 'SecurityGroups';
                // no break
            case 'Value':
            default:
                echo getModuleField($rel_module, $fieldname, $aow_field, $view, $value, '', '', $params);
                break;
        }
        die;
    }

    protected function action_getModuleFieldTypeSet()
    {
        $module = $_REQUEST['aow_module'];
        $fieldname = $_REQUEST['aow_fieldname'];
        $aow_field = $_REQUEST['aow_newfieldname'];

        if (isset($_REQUEST['view'])) {
            $view = $_REQUEST['view'];
        } else {
            $view= 'EditView';
        }

        if (isset($_REQUEST['aow_value'])) {
            $value = $_REQUEST['aow_value'];
        } else {
            $value = '';
        }

        switch ($_REQUEST['aow_type']) {
            case 'Field':
                $valid_fields = getValidFieldsTypes($module, $fieldname);
                if (isset($_REQUEST['alt_module']) && $_REQUEST['alt_module'] != '') {
                    $module = $_REQUEST['alt_module'];
                }
                if ($view == 'EditView') {
                    echo "<select type='text'  name='$aow_field' id='$aow_field ' title='' tabindex='116'>". getModuleFields($module, $view, $value, $valid_fields) ."</select>";
                } else {
                    echo getModuleFields($module, $view, $value);
                }
                break;
            case 'Date':
                if (isset($_REQUEST['alt_module']) && $_REQUEST['alt_module'] != '') {
                    $module = $_REQUEST['alt_module'];
                }
                echo getDateField($module, $aow_field, $view, $value);
                break;
            case 'Round_Robin':
            case 'Least_Busy':
            case 'Random':
                echo getAssignField($aow_field, $view, $value);
                break;
            case 'Value':
            default:
                echo getModuleField($module, $fieldname, $aow_field, $view, $value);
                break;
        }
        die;
    }

    protected function action_getModuleField()
    {
        if (isset($_REQUEST['view'])) {
            $view = $_REQUEST['view'];
        } else {
            $view= 'EditView';
        }

        if (isset($_REQUEST['aow_value'])) {
            $value = $_REQUEST['aow_value'];
        } else {
            $value = '';
        }

        echo getModuleField($_REQUEST['aow_module'], $_REQUEST['aow_fieldname'], $_REQUEST['aow_newfieldname'], $view, $value);
        die;
    }

    protected function action_getRelFieldTypeSet()
    {
        $module = $_REQUEST['aow_module'];
        $fieldname = $_REQUEST['aow_fieldname'];
        $aow_field = $_REQUEST['aow_newfieldname'];

        if (isset($_REQUEST['view'])) {
            $view = $_REQUEST['view'];
        } else {
            $view= 'EditView';
        }

        if (isset($_REQUEST['aow_value'])) {
            $value = $_REQUEST['aow_value'];
        } else {
            $value = '';
        }

        switch ($_REQUEST['aow_type']) {
            case 'Field':
                if (isset($_REQUEST['alt_module']) && $_REQUEST['alt_module'] != '') {
                    $module = $_REQUEST['alt_module'];
                }
                if ($view == 'EditView') {
                    echo "<select type='text'  name='$aow_field' id='$aow_field ' title='' tabindex='116'>". getModuleFields($module, $view, $value) ."</select>";
                } else {
                    echo getModuleFields($module, $view, $value);
                }
                break;
            case 'Value':
            default:
                echo getModuleField($module, $fieldname, $aow_field, $view, $value);
                break;
        }
        die;
    }

    protected function action_getRelActionFieldTypeOptions()
    {
        global $app_list_strings, $beanFiles, $beanList;

        $module = $_REQUEST['aow_module'];
        $alt_module = $_REQUEST['alt_module'];
        $fieldname = $_REQUEST['aow_fieldname'];
        $aow_field = $_REQUEST['aow_newfieldname'];

        if (isset($_REQUEST['view'])) {
            $view = $_REQUEST['view'];
        } else {
            $view= 'EditView';
        }

        if (isset($_REQUEST['aow_value'])) {
            $value = $_REQUEST['aow_value'];
        } else {
            $value = '';
        }


        require_once($beanFiles[$beanList[$module]]);
        $focus = new $beanList[$module];
        $vardef = $focus->getFieldDefinition($fieldname);


        /*if($vardef['module'] == $alt_module){
            $valid_opp = array('Value','Field');
        }
        else{
            $valid_opp = array('Value');
        }*/
        $valid_opp = array('Value','Field');

        foreach ($app_list_strings['aow_rel_action_type_list'] as $key => $keyValue) {
            if (!in_array($key, $valid_opp)) {
                unset($app_list_strings['aow_rel_action_type_list'][$key]);
            }
        }

        if ($view == 'EditView') {
            echo "<select type='text'  name='$aow_field' id='$aow_field' title='' tabindex='116'>". get_select_options_with_id($app_list_strings['aow_rel_action_type_list'], $value) ."</select>";
        } else {
            echo $app_list_strings['aow_rel_action_type_list'][$value];
        }
        die;
    }

    protected function action_getAction()
    {
        global $beanList, $beanFiles;

        $action_name = 'action'.$_REQUEST['aow_action'];
        $line = $_REQUEST['line'];

        if ($_REQUEST['aow_module'] == '' || !isset($beanList[$_REQUEST['aow_module']])) {
            echo '';
            die;
        }

        if (file_exists('custom/modules/AOW_Actions/actions/'.$action_name.'.php')) {
            require_once('custom/modules/AOW_Actions/actions/'.$action_name.'.php');
        } else {
            if (file_exists('modules/AOW_Actions/actions/'.$action_name.'.php')) {
                require_once('modules/AOW_Actions/actions/'.$action_name.'.php');
            } else {
                echo '';
                die;
            }
        }

        $custom_action_name = "custom" . $action_name;
        if (class_exists($custom_action_name)) {
            $action_name = $custom_action_name;
        }

        $id = '';
        $params = array();
        if (isset($_REQUEST['id']) && strlen(trim($_REQUEST['id'])) > 0 ) {
            require_once('modules/AOW_Actions/AOW_Action.php');
            $aow_action = BeanFactory::newBean('AOW_Actions');
            $aow_action->retrieve($_REQUEST['id']);
            $id = $aow_action->id;
            $params = unserialize(base64_decode($aow_action->parameters ?? ''), ['allowed_classes' => false]);

            if ($params === false) {
                $params = [];
            }
        }

        $action = new $action_name($id);

        require_once($beanFiles[$beanList[$_REQUEST['aow_module']]]);
        $bean = new $beanList[$_REQUEST['aow_module']];
        echo $action->edit_display($line, $bean, $params);
        die;
    }

    protected function action_getEmailField()
    {
        $module = $_REQUEST['aow_module'];
        $aow_field = $_REQUEST['aow_newfieldname'];

        if (isset($_REQUEST['view'])) {
            $view = $_REQUEST['view'];
        } else {
            $view= 'EditView';
        }

        if (isset($_REQUEST['aow_value'])) {
            $value = $_REQUEST['aow_value'];
        } else {
            $value = '';
        }

        switch ($_REQUEST['aow_type']) {
            case 'Record Email':
                echo '';
                break;
            case 'Related Field':
                $rel_field_list = getRelatedEmailableFields($module);
                if ($view == 'EditView') {
                    echo "<select type='text'  name='$aow_field' id='$aow_field' title='' tabindex='116'>". get_select_options_with_id($rel_field_list, $value) ."</select>";
                } else {
                    echo $rel_field_list[$value];
                }
                break;
            case 'Specify User':
                echo getModuleField('Accounts', 'assigned_user_name', $aow_field, $view, $value);
                break;
            case 'Users':
                echo getAssignField($aow_field, $view, $value);
                break;
            case 'Email Address':
            default:
                if ($view == 'EditView') {
                    echo "<input type='text' name='$aow_field' id='$aow_field' size='25' title='' tabindex='116' value='$value'>";
                } else {
                    echo $value;
                }
                break;
        }
        die;
    }


    protected function action_testFlow()
    {
        echo 'Started<br />';
        require_once('modules/AOW_WorkFlow/AOW_WorkFlow.php');
        $workflow = BeanFactory::newBean('AOW_WorkFlow');

        if ($workflow->run_flows()) {
            echo 'PASSED';
        }
    }
}
