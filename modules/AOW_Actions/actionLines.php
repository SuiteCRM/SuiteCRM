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


function display_action_lines(SugarBean $focus, $field, $value, $view)
{

    global $locale, $app_list_strings, $mod_strings;

    $html = '';

    if (!is_file('cache/jsLanguage/AOW_Actions/' . $GLOBALS['current_language'] . '.js')) {
        require_once('include/language/jsLanguage.php');
        jsLanguage::createModuleStringsCache('AOW_Actions', $GLOBALS['current_language']);
    }
    $html .= '<script src="cache/jsLanguage/AOW_Actions/'. $GLOBALS['current_language'] . '.js"></script>';

    $app_list_actions = [];
    $app_list_actions[''] = '';

    if ($view == 'EditView') {
        $html .= '<script src="modules/AOW_Actions/actionLines.js"></script>';

        $aow_actions_list = array();

        include_once('modules/AOW_Actions/actions.php');

        foreach ($aow_actions_list as $action_value) {
            $action_name = 'action'.$action_value;

            if (file_exists('custom/modules/AOW_Actions/actions/'.$action_name.'.php')) {
                require_once('custom/modules/AOW_Actions/actions/'.$action_name.'.php');
            } elseif (file_exists('modules/AOW_Actions/actions/'.$action_name.'.php')) {
                require_once('modules/AOW_Actions/actions/'.$action_name.'.php');
            } else {
                continue;
            }

            $action = new $action_name();
            foreach ($action->loadJS() as $js_file) {
                $html .= '<script src="'.$js_file.'"></script>';
            }

            $app_list_actions[$action_value] = translate('LBL_'.strtoupper($action_value), 'AOW_Actions');
        }

        $html .= '<input type="hidden" name="app_list_actions" id="app_list_actions" value="'.get_select_options_with_id($app_list_actions, '').'">';

        $html .= "<table style='padding-top: 10px; padding-bottom:10px;' id='actionLines'></table>";

        $html .= "<div style='padding-top: 10px; padding-bottom:10px;'>";
        $html .= "<input type=\"button\" tabindex=\"116\" class=\"button\" value=\"".$mod_strings['LBL_ADD_ACTION']."\" id=\"btn_ActionLine\" onclick=\"insertActionLine()\" disabled/>";
        $html .= "</div>";

        if (isset($focus->flow_module) && $focus->flow_module != '') {
            $html .= "<script>document.getElementById('btn_ActionLine').disabled = '';</script>";
            if ($focus->id !== '') {
                $idQuoted = $focus->db->quoted($focus->id);
                $sql = 'SELECT id FROM aow_actions WHERE aow_workflow_id = ' . $idQuoted . ' AND deleted = 0 ORDER BY action_order ASC';
                $result = $focus->db->query($sql);

                while ($row = $focus->db->fetchByAssoc($result)) {
                    $action_name = BeanFactory::newBean('AOW_Actions');
                    $action_name->retrieve($row['id']);
                    $action_item = json_encode($action_name->toArray());

                    $html .= "<script>
                            loadActionLine(".$action_item.");
                        </script>";
                }
            }
        }
    } elseif ($view == 'DetailView') {
        $html .= "<table border='0' width='100%' cellpadding='0' cellspacing='0'>";
        $idQuoted = $focus->db->quoted($focus->id);
        $sql = 'SELECT id FROM aow_actions WHERE aow_workflow_id = ' . $idQuoted . ' AND deleted = 0 ORDER BY action_order ASC';
        $result = $focus->db->query($sql);

        while ($row = $focus->db->fetchByAssoc($result)) {
            $action_name = BeanFactory::newBean('AOW_Actions');
            $action_name->retrieve($row['id']);

            $html .= "<tr><td>". $action_name->action_order ."</td><td>".$action_name->name."</td><td>". translate('LBL_'.strtoupper($action_name->action), 'AOW_Actions')."</td></tr>";
        }
        $html .= "</table>";
    }
    return $html;
}
