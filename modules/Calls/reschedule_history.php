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

require_once('modules/Calls_Reschedule/Calls_Reschedule.php');

function reschedule_history($focus, $field, $value, $view)
{
    global $app_list_strings,$mod_strings;

    if ($view == 'DetailView') {
        $html = '';
        $html .= '<ul id="history_list">';

        $query = "SELECT calls_reschedule.id FROM calls_reschedule JOIN users ON calls_reschedule.modified_user_id = users.id WHERE call_id='".$focus->id."' ORDER BY calls_reschedule.date_entered DESC";
        
        $result = $focus->db->query($query);
        
        $reschedule = BeanFactory::newBean('Calls_Reschedule');

        while ($row = $focus->db->fetchByAssoc($result)) {
            $reschedule->retrieve($row['id']);
                       
            $html .= '<li>'.$app_list_strings["call_reschedule_dom"][$reschedule->reason].' - '.$reschedule->date_entered.' '.$mod_strings['LBL_RESCHEDULED_BY'].' '.$reschedule->created_by_name.'</li>';
        }

        $html .= '</ul>';

        return $html;
    }
}

function reschedule_count($focus, $field, $value, $view)
{
    $query = "SELECT COUNT(*) FROM calls_reschedule WHERE call_id='".$focus->id."'";
    $result = $focus->db->getOne($query);

    $focus->reschedule_count = $result;
}
