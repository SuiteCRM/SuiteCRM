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

function display_email_lines($focus, $field, $value, $view)
{
    global $app_list_strings;
    $aorEmailToList = $app_list_strings['aor_email_to_list'] ?? '';
    $params = unserialize(base64_decode($value),['allowed_classes' => false]);

    if ($view == 'EditView') {
        $html = '<script src="modules/AOR_Scheduled_Reports/emailRecipients.js"></script>';
        $html .= '<input type="hidden" name="aor_email_type_list" id="aor_email_type_list" value="' . get_select_options_with_id($app_list_strings['aor_email_type_list'], '') . '">
				  <input type="hidden" name="aor_email_to_list" id="aor_email_to_list" value="' . get_select_options_with_id($aorEmailToList, '') . '">';

        $html .= '<button type="button" class="button" onclick="add_emailLine()"><img src="' . SugarThemeRegistry::current()->getImageURL('id-ff-add.png') . '"></button>';
        $html .= '<table id="emailLine_table" width="100%"></table>';

        $html .= "<script>";

        if (isset($params['email_target_type'])) {
            foreach ($params['email_target_type'] as $key => $field) {
                if (is_array($params['email'][$key])) {
                    $params['email'][$key] = json_encode($params['email'][$key]);
                }
                $emailToType = $params['email_to_type'][$key] ?? '';
                $html .= "load_emailline('" . $emailToType . "','" . $params['email_target_type'][$key] . "','" . $params['email'][$key] . "');";
            }
        }
        $html .= "</script>";

        return $html;
    }

    if ($view === 'DetailView') {
        $recipients = [];
        $result = [];

        if (isset($params['email_target_type'])) {
            $typeValues = $params['email'];
            foreach ($params['email_target_type'] as $key => $type) {
                if (array_key_exists($type, $app_list_strings['aor_email_type_list'])) {
                    switch ($type) {
                        case 'Specify User':
                            $recipients['User'][] = BeanFactory::getBean('Users', $typeValues[$key])->name;
                            break;
                        case 'Email Address':
                            $recipients['Emails'][] = $typeValues[$key];
                            break;
                        case 'Users':
                            $recipients['Users'][] = $typeValues[$key][0];
                            break;
                    }
                }
            }
        }

        array_walk(
            $recipients,
            function ($recipients, $type) use (&$result) {
                $result[] = sprintf('%s: %s', $type, implode(', ', $recipients));
            }
        );

        return implode("<br><br>", $result);
    }

    return '';
}
