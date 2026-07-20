<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

ob_start();
require('modules/EmailTemplates/DetailView.php');
$html = ob_get_clean();

global $mod_strings;
global $app_list_strings;

$focus = BeanFactory::getBean('EmailTemplates', $_REQUEST['record'] ?? '');
if ($focus && $focus->id) {
    $stic_status_val = $focus->stic_whatsapp_status_c ?? '';
    $stic_category_val = $focus->stic_whatsapp_category_c ?? '';
    $stic_twilio_id = htmlspecialchars($focus->stic_whatsapp_twilio_id_c ?? '', ENT_QUOTES);

    $stic_status_label = htmlspecialchars(
        $app_list_strings['stic_whatsapp_status_list'][$stic_status_val] ?? $stic_status_val,
        ENT_QUOTES
    );
    $stic_category_label = htmlspecialchars(
        $app_list_strings['stic_whatsapp_category_list'][$stic_category_val] ?? $stic_category_val,
        ENT_QUOTES
    );

    $whatsappRows = '
    <tr>
        <td scope="row" width="15%"><span>' . $mod_strings['LBL_STIC_WHATSAPP_STATUS'] . ':</span></td>
        <td><span>' . $stic_status_label . '&nbsp;</span></td>
    </tr>
    <tr>
        <td scope="row" width="15%"><span>' . $mod_strings['LBL_STIC_WHATSAPP_CATEGORY'] . ':</span></td>
        <td><span>' . $stic_category_label . '&nbsp;</span></td>
        <td scope="row" width="15%"><span>' . $mod_strings['LBL_STIC_WHATSAPP_TWILIO_ID'] . ':</span></td>
        <td><span>' . $stic_twilio_id . '&nbsp;</span></td>
    </tr>';

    $pos = strrpos($html, '</table>');
    if ($pos !== false) {
        $html = substr($html, 0, $pos) . $whatsappRows . '</table>' . substr($html, $pos + 8);
    }
}

echo $html;
echo getVersionedScript("SticInclude/js/Utils.js");
echo getVersionedScript("custom/modules/EmailTemplates/SticUtils.js");
