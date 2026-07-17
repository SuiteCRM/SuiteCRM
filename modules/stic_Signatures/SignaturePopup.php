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

/**
 * This class provides functionalities related to selecting signature templates.
 * It includes methods to generate HTML for displaying signature selection popups
 * in both List View (LV) and Detail View (DV) contexts, and to retrieve available
 * signature templates for a given module.
 */
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
class SelectSignatureTemplate
{
    /**
     * Generates the Smarty script for displaying the "Add to Signature Process" button/link
     * based on the SugarCRM version.
     *
     * @return string The HTML string for the button or link.
     */
    public static function LVSmarty()
    {
        global $app_strings, $sugar_config;
        if (preg_match('/^6\./', (string) $sugar_config['sugar_version'])) {
            $script = '<a href="#" class="menuItem" onmouseover="hiliteItem(this,\'yes\');
" onmouseout="unhiliteItem(this);" onclick="showPopupSignatures()">' . $app_strings['LBL_ADD_TO_SIGNATURE_PROCESS'] . '</a>';
        } else {
            $script = ' <input class="button" type="button" value="' .
                $app_strings['LBL_ADD_TO_SIGNATURE_PROCESS'] . '" ' . 'onClick="showPopupSignatures();">';
        }

        return $script;
    }

    /**
     * Retrieves a list of open signature templates for a given module.
     *
     * @param string $module The name of the module.
     * @return array An associative array where keys are signature IDs and values are signature names.
     */
    public static function getModuleTemplates($module)
    {
        global $current_user;
        $db = DBManagerFactory::getInstance();
        $signatures = array();

        $sql = "SELECT id, name, status FROM stic_signatures WHERE main_module = '" . $module . "' AND deleted = 0";

        require_once 'modules/SecurityGroups/SecurityGroup.php';
        require_once 'modules/ACL/ACLController.php';
        if (ACLController::requireSecurityGroup('stic_Signatures', 'list')) {
            $sql .= " AND (stic_signatures.assigned_user_id = '" . $current_user->id . "' OR " . SecurityGroup::getGroupWhere('stic_signatures', 'stic_Signatures', $current_user->id) . ")";
        }

        $sql .= " ORDER BY date_modified DESC";
        $result = $db->query($sql);
        while ($row = $db->fetchByAssoc($result)) {
            $signatures[$row['id']] = ['name' => $row['name'], 'status' => $row['status']];
        }

        return $signatures;
    }

    /**
     * Generates the HTML for the signature selection popup in a List View context.
     *
     * @param string $module The name of the module from which the popup is initiated.
     * @return void Echoes the HTML directly.
     */
    public static function LVPopupHtml($module)
    {
        global $app_strings, $app_list_strings ;

        $mod_strings = return_module_language($GLOBALS['current_language'], 'stic_Signatures');

        $signatures = self::getModuleTemplates($module);

        if (!empty($signatures)) {
            echo '    
            
            <div id="popup-div-signature" class="modal fade" style="display: none;">
               <div class="modal-dialog">
                  <div class="modal-content">
                     <div class="modal-header">
                        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">×</button>
                        <h4 class="modal-title">' . $app_strings['LBL_SELECT_SIGNATURE'] . '</h4>

                     </div>
                     <div class="modal-body">
                        <div style="padding: 5px 5px; overflow: auto; height: auto;">
                              <table width="100%" class="list view table-responsive" cellspacing="0" cellpadding="0" border="0">
                                 <tbody>';
            $iOddEven = 1;
            foreach ($signatures as $signatureId => $signature) {
                $iOddEvenCls = 'oddListRowS1';
                if ($iOddEven % 2 == 0) {
                    $iOddEvenCls = 'evenListRowS1';
                }
                echo '<tr height="20" class="' . $iOddEvenCls . '" >
                                            <td width="17" valign="center"><a href="#" onclick="$(\'#popup-div-signature\').modal(\'hide\');sListView.send_form(true, \'' . $module .
                    '\', \'index.php?signature-id=' . $signatureId . '&entryPoint=sticSignatureSignersSelect\',\'' . $app_strings['LBL_LISTVIEW_NO_SELECTED'] . '\');"><img src="themes/SuiteP/images/insert-signature.png" width="16" height="16" /></a></td>
                                            <td scope="row" align="left"><b><a href="#" onclick="$(\'#popup-div-signature\').modal(\'hide\');sListView.send_form(true, \'' . $module .
                    '\', \'index.php?signature-id=' . $signatureId . '&entryPoint=sticSignatureSignersSelect\',\'' . $app_strings['LBL_LISTVIEW_NO_SELECTED'] . '\');">' . $signature['name'] . '</a></b></td>
                    <td>'.$app_list_strings['stic_signatures_status_list'][$signature['status']].'</td></tr>';
                $iOddEven++;
            }
            echo '</tbody></table>
                        </div>
                     </div>
                     <div class="modal-footer">&nbsp;<button type="button" class="btn btn-primary" data-dismiss="modal">' . $app_strings['LBL_CANCEL_BUTTON_LABEL'] . '</button></div>
                  </div>
               </div>
            </div>
            <script>
                function showPopupSignatures(){
                    if(sugarListView.get_checks_count() < 1)
                    {
                        alert(\'' . $app_strings['LBL_LISTVIEW_NO_SELECTED'] . '\');
                    }
                    else
                    {
                        var ppd2=document.getElementById(\'popup-div-signature\');
                        if(ppd2!=null){
                            $("#popup-div-signature").modal("show",{backdrop: "static"});
                        }else{
                            alert(\'Error!\');
                        }
                    }
                }
                function showRelatedSignatures(){
                    alert(\'Not implemented yet\');
                }    
            </script>';
        } else {
            $moduleName = isset($app_list_strings['moduleList'][$module]) ? $app_list_strings['moduleList'][$module] : $module;
            echo '<script>
                function showPopupSignatures(){
                alert(\'' . $mod_strings['LBL_NO_SIGNATURE_EXISTS_FOR_MODULE'] . ' ' . $moduleName . '\');
                }
            </script>';
        }
    }





    /**
     * Generates the HTML for the signature selection popup in a Detail View context.
     *
     * @param string $module The name of the module from which the popup is initiated.
     * @return void Echoes the HTML directly.
     */
    public static function DVPopupHtml($module)
    {
        global $app_strings, $app_list_strings;
        $mod_strings = return_module_language($GLOBALS['current_language'], 'stic_Signatures');

        $signatures = self::getModuleTemplates($module);

        if (!empty($signatures)) {
            echo '
            <div id="popup-div-signature" class="modal fade" style="display: none;">
               <div class="modal-dialog">
                  <div class="modal-content">
                     <div class="modal-header">
                        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">×</button>
                        <h4 class="modal-title">' . $app_strings['LBL_SELECT_SIGNATURE'] . '</h4>
                        
                     </div>
                     <div class="modal-body">
                        <div style="padding: 5px 5px; overflow: auto; height: auto;">
                           <form id="signatureForm" action="index.php?entryPoint=sticSignatureSignersSelect" method="post">
                              <table width="100%" class="list view table-responsive" cellspacing="0" cellpadding="0" border="0">
                                 <tbody>';
            $iOddEven = 1;
            foreach ($signatures as $signatureId => $signature) {
                $iOddEvenCls = 'oddListRowS1';
                if ($iOddEven % 2 == 0) {
                    $iOddEvenCls = 'evenListRowS1';
                }
                $js = "$('#popup-div-signature').modal('hide');var form=document.getElementById('signatureForm');if(form!=null){console.log(form);form['signature-id'].value='" . $signatureId . "';form.submit();}else{alert('Error!');}";
                echo '<tr height="20" class="' . $iOddEvenCls . '">
                                        <td width="17" valign="center"><a href="#" onclick="' . $js . '"><img src="themes/SuiteP/images/insert-signature.png" width="16" height="16" /></a></td>
                                        <td scope="row" align="left"><b><a href="#" onclick="' . $js . '">' . $signature['name'] . '</a></b></td>
                                        <td>' . $app_list_strings['stic_signatures_status_list'][$signature['status']] . '</td></tr>';
                $iOddEven++;
            }
            echo '</tbody></table>
                              <input type="hidden" name="signature-id" value="" />
                            <input type="hidden" name="module" value="' . $module . '" />
                            <input type="hidden" name="uid" value="' . clean_string($_REQUEST['record'],
                    'STANDARDSPACE') . '" />
                           </form>
                        </div>
                     </div>
                     <div class="modal-footer">&nbsp;<button type="button" class="btn btn-primary" data-dismiss="modal">' . $app_strings['LBL_CANCEL_BUTTON_LABEL'] . '</button></div>
                  </div>
               </div>
            </div>
            <script>
                function showPopupSignature(){
                    var ppd2=document.getElementById(\'popup-div-signature\');
                    if(ppd2!=null){
                        $("#popup-div-signature").modal("show",{backdrop: "static"});
                    }else{
                        alert(\'Error!\');
                    }
                }
            </script>';
        } else {
            $moduleName = isset($app_list_strings['moduleList'][$module]) ? $app_list_strings['moduleList'][$module] : $module;
            echo '<script>
                function showPopupSignature(){
                alert(\'' . $mod_strings['LBL_NO_SIGNATURE_EXISTS_FOR_MODULE'] . ' ' . $moduleName . '\');
                }
            </script>';
        }
    }
    public static function DVPopupRelatedSignaturesHtml($module, $recordId)
    {
        global $app_strings, $app_list_strings, $db, $timedate, $current_user; 
        $mod_strings = return_module_language($current_user->preferred_language ?? '', 'stic_Signers');

        $signaturesQuery = 
            "SELECT * FROM stic_signers s
            WHERE s.record_type = '{$module}' AND s.record_id = '{$recordId}'
            AND s.deleted = 0 ORDER BY s.name";
        $result = $db->query($signaturesQuery);
        $signatures = array();
        while ($row = $db->fetchByAssoc($result)) {
            $signatures[$row['id']]['name'] = $row['name'];
            $signatures[$row['id']]['status'] = $row['status'];
            $signatures[$row['id']]['signature_date'] = $timedate->to_display_date_time($row['signature_date'], true, true, $current_user);
            $signatures[$row['id']]['id'] = $row['id'];
        }
        
            echo '
            <div id="popup-div-related-signature" class="modal fade" style="display: none;">
               <div class="modal-dialog">
                  <div class="modal-content">
                     <div class="modal-header">
                        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">×</button>
                        <h4 class="modal-title"><img src="themes/SuiteP/images/insert-signature.png" width="32" height="32" />&nbsp;' . $app_strings['LBL_SHOW_RELATED_SIGNATURES'] . '</h4>
                        <span style="margin-right: 0.5rem;">&nbsp;&nbsp;' . $app_strings['LBL_SHOW_RELATED_SIGNATURES_INFO'] . ' <strong>' . $app_list_strings['moduleList'][$module] . '</strong></span>
                     </div>
                     <div class="modal-body">
                        <div style="padding: 5px 5px; overflow: auto; height: auto;">
                           <table width="100%" class="list view table-responsive" cellspacing="0" cellpadding="0" border="0">
                              <tbody>';                              
            if(empty($signatures)) {
                echo '<tr><td><span style="color: red;">' . $app_strings['LBL_NO_SIGNATURES_FOUND'] . '</span></td></tr>';
            }else
            {
            $iOddEven = 1;
            echo '<tr style="font-weight: bold; text-decoration: underline;"><td>' . $mod_strings['LBL_NAME'] . '</td>
                            <td>' . $mod_strings['LBL_STATUS'] . '</td>
                            <td>' . $mod_strings['LBL_SIGNATURE_DATE'] . '</td>
                            </tr>';
            foreach ($signatures as $signatureId => $signature) {
                $iOddEvenCls = 'oddListRowS1';
                if ($iOddEven % 2 == 0) {
                    $iOddEvenCls = 'evenListRowS1';
                }
                
                echo '<tr height="20" class="' . $iOddEvenCls . '">
                                        <td scope="row" align="left"><b><a href="index.php?module=stic_Signers&action=DetailView&record=' . $signature['id'] . '">' . $signature['name'] . '</a></b></td>
                                        <td scope="row" align="left">' . $app_list_strings['stic_signers_status_list'][$signature['status']] . '</td>
                                        <td scope="row" align="left">' . $signature['signature_date'] . '</td>
                                        </tr>';
                $iOddEven++;
            }
        }
            echo '</tbody></table>
                              <input type="hidden" name="signature-id" value="" />
                            <input type="hidden" name="module" value="' . $module . '" />
                            <input type="hidden" name="uid" value="' . clean_string($_REQUEST['record'],
                    'STANDARDSPACE') . '" />
                           
                        </div>
                     </div>
                     <div class="modal-footer">&nbsp;<button type="button" class="btn btn-primary" data-dismiss="modal">' . $app_strings['LBL_CANCEL_BUTTON_LABEL'] . '</button></div>
                  </div>
               </div>
            </div>
            <script>
                function showRelatedSignatures(){
                    var ppd2=document.getElementById(\'popup-div-related-signature\');
                    if(ppd2!=null){
                        $("#popup-div-related-signature").modal("show",{backdrop: "static"});
                    }else{
                        alert(\'Error!\');
                    }
                }
            </script>';
        
    }
}