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

#[\AllowDynamicProperties]
class formLetter
{
    public static function LVSmarty()
    {
        global $app_strings, $sugar_config;
        if (preg_match('/^6\./', (string) $sugar_config['sugar_version'])) {
            $script = '<a href="#" class="menuItem" onmouseover="hiliteItem(this,\'yes\');
" onmouseout="unhiliteItem(this);" onclick="showPopup()">' . $app_strings['LBL_PRINT_AS_PDF'] . '</a>';
        } else {
            $script = ' <input class="button" type="button" value="' .
                $app_strings['LBL_PRINT_AS_PDF'] . '" ' . 'onClick="showPopup();">';
        }

        return $script;
    }

    public static function getModuleTemplates($module)
    {
        $db = DBManagerFactory::getInstance();
        $templates = array();

        $sql = "SELECT id,name FROM aos_pdf_templates WHERE type = '" . $module . "' AND deleted = 0  AND active = 1 ORDER BY name";
        $result = $db->query($sql);
        while ($row = $db->fetchByAssoc($result)) {
            $templates[$row['id']] = $row['name'];
        }

        return $templates;
    }

    public static function LVPopupHtml($module)
    {
        global $app_strings;

        $templates = formLetter::getModuleTemplates($module);

        if (!empty($templates)) {
            echo '    
            
            <div id="popupDiv_ara" class="modal fade" style="display: none;">
               <div class="modal-dialog">
                  <div class="modal-content">
                     <div class="modal-header">
                        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">x</button>
                        <h4 class="modal-title">' . $app_strings['LBL_SELECT_TEMPLATE'] . '</h4>
                     </div>
                     <div class="modal-body">
                        <div style="padding: 5px 5px; overflow: auto; height: auto;">
                              <table width="100%" class="list view table-responsive" cellspacing="0" cellpadding="0" border="0">
                                 <tbody>';
            $iOddEven = 1;
            foreach ($templates as $templateid => $template) {
                $iOddEvenCls = 'oddListRowS1';
                if ($iOddEven % 2 == 0) {
                    $iOddEvenCls = 'evenListRowS1';
                }
                echo '<tr height="20" class="' . $iOddEvenCls . '" >
                                            <td width="17" valign="center"><a href="#" onclick="$(\'#popupDiv_ara\').modal(\'hide\');sListView.send_form(true, \'' . $module .
                    '\', \'index.php?templateID=' . $templateid . '&entryPoint=formLetter\',\'' . $app_strings['LBL_LISTVIEW_NO_SELECTED'] . '\');"><img src="themes/default/images/txt_image_inline.gif" width="16" height="16" /></a></td>
                                            <td scope="row" align="left"><b><a href="#" onclick="$(\'#popupDiv_ara\').modal(\'hide\');sListView.send_form(true, \'' . $module .
                    '\', \'index.php?templateID=' . $templateid . '&entryPoint=formLetter\',\'' . $app_strings['LBL_LISTVIEW_NO_SELECTED'] . '\');">' . $template . '</a></b></td></tr>';
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
                function showPopup(){
                    if(sugarListView.get_checks_count() < 1)
                    {
                        alert(\'' . $app_strings['LBL_LISTVIEW_NO_SELECTED'] . '\');
                    }
                    else
                    {
                        var ppd2=document.getElementById(\'popupDiv_ara\');
                        if(ppd2!=null){
                            $("#popupDiv_ara").modal("show",{backdrop: "static"});
                        }else{
                            alert(\'Error!\');
                        }
                    }
                }
            </script>';
        } else {
            echo '<script>
                function showPopup(){
                alert(\'' . $app_strings['LBL_NO_TEMPLATE'] . '\');        
                }
            </script>';
        }
    }

    public static function DVPopupHtml($module)
    {
        global $app_strings;

        $templates = formLetter::getModuleTemplates($module);

        if (!empty($templates)) {
            echo '
            <div id="popupDiv_ara" class="modal fade" style="display: none;">
               <div class="modal-dialog">
                  <div class="modal-content">
                     <div class="modal-header">
                        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">x</button>
                        <h4 class="modal-title">' . $app_strings['LBL_SELECT_TEMPLATE'] . '</h4>
                     </div>
                     <div class="modal-body">
                        <div style="padding: 5px 5px; overflow: auto; height: auto;">
                           <form id="popupForm" action="index.php?entryPoint=formLetter" method="post">
                              <table width="100%" class="list view table-responsive" cellspacing="0" cellpadding="0" border="0">
                                 <tbody>';
            $iOddEven = 1;
            foreach ($templates as $templateid => $template) {
                $iOddEvenCls = 'oddListRowS1';
                if ($iOddEven % 2 == 0) {
                    $iOddEvenCls = 'evenListRowS1';
                }
                $js = "$('#popupDiv_ara').modal('hide');var form=document.getElementById('popupForm');if(form!=null){form.templateID.value='" . $templateid . "';form.submit();}else{alert('Error!');}";
                echo '<tr height="20" class="' . $iOddEvenCls . '">
                                        <td width="17" valign="center"><a href="#" onclick="' . $js . '"><img src="themes/default/images/txt_image_inline.gif" width="16" height="16" /></a></td>
                                        <td scope="row" align="left"><b><a href="#" onclick="' . $js . '">' . $template . '</a></b></td></tr>';
                $iOddEven++;
            }
            echo '</tbody></table>
                              <input type="hidden" name="templateID" value="" />
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
                function showPopup(){
                    var ppd2=document.getElementById(\'popupDiv_ara\');
                    if(ppd2!=null){
                        $("#popupDiv_ara").modal("show",{backdrop: "static"});
                    }else{
                        alert(\'Error!\');
                    }
                }
            </script>';
        } else {
            echo '<script>
                function showPopup(){
                alert(\'' . $app_strings['LBL_NO_TEMPLATE'] . '\');        
                }
            </script>';
        }
    }
}
