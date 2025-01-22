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


require_once('modules/Contacts/ContactFormBase.php');

#[\AllowDynamicProperties]
class Popup_Picker
{
    /*
     *
     */
    public function _get_where_clause()
    {
        $where = '';
        if (isset($_REQUEST['query'])) {
            $where_clauses = array();
            append_where_clause($where_clauses, "first_name", "contacts.first_name");
            append_where_clause($where_clauses, "last_name", "contacts.last_name");
            append_where_clause($where_clauses, "account_name", "accounts.name");
            append_where_clause($where_clauses, "account_id", "accounts.id");
            $where = generate_where_statement($where_clauses);
        }
        return $where;
    }

    /**
     *
     */
    public function process_page_for_address()
    {
        global $theme;
        global $mod_strings;
        global $app_strings;
        global $currentModule;
        global $sugar_version, $sugar_config;

        $output_html = '';
        $where = '';

        $where = $this->_get_where_clause();


        $formBase = new ContactFormBase();
        if (isset($_REQUEST['doAction']) && $_REQUEST['doAction'] == 'save') {
            $formBase->handleSave('', false, true);
        }

        $first_name = empty($_REQUEST['first_name']) ? '' : $_REQUEST['first_name'];
        $last_name = empty($_REQUEST['last_name']) ? '' : $_REQUEST['last_name'];
        $account_name = empty($_REQUEST['account_name']) ? '' : $_REQUEST['account_name'];
        $request_data = empty($_REQUEST['request_data']) ? '' : $_REQUEST['request_data'];
        $hide_clear_button = empty($_REQUEST['hide_clear_button']) ? false : true;
        $lbl_save_button_title = $app_strings['LBL_SAVE_BUTTON_TITLE'];
        $lbl_save_button_key = $app_strings['LBL_SAVE_BUTTON_KEY'];
        $lbl_save_button_label = $app_strings['LBL_SAVE_BUTTON_LABEL'];

        // TODO: cleanup the construction of $addform
        $formbody = $formBase->getFormBody('', '', 'EmailEditView');
        $addform = '<table><tr><td nowrap="nowrap" valign="top">'
            .str_replace('<br>', '</td><td nowrap="nowrap" valign="top">&nbsp;', (string) $formbody)
            . '</td></tr></table>'
            . '<input type="hidden" name="action" value="Popup" />';
        $formSave = <<<EOQ
		<input type="submit" name="button" class="button" title="$lbl_save_button_title" value="  $lbl_save_button_label  " />
		<input type="button" name="button" class="button" title="{$app_strings['LBL_CANCEL_BUTTON_TITLE']}" accesskey="{$app_strings['LBL_CANCEL_BUTTON_KEY']}" value="{$app_strings['LBL_CANCEL_BUTTON_LABEL']}" onclick="toggleDisplay('addform');" />
EOQ;
        $createContact = <<<EOQ
		<input type="button" id="showAdd" name="showAdd" class="button" value="{$mod_strings['LNK_NEW_CONTACT']}" onclick="toggleDisplay('addform');" />
EOQ;
        $addformheader = get_form_header($mod_strings['LNK_NEW_CONTACT'], $formSave, false);
        $button  = "<form action='index.php' method='post' name='form' id='form'>\n";
        if (!$hide_clear_button) {
            $button .= "<input type='button' name='button' class='button' onclick=\"send_back('','');\" title='"
                .$app_strings['LBL_CLEAR_BUTTON_TITLE']."' value='  "
                .$app_strings['LBL_CLEAR_BUTTON_LABEL']."  ' />\n";
        }
        $button .= "<input type='submit' name='button' class='button' onclick=\"window.close();\" title='"
            .$app_strings['LBL_CANCEL_BUTTON_TITLE']."' accesskey='"
            .$app_strings['LBL_CANCEL_BUTTON_KEY']."' value='  "
            .$app_strings['LBL_CANCEL_BUTTON_LABEL']."  ' />\n";
        $button .= "</form>\n";

        $form = new XTemplate('modules/Contacts/Address_picker.html');
        $form->assign('MOD', $mod_strings);
        $form->assign('APP', $app_strings);
        $form->assign('ADDFORMHEADER', $addformheader);
        $form->assign('ADDFORM', $addform);
        $form->assign('THEME', $theme);
        $form->assign('MODULE_NAME', $currentModule);
        $form->assign('FIRST_NAME', $first_name);
        $form->assign('LAST_NAME', $last_name);
        $form->assign('ACCOUNT_NAME', $account_name);
        $form->assign('request_data', $request_data);

        // fill in for mass update
        $button = "<input type='hidden' name='module' value='Contacts'>".
                  "<input type='hidden' id='form_action' name='action' value='CloseContactAddressPopup'>".
                  "<input type='hidden' name='massupdate' value='true'>".
                  "<input type='hidden' name='delete' value='false'>".
                  "<input type='hidden' name='mass' value='Array'>".
                  "<input type='hidden' name='Update' value='Update'>";

        if (isset($_REQUEST['mass']) && is_array($_REQUEST['mass'])) {
            foreach (array_unique($_REQUEST['mass']) as $record) {
                $button .= "<input style='display: none' checked type='checkbox' name='mass[]' value='$record'>\n";
            }
        }

        $button .= "<input type='hidden' name='query' value='true'>";
        $button .= "<input type='hidden' name='saved_associated_data' value=''>";
        $button .= "<input type='hidden' name='close_window' value='true'>";
        $button .= "<input type='hidden' name='html' value='change_address'>";
        $button .= "<input type='hidden' name='account_name' value='$account_name'>";
        // Added ID attribute to each element to use getElementById. To give ID attribute to an element is a good practice.
        $button .= "<span style='display: none'><textarea name='primary_address_street' id='primary_address_street'>" . str_replace("&lt;br&gt;", "\n", (string) $_REQUEST["primary_address_street"]) . "</textarea></span>";
        $button .= "<input type='hidden' name='primary_address_city' id='primary_address_city' value='". $_REQUEST["primary_address_city"] ."'>";
        $button .= "<input type='hidden' name='primary_address_state' id='primary_address_state' value='". $_REQUEST["primary_address_state"] ."'>";
        $button .= "<input type='hidden' name='primary_address_postalcode' id='primary_address_postalcode' value='". $_REQUEST["primary_address_postalcode"] ."'>";
        $button .= "<input type='hidden' name='primary_address_country' id='primary_address_country' value='". $_REQUEST["primary_address_country"] ."'>";
        // Adding an onclick event to remove address for alternate address, as user has selected copy address to primary address
        $button .= "<input title='".$mod_strings['LBL_COPY_ADDRESS_CHECKED_PRIMARY']."'  class='button' LANGUAGE=javascript type='submit' name='button' value='  ".$mod_strings['LBL_COPY_ADDRESS_CHECKED_PRIMARY']."  ' onclick='clearAddress(\"alt\");'>\n";
        // Adding a new block of code copy the address to alternate address for contacts
        $button .= "<span style='display: none'><textarea name='alt_address_street' id='alt_address_street'>" . str_replace("&lt;br&gt;", "\n", (string) $_REQUEST["primary_address_street"]) . "</textarea></span>";
        $button .= "<input type='hidden' name='alt_address_city' id='alt_address_city' value='". $_REQUEST["primary_address_city"] ."'>";
        $button .= "<input type='hidden' name='alt_address_state' id='alt_address_state' value='". $_REQUEST["primary_address_state"] ."'>";
        $button .= "<input type='hidden' name='alt_address_postalcode' id='alt_address_postalcode' value='". $_REQUEST["primary_address_postalcode"] ."'>";
        $button .= "<input type='hidden' name='alt_address_country' id='alt_address_country' value='". $_REQUEST["primary_address_country"] ."'>";
        // Adding an onclick event to remove address for primary address, as user has selected copy address to alternate address
        // NOTE => You need to change the label as as per SugarCRM way..
        $button .= "<input title='".$mod_strings['LBL_COPY_ADDRESS_CHECKED_ALT']."'  class='button' LANGUAGE=javascript type='submit' name='button' value='  ".$mod_strings['LBL_COPY_ADDRESS_CHECKED_ALT']."  ' onclick='clearAddress(\"primary\");'>\n";
        $button .= "<input title='".$app_strings['LBL_CANCEL_BUTTON_TITLE']."' accessKey='".$app_strings['LBL_CANCEL_BUTTON_KEY']."' class='button' LANGUAGE=javascript onclick=\"window.close()\" type='submit' name='button' value='  ".$app_strings['LBL_CANCEL_BUTTON_LABEL']."  '>\n";
        ob_start();
        insert_popup_header($theme);
        $output_html .= ob_get_contents();
        ob_end_clean();

        // Reset the sections that are already in the page so that they do not print again later.
        $form->reset('main.SearchHeader');

        // create the listview
        $seed_bean = BeanFactory::newBean('Contacts');
        $ListView = new ListView();
        $ListView->show_export_button = false;
        $ListView->process_for_popups = true;
        $ListView->show_delete_button = false;
        $ListView->show_select_menu = false;
        $ListView->setXTemplate($form);
        $ListView->setHeaderTitle($mod_strings['LBL_LIST_FORM_TITLE']);
        $ListView->setHeaderText($button);
        $ListView->setQuery($where, '', '', 'CONTACT');
        $ListView->setModStrings($mod_strings);

        ob_start();
        $ListView->processListViewMulti($seed_bean, 'main', 'CONTACT');
        $output_html .= ob_get_contents();
        ob_end_clean();

        // Regular Expression to override sListView
        $exp = '/sListView.save_checks/si';
        $change = 'save_checks';
        $output_html = preg_replace(array($exp), array($change), $output_html);

        $output_html .= <<<EOJS
        <script type="text/javascript">
        <!--
        // Function to clear address according to the buttons clicked.
        function clearAddress(key)
        {
            document.getElementById(key+"_address_street").value = "";
            document.getElementById(key+"_address_city").value = "";
            document.getElementById(key+"_address_state").value = "";
            document.getElementById(key+"_address_postalcode").value = "";
            document.getElementById(key+"_address_country").value = "";
        }
        checked_items = Array();
        inputs_array = document.MassUpdate.elements;

        for(wp = 0 ; wp < inputs_array.length; wp++) {
            if(inputs_array[wp].name == "mass[]" && inputs_array[wp].style.display == "none") {
                checked_items.push(inputs_array[wp].value);
            }
        }
        for(i in checked_items) {
            for(wp = 0 ; wp < inputs_array.length; wp++) {
                if(inputs_array[wp].name == "mass[]" && inputs_array[wp].value == checked_items[i]) {
                    inputs_array[wp].checked = true;
                }
            }
        }
        -->
        </script>
EOJS;

        $output_html .= insert_popup_footer();
        return $output_html;
    }

    public function process_page_for_merge()
    {
        global $theme;
        global $mod_strings;
        global $app_strings;
        global $currentModule;
        global $sugar_version, $sugar_config;

        $output_html = '';
        $where = '';

        $where = $this->_get_where_clause();

        $first_name = empty($_REQUEST['first_name']) ? '' : $_REQUEST['first_name'];
        $last_name = empty($_REQUEST['last_name']) ? '' : $_REQUEST['last_name'];
        $account_name = empty($_REQUEST['account_name']) ? '' : $_REQUEST['account_name'];
        $hide_clear_button = empty($_REQUEST['hide_clear_button']) ? false : true;
        $button  = "<form action='index.php' method='post' name='form' id='form'>\n";
        //START:FOR MULTI-SELECT
        $multi_select=false;
        if (!empty($_REQUEST['mode']) && strtoupper($_REQUEST['mode']) == 'MULTISELECT') {
            $multi_select=true;
            $button .= "<input type='button' name='button' class='button' onclick=\"send_back_selected('Contacts',document.MassUpdate,'mass[]','" .$app_strings['ERR_NOTHING_SELECTED']."');\" title='"
                .$app_strings['LBL_SELECT_BUTTON_TITLE']."' accesskey='"
                .$app_strings['LBL_SELECT_BUTTON_KEY']."' value='  "
                .$app_strings['LBL_SELECT_BUTTON_LABEL']."  ' />\n";
        }
        //END:FOR MULTI-SELECT
        if (!$hide_clear_button) {
            $button .= "<input type='button' name='button' class='button' onclick=\"send_back('','');\" title='"
                .$app_strings['LBL_CLEAR_BUTTON_TITLE']."' value='  "
                .$app_strings['LBL_CLEAR_BUTTON_LABEL']."  ' />\n";
        }
        $button .= "<input type='submit' name='button' class='button' onclick=\"window.close();\" title='"
            .$app_strings['LBL_CANCEL_BUTTON_TITLE']."' accesskey='"
            .$app_strings['LBL_CANCEL_BUTTON_KEY']."' value='  "
            .$app_strings['LBL_CANCEL_BUTTON_LABEL']."  ' />\n";
        $button .= "</form>\n";

        $form = new XTemplate('modules/Contacts/MailMergePicker.html');
        $form->assign('MOD', $mod_strings);
        $form->assign('APP', $app_strings);
        $form->assign('THEME', $theme);
        $form->assign('MODULE_NAME', $currentModule);
        $form->assign('FIRST_NAME', $first_name);
        $form->assign('LAST_NAME', $last_name);
        $form->assign('ACCOUNT_NAME', $account_name);
        $request_data = empty($_REQUEST['request_data']) ? '' : $_REQUEST['request_data'];
        $form->assign('request_data', $request_data);

        ob_start();
        insert_popup_header($theme);
        $output_html .= ob_get_contents();
        ob_end_clean();

        $output_html .= get_form_header($mod_strings['LBL_SEARCH_FORM_TITLE'], '', false);

        $form->parse('main.SearchHeader');
        $output_html .= $form->text('main.SearchHeader');

        // Reset the sections that are already in the page so that they do not print again later.
        $form->reset('main.SearchHeader');

        // create the listview
        $seed_bean = BeanFactory::newBean('Contacts');
        $ListView = new ListView();
        $ListView->display_header_and_footer=false;
        $ListView->show_export_button = false;
        $ListView->process_for_popups = true;
        $ListView->setXTemplate($form);
        $ListView->multi_select_popup=$multi_select;
        if ($multi_select) {
            $ListView->xTemplate->assign("TAG_TYPE", "SPAN");
        } else {
            $ListView->xTemplate->assign("TAG_TYPE", "A");
        }
        $ListView->setHeaderTitle($mod_strings['LBL_LIST_FORM_TITLE']);
        $ListView->setQuery($where, '', 'contacts.last_name, contacts.first_name', 'CONTACT');
        $ListView->setModStrings($mod_strings);

        ob_start();
        $output_html .= get_form_header($mod_strings['LBL_LIST_FORM_TITLE'], $button, false);
        //BEGIN ATHENA CUSTOMIZATION - rsmith
        require_once('modules/MailMerge/merge_query.php');
        $rel_module = empty($_REQUEST['rel_module'])?'': $_REQUEST['rel_module'];
        $id = empty($_REQUEST['id'])?'': $_REQUEST['id'];

        $query = get_merge_query($seed_bean, $rel_module, $id);
        $result = $seed_bean->db->query($query, true, "Error retrieving $seed_bean->object_name list: ");

        $list = array();
        while (($row = $seed_bean->db->fetchByAssoc($result)) != null) {
            $seed_bean = BeanFactory::newBean('Contacts');
            foreach ($seed_bean->field_defs as $field=>$value) {
                if (isset($row[$field])) {
                    $seed_bean->$field = $row[$field];
                } else {
                    if (isset($row[$seed_bean->table_name .'.'.$field])) {
                        $seed_bean->$field = $row[$seed_bean->table_name .'.'.$field];
                    } else {
                        $seed_bean->$field = "";
                    }
                }
            }
            $seed_bean->fill_in_additional_list_fields();

            $list[] = $seed_bean;
        }

        $ListView->processListViewTwo($list, 'main', 'CONTACT');

        //END ATHENA CUSTOMIZATION - rsmith
        $output_html .= ob_get_contents();
        ob_end_clean();

        $output_html .= insert_popup_footer();
        return $output_html;
    }
}
