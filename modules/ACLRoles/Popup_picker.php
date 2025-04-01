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









#[\AllowDynamicProperties]
class Popup_Picker
{


    /*
     *
     */
    public function __construct()
    {
    }




    /*
     *
     */
    public function _get_where_clause()
    {
        $where = '';
        if (isset($_REQUEST['query'])) {
            $where_clauses = array();
            append_where_clause($where_clauses, "name", "acl_roles.name");
            $where = generate_where_statement($where_clauses);
        }

        return $where;
    }

    /**
     *
     */
    public function process_page()
    {
        global $mod_strings;
        global $app_strings;
        global $currentModule;
        global $sugar_version, $sugar_config;

        $output_html = '';
        $where = '';

        $where = $this->_get_where_clause();

        $name = empty($_REQUEST['name']) ? '' : $_REQUEST['name'];
        $request_data = empty($_REQUEST['request_data']) ? '' : $_REQUEST['request_data'];
        $hide_clear_button = empty($_REQUEST['hide_clear_button']) ? false : true;

        $button  = "<form action='index.php' method='post' name='form' id='form'>\n";
        if (!$hide_clear_button) {
            $button .= "<input type='button' name='button' class='button' onclick=\"send_back('','');\" title='"
                .$app_strings['LBL_CLEAR_BUTTON_TITLE']."' value='  "
                .$app_strings['LBL_CLEAR_BUTTON_LABEL']."  ' />\n";
        }
        $button .= "<input type='submit' name='button' class='button' onclick=\"window.close();\" title='"
            .$app_strings['LBL_CANCEL_BUTTON_TITLE']."'  value='  "
            .$app_strings['LBL_CANCEL_BUTTON_LABEL']."  ' />\n";
        $button .= "</form>\n";

        $form = new XTemplate('modules/ACLRoles/Popup_picker.html');
        $form->assign('MOD', $mod_strings);
        $form->assign('APP', $app_strings);
        $form->assign('MODULE_NAME', $currentModule);
        $form->assign('NAME', $name);
        $form->assign('request_data', $request_data);

        ob_start();
        insert_popup_header();
        $output_html .= ob_get_contents();
        ob_end_clean();

        $output_html .= get_form_header($mod_strings['LBL_SEARCH_FORM_TITLE'], '', false);

        $form->parse('main.SearchHeader');
        $output_html .= $form->text('main.SearchHeader');

        // Reset the sections that are already in the page so that they do not print again later.
        $form->reset('main.SearchHeader');

        // create the listview
        $seed_bean = BeanFactory::newBean('ACLRoles');
        $ListView = new ListView();
        $ListView->show_export_button = false;
        $ListView->process_for_popups = true;
        $ListView->setXTemplate($form);
        $ListView->setHeaderTitle($mod_strings['LBL_ROLE']);
        $ListView->setHeaderText($button);
        $ListView->setQuery($where, '', 'name', 'ROLE');
        $ListView->setModStrings($mod_strings);

        ob_start();
        $ListView->processListView($seed_bean, 'main', 'ROLE');
        $output_html .= ob_get_contents();
        ob_end_clean();

        $output_html .= insert_popup_footer();
        return $output_html;
    }
} // end of class Popup_Picker
