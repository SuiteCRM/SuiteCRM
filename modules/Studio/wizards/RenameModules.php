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


require_once('modules/Studio/DropDowns/DropDownHelper.php');
require_once 'modules/ModuleBuilder/parsers/parser.label.php' ;

#[\AllowDynamicProperties]
class RenameModules
{
    /**
     * Selected language user is renaming for (eg. en_us).
     *
     * @var string
     */
    private $selectedLanguage;

    /**
     * An array containing the modules which should be renamed.
     *
     * @var array
     */
    private $changedModules;

    /**
     * An array containing the modules which have had their module strings modified as part of the
     * renaming process.
     *
     * @var array
     */
    private $renamedModules = array();


    /**
     * An array containing the modules and their labels to be changed when module is renamed.
     */
    private static $labelMap = array(
        'Accounts' => array(
            array('name' => 'LBL_CAMPAIGNS', 'type' => 'plural', 'source' => 'Campaigns'),
            array('name' => 'LBL_CAMPAIGN_ID', 'type' => 'singular', 'source' => 'Campaigns'),
            array('name' => 'LBL_PARENT_ACCOUNT_ID', 'type' => 'singular', 'source' => 'Accounts'),
            array('name' => 'LBL_PROSPECT_LIST', 'type' => 'singular', 'source' => 'Prospects'),
            array('name' => 'LBL_MODULE_NAME', 'type' => 'plural', 'source' => 'Accounts'),
        ),
        'Bugs' => array(
            array('name' => 'LBL_LIST_FORM_TITLE', 'type' => 'singular', 'source' => 'Bugs'),
            array('name' => 'LBL_LIST_MY_BUGS', 'type' => 'plural', 'source' => 'Bugs'),
            array('name' => 'LBL_SEARCH_FORM_TITLE', 'type' => 'singular', 'source' => 'Bugs'),
            array('name' => 'LNK_BUG_LIST', 'type' => 'plural', 'source' => 'Bugs'),
            array('name' => 'LNK_BUG_REPORTS', 'type' => 'singular', 'source' => 'Bugs'),
            array('name' => 'LNK_IMPORT_BUGS', 'type' => 'plural', 'source' => 'Bugs'),
            array('name' => 'LNK_NEW_BUG', 'type' => 'singular', 'source' => 'Bugs'),
            array('name' => 'LBL_MODULE_NAME', 'type' => 'plural', 'source' => 'Bugs'),
        ),
        'Calls' => array(
            array('name' => 'LBL_LIST_CONTACT', 'type' => 'singular', 'source' => 'Contacts'),
            array('name' => 'LBL_MODULE_NAME', 'type' => 'plural', 'source' => 'Calls'),
        ),
        'Campaigns' => array(
            array('name' => 'LBL_ACCOUNTS', 'type' => 'plural', 'source' => 'Accounts'),
            array('name' => 'LBL_CONTACTS', 'type' => 'plural', 'source' => 'Contacts'),
            array('name' => 'LBL_LIST_CAMPAIGN_NAME', 'type' => 'singular', 'source' => 'Campaigns'),
            array('name' => 'LBL_LOG_ENTRIES_CONTACT_TITLE', 'type' => 'plural', 'source' => 'Contacts'),
            array('name' => 'LBL_LOG_ENTRIES_LEAD_TITLE', 'type' => 'plural', 'source' => 'Leads'),
            array('name' => 'LBL_OPPORTUNITIES', 'type' => 'plural', 'source' => 'Opportunities'),
            array('name' => 'LBL_PROSPECT_LIST_SUBPANEL_TITLE', 'type' => 'singular', 'source' => 'Targets'),
            array('name' => 'LBL_MODULE_NAME', 'type' => 'plural', 'source' => 'Campaigns'),
        ),
        'Cases' => array(
            array('name' => 'LBL_BUGS_SUBPANEL_TITLE', 'type' => 'plural', 'source' => 'Bugs'),
            array('name' => 'LBL_LIST_ACCOUNT_NAME', 'type' => 'singular', 'source' => 'Accounts'),
            array('name' => 'LBL_MODULE_NAME', 'type' => 'plural', 'source' => 'Cases'),
        ),
        'Contacts' => array(
            array('name' => 'LBL_BUGS_SUBPANEL_TITLE', 'type' => 'plural', 'source' => 'Bugs'),
            array('name' => 'LBL_CAMPAIGN_LIST_SUBPANEL_TITLE', 'type' => 'plural', 'source' => 'Campaigns'),
            array('name' => 'LBL_CONTRACTS', 'type' => 'plural', 'source' => 'Contracts'),
            array('name' => 'LBL_LIST_ACCOUNT_NAME', 'type' => 'singular', 'source' => 'Accounts'),
            array('name' => 'LBL_LEAD_SOURCE', 'type' => 'singular', 'source' => 'Leads'),
            array('name' => 'LBL_OPPORTUNITIES', 'type' => 'singular', 'source' => 'Opportunities'),
            array('name' => 'LBL_OPPORTUNITY_ROLE', 'type' => 'singular', 'source' => 'Opportunities'),
            array('name' => 'LBL_OPPORTUNITY_ROLE_ID', 'type' => 'singular', 'source' => 'Opportunities'),
            array('name' => 'LBL_PRODUCTS_TITLE', 'type' => 'plural', 'source' => 'Products'),
            array('name' => 'LBL_PROSPECT_LIST', 'type' => 'singular', 'source' => 'Prospects'),
            array('name' => 'LBL_MODULE_NAME', 'type' => 'plural', 'source' => 'Contacts'),
        ),
        'Contracts' => array(
            array('name' => 'LBL_CONTRACT_NAME', 'type' => 'singular', 'source' => 'Contracts'),
            array('name' => 'LBL_CONTRACT_TERM', 'type' => 'singular', 'source' => 'Contracts'),
            array('name' => 'LBL_DOCUMENTS', 'type' => 'plural', 'source' => 'Documents'),
            array('name' => 'LBL_LIST_ACCOUNT_NAME', 'type' => 'singular', 'source' => 'Accounts'),
            array('name' => 'LBL_LIST_CONTRACT_NAME', 'type' => 'singular', 'source' => 'Contracts'),
            array('name' => 'LBL_OPPORTUNITY', 'type' => 'singular', 'source' => 'Opportunities'),
            array('name' => 'LBL_OPPORTUNITY_NAME', 'type' => 'singular', 'source' => 'Opportunities'),
            array('name' => 'LBL_SEARCH_FORM_TITLE', 'type' => 'singular', 'source' => 'Contracts'),
            array('name' => 'LBL_TOTAL_CONTRACT_VALUE', 'type' => 'singular', 'source' => 'Contracts'),
            array('name' => 'LBL_TOTAL_CONTRACT_VALUE_USDOLLAR', 'type' => 'singular', 'source' => 'Contracts'),
            array('name' => 'LNK_NEW_CONTRACT', 'type' => 'singular', 'source' => 'Contracts'),
            array('name' => 'LBL_MODULE_NAME', 'type' => 'plural', 'source' => 'Contracts'),
        ),
        'Documents' => array(
            array('name' => 'LBL_BUGS_SUBPANEL_TITLE', 'type' => 'plural', 'source' => 'Bugs'),
            array('name' => 'LBL_CONTRACTS', 'type' => 'plural', 'source' => 'Contracts'),
            array('name' => 'LBL_CONTRACT_NAME', 'type' => 'singular', 'source' => 'Contracts'),
            array('name' => 'LBL_CONTRACT_STATUS', 'type' => 'singular', 'source' => 'Contracts'),
            array('name' => 'LBL_DET_RELATED_DOCUMENT_VERSION', 'type' => 'singular', 'source' => 'Documents'),
            array('name' => 'LBL_DET_TEMPLATE_TYPE', 'type' => 'singular', 'source' => 'Documents'),
            array('name' => 'LBL_DOC_ID', 'type' => 'singular', 'source' => 'Documents'),
            array('name' => 'LBL_DOC_NAME', 'type' => 'singular', 'source' => 'Documents'),
            array('name' => 'LBL_DOC_REV_HEADER', 'type' => 'singular', 'source' => 'Documents'),
            array('name' => 'LBL_DOC_URL', 'type' => 'singular', 'source' => 'Documents'),
            array('name' => 'LBL_NAME', 'type' => 'singular', 'source' => 'Documents'),
            array('name' => 'LBL_TEMPLATE_TYPE', 'type' => 'singular', 'source' => 'Documents'),
            array('name' => 'LBL_MODULE_NAME', 'type' => 'plural', 'source' => 'Documents'),
        ),
        'KBDocuments' => array(
            array('name' => 'LBL_CASES', 'type' => 'plural', 'source' => 'Cases'),
            array('name' => 'LBL_CONTRACTS', 'type' => 'plural', 'source' => 'Contracts'),
            array('name' => 'LBL_CONTRACT_NAME', 'type' => 'plural', 'source' => 'Contracts'),
            array('name' => 'LBL_MODULE_NAME', 'type' => 'plural', 'source' => 'KBDocuments'),
        ),
        'Leads' => array(
            array('name' => 'LNK_SELECT_###MODULE_PLURAL###', 'type' => 'singular', 'source' => 'Leads'),
            array('name' => 'LNK_SELECT_###MODULE_SINGULAR###', 'type' => 'singular', 'source' => 'Leads'),
            array('name' => 'LBL_ACCOUNT_DESCRIPTION', 'type' => 'singular', 'source' => 'Accounts'),
            array('name' => 'LBL_ACCOUNT_ID', 'type' => 'singular', 'source' => 'Accounts'),
            array('name' => 'LBL_ACCOUNT_NAME', 'type' => 'singular', 'source' => 'Accounts'),
            array('name' => 'LBL_CAMPAIGN_ID', 'type' => 'singular', 'source' => 'Campaigns'),
            array('name' => 'LBL_CAMPAIGN_LEAD', 'type' => 'plural', 'source' => 'Campaigns'),
            array('name' => 'LBL_CAMPAIGN_LIST_SUBPANEL_TITLE', 'type' => 'plural', 'source' => 'Campaigns'),
            array('name' => 'LBL_CONTACT_ID', 'type' => 'singular', 'source' => 'Contacts'),
            array('name' => 'LBL_LEAD_SOURCE', 'type' => 'singular', 'source' => 'Leads'),
            array('name' => 'LBL_LEAD_SOURCE_DESCRIPTION', 'type' => 'singular', 'source' => 'Leads'),
            array('name' => 'LBL_LIST_ACCOUNT_NAME', 'type' => 'singular', 'source' => 'Accounts'),
            array('name' => 'LBL_OPPORTUNITY_AMOUNT', 'type' => 'singular', 'source' => 'Opportunities'),
            array('name' => 'LBL_OPPORTUNITY_ID', 'type' => 'singular', 'source' => 'Opportunities'),
            array('name' => 'LBL_OPPORTUNITY_NAME', 'type' => 'singular', 'source' => 'Opportunities'),
            array('name' => 'LBL_CONVERTED_ACCOUNT', 'type' => 'singular', 'source' => 'Accounts'),
            array('name' => 'LNK_SELECT_ACCOUNTS', 'type' => 'singular', 'source' => 'Accounts'),
            array('name' => 'LNK_NEW_ACCOUNT', 'type' => 'singular', 'source' => 'Accounts'),
            array('name' => 'LBL_CONVERTED_CONTACT', 'type' => 'singular', 'source' => 'Contacts'),
            array('name' => 'LBL_CONVERTED_OPP', 'type' => 'singular', 'source' => 'Opportunities'),
            array('name' => 'LBL_MODULE_NAME', 'type' => 'plural', 'source' => 'Leads'),
        ),
        'Meetings' => array(
            array('name' => 'LBL_LIST_CONTACT', 'type' => 'singular', 'source' => 'Contacts'),
            array('name' => 'LBL_LIST_JOIN_MEETING', 'type' => 'singular', 'source' => 'Meetings'),
            array('name' => 'LBL_PASSWORD', 'type' => 'singular', 'source' => 'Meetings'),
            array('name' => 'LBL_TYPE', 'type' => 'singular', 'source' => 'Meetings'),
            array('name' => 'LBL_URL', 'type' => 'singular', 'source' => 'Meetings'),
            array('name' => 'LBL_MODULE_NAME', 'type' => 'plural', 'source' => 'Meetings'),
        ),
        'Notes' => array(
            array('name' => 'LBL_ACCOUNT_ID', 'type' => 'singular', 'source' => 'Accounts'),
            array('name' => 'LBL_CASE_ID', 'type' => 'singular', 'source' => 'Cases'),
            array('name' => 'LBL_CONTACT_ID', 'type' => 'singular', 'source' => 'Contacts'),
            array('name' => 'LBL_LIST_CONTACT', 'type' => 'singular', 'source' => 'Contacts'),
            array('name' => 'LBL_LIST_CONTACT_NAME', 'type' => 'singular', 'source' => 'Contacts'),
            array('name' => 'LBL_NOTE_STATUS', 'type' => 'singular', 'source' => 'Notes'),
            array('name' => 'LBL_OPPORTUNITY_ID', 'type' => 'singular', 'source' => 'Opportunities'),
            array('name' => 'LBL_PRODUCT_ID', 'type' => 'singular', 'source' => 'Products'),
            array('name' => 'LBL_QUOTE_ID', 'type' => 'singular', 'source' => 'Quotes'),
            array('name' => 'LBL_MODULE_NAME', 'type' => 'plural', 'source' => 'Notes'),
        ),
        'Opportunities' => array(
            array('name' => 'LBL_ACCOUNT_ID', 'type' => 'singular', 'source' => 'Accounts'),
            array('name' => 'LBL_AMOUNT', 'type' => 'singular', 'source' => 'Opportunities'),
            array('name' => 'LBL_CAMPAIGN_OPPORTUNITY', 'type' => 'plural', 'source' => 'Campaigns'),
            array('name' => 'LBL_CONTRACTS', 'type' => 'plural', 'source' => 'Contracts'),
            array('name' => 'LBL_LEAD_SOURCE', 'type' => 'singular', 'source' => 'Leads'),
            array('name' => 'LBL_LIST_ACCOUNT_NAME', 'type' => 'singular', 'source' => 'Accounts'),
            array('name' => 'LBL_OPPORTUNITY_NAME', 'type' => 'singular', 'source' => 'Opportunities'),
            array('name' => 'LBL_MODULE_NAME', 'type' => 'plural', 'source' => 'Opportunities'),
        ),
        'ProductTemplates' => array(
            array('name' => 'LBL_PRODUCT_ID', 'type' => 'singular', 'source' => 'Products'),
            array('name' => 'LBL_MODULE_NAME', 'type' => 'plural', 'source' => 'ProductTemplates'),
        ),
        'Products' => array(
            array('name' => 'LBL_ACCOUNT_ID', 'type' => 'singular', 'source' => 'Accounts'),
            array('name' => 'LBL_CONTACT', 'type' => 'singular', 'source' => 'Contacts'),
            array('name' => 'LBL_CONTACT_ID', 'type' => 'singular', 'source' => 'Contacts'),
            array('name' => 'LBL_CONTRACTS', 'type' => 'plural', 'source' => 'Contacts'),
            array('name' => 'LBL_LIST_ACCOUNT_NAME', 'type' => 'singular', 'source' => 'Accounts'),
            array('name' => 'LBL_LIST_NAME', 'type' => 'singular', 'source' => 'Products'),
            array('name' => 'LBL_NAME', 'type' => 'singular', 'source' => 'Products'),
            array('name' => 'LBL_QUOTE_ID', 'type' => 'singular', 'source' => 'Quotes'),
            array('name' => 'LBL_RELATED_PRODUCTS', 'type' => 'plural', 'source' => 'Products'),
            array('name' => 'LBL_URL', 'type' => 'singular', 'source' => 'Products'),
            array('name' => 'LBL_MODULE_NAME', 'type' => 'plural', 'source' => 'Products'),
        ),
        'ProjectTask' => array(
            array('name' => 'LBL_PARENT_NAME', 'type' => 'singular', 'source' => 'Projects'),
            array('name' => 'LBL_PROJECT_ID', 'type' => 'singular', 'source' => 'Projects'),
            array('name' => 'LBL_PROJECT_NAME', 'type' => 'singular', 'source' => 'Projects'),
            array('name' => 'LBL_PROJECT_TASK_ID', 'type' => 'singular', 'source' => 'Projects'),
            array('name' => 'LBL_MODULE_NAME', 'type' => 'plural', 'source' => 'ProjectTask'),
        ),
        'Project' => array(
            array('name' => 'LBL_BUGS_SUBPANEL_TITLE', 'type' => 'plural', 'source' => 'Bugs'),
            array('name' => 'LBL_CONTACTS_RESOURCE', 'type' => 'singular', 'source' => 'Contacts'),
            array('name' => 'LBL_LIST_FORM_TITLE', 'type' => 'singular', 'source' => 'Projects'),
            array('name' => 'LBL_OPPORTUNITIES', 'type' => 'plural', 'source' => 'Opportunities'),
            array('name' => 'LBL_PROJECT_HOLIDAYS_TITLE', 'type' => 'singular', 'source' => 'Projects'),
            array('name' => 'LBL_PROJECT_TASKS_SUBPANEL_TITLE', 'type' => 'singular', 'source' => 'Projects'),
            array('name' => 'LBL_SEARCH_FORM_TITLE', 'type' => 'singular', 'source' => 'Projects'),
            array('name' => 'LNK_NEW_PROJECT', 'type' => 'singular', 'source' => 'Projects'),
            array('name' => 'LNK_PROJECT_LIST', 'type' => 'singular', 'source' => 'Projects'),
            array('name' => 'LBL_MODULE_NAME', 'type' => 'plural', 'source' => 'Projects'),
        ),
        'Quotes' => array(
            array('name' => 'LBL_ACCOUNT_ID', 'type' => 'singular', 'source' => 'Accounts'),
            array('name' => 'LBL_CONTRACTS', 'type' => 'plural', 'source' => 'Contracts'),
            array('name' => 'LBL_LIST_ACCOUNT_NAME', 'type' => 'singular', 'source' => 'Accounts'),
            array('name' => 'LBL_QUOTE_NUM', 'type' => 'singular', 'source' => 'Quotes'),
            array('name' => 'LBL_MODULE_NAME', 'type' => 'plural', 'source' => 'Quotes'),
        ),
        'Targets' => array(
            array('name' => 'LBL_ACCOUNT_NAME', 'type' => 'singular', 'source' => 'Accounts'),
            array('name' => 'LBL_CAMPAIGN_ID', 'type' => 'plural', 'source' => 'Campaigns'),
            array('name' => 'LBL_CAMPAIGN_LIST_SUBPANEL_TITLE', 'type' => 'singular', 'source' => 'Campaigns'),
            array('name' => 'LBL_PROSPECT_LIST', 'type' => 'singular', 'source' => 'Prospects'),
            array('name' => 'LBL_MODULE_NAME', 'type' => 'plural', 'source' => 'Targets'),
        ),
        'Tasks' => array(
            array('name' => 'LBL_CONTACT', 'type' => 'singular', 'source' => 'Contacts'),
            array('name' => 'LBL_CONTACT_ID', 'type' => 'singular', 'source' => 'Contacts'),
            array('name' => 'LBL_CONTACT_PHONE', 'type' => 'singular', 'source' => 'Contacts'),
            array('name' => 'LBL_MODULE_NAME', 'type' => 'plural', 'source' => 'Tasks'),
        ),
    );


    /**
     *
     * @param string $options
     * @return void
     */
    public function process($options = '')
    {
        if ($options == 'SaveDropDown') {
            $this->save();
        }

        $this->display();
    }

    /**
     * Main display function.
     *
     * @return void
     */
    protected function display()
    {
        global $app_list_strings, $mod_strings;


        require_once('modules/Studio/parsers/StudioParser.php');
        $dh = new DropDownHelper();

        $smarty = new Sugar_Smarty();
        $smarty->assign('MOD', $GLOBALS['mod_strings']);
        $title=getClassicModuleTitle($mod_strings['LBL_MODULE_NAME'], array("<a href='index.php?module=Administration&action=index'>".$mod_strings['LBL_MODULE_NAME']."</a>", $mod_strings['LBL_RENAME_TABS']), false);
        $smarty->assign('title', $title);

        $selected_lang = (!empty($_REQUEST['dropdown_lang'])?$_REQUEST['dropdown_lang']:$_SESSION['authenticated_user_language']);
        if (empty($selected_lang)) {
            $selected_lang = $GLOBALS['sugar_config']['default_language'];
        }

        if ($selected_lang == $GLOBALS['current_language']) {
            $my_list_strings = $GLOBALS['app_list_strings'];
        } else {
            $my_list_strings = return_app_list_strings_language($selected_lang);
        }

        $selected_dropdown = $my_list_strings['moduleList'];
        $selected_dropdown_singular = $my_list_strings['moduleListSingular'];


        foreach ($selected_dropdown as $key=>$value) {
            $singularValue = isset($selected_dropdown_singular[$key]) ? $selected_dropdown_singular[$key] : $value;
            if ($selected_lang != $_SESSION['authenticated_user_language'] && !empty($app_list_strings['moduleList']) && isset($app_list_strings['moduleList'][$key])) {
                $selected_dropdown[$key]=array('lang'=>$value, 'user_lang'=> '['.$app_list_strings['moduleList'][$key] . ']', 'singular' => $singularValue);
            } else {
                $selected_dropdown[$key]=array('lang'=>$value, 'singular' => $singularValue);
            }
        }


        $selected_dropdown = $dh->filterDropDown('moduleList', $selected_dropdown);

        $smarty->assign('dropdown', $selected_dropdown);
        $smarty->assign('dropdown_languages', get_languages());

        $buttons = array();
        $buttons[] = array('text'=>$mod_strings['LBL_BTN_UNDO'],'actionScript'=>"onclick='jstransaction.undo()'" );
        $buttons[] = array('text'=>$mod_strings['LBL_BTN_REDO'],'actionScript'=>"onclick='jstransaction.redo()'" );
        $buttons[] = array('text'=>$mod_strings['LBL_BTN_SAVE'],'actionScript'=>"onclick='if(check_form(\"editdropdown\")){document.editdropdown.submit();}'");
        $buttonTxt = StudioParser::buildImageButtons($buttons);
        $smarty->assign('buttons', $buttonTxt);
        $smarty->assign('dropdown_lang', $selected_lang);

        $editImage = SugarThemeRegistry::current()->getImage('edit_inline', '');
        $smarty->assign('editImage', $editImage);
        $deleteImage = SugarThemeRegistry::current()->getImage('delete_inline', '');
        $smarty->assign('deleteImage', $deleteImage);
        $smarty->display("modules/Studio/wizards/RenameModules.tpl");
    }

    /**
     * Save function responsible executing all sub-save functions required to rename a module.
     *
     * @return void
     */
    public function save($redirect = true)
    {
        $this->selectedLanguage = (!empty($_REQUEST['dropdown_lang'])? $_REQUEST['dropdown_lang']:$_SESSION['authenticated_user_language']);

        //Clear all relevant language caches
        $this->clearLanguageCaches();

        //Retrieve changes the user is requesting and store previous values for future use.
        $this->changedModules = $this->getChangedModules();

        //Change module, appStrings, subpanels, and related links.
        $this->changeAppStringEntries()->changeAllModuleModStrings()->renameAllRelatedLinks()->renameAllSubpanels()->renameAllDashlets();

        foreach (self::$labelMap as $module=>$labelsArr) {
            $this->renameCertainModuleModStrings($module, $labelsArr);
        }

        //Refresh the page again so module tabs are changed as the save process happens after module tabs are already generated.
        if ($redirect) {
            SugarApplication::redirect('index.php?action=wizard&module=Studio&wizard=StudioWizard&option=RenameTabs');
        }
    }

    /**
     * Rename all subpanels within the application.
     *
     *
     * @return RenameModules
     */
    private function renameAllSubpanels()
    {
        global $beanList;

        foreach ($beanList as $moduleName => $beanName) {
            if (class_exists($beanName)) {
                $this->renameModuleSubpanel($moduleName, $beanName, $this->changedModules);
            } else {
                $GLOBALS['log']->error("Class $beanName does not exist, unable to rename.");
            }
        }

        return $this;
    }

    /**
     * Rename subpanels for a particular module.
     *
     * @param  string $moduleName The name of the module to be renamed
     * @param  string $beanName  The name of the SugarBean to be renamed.
     * @return void
     */
    private function renameModuleSubpanel($moduleName, $beanName)
    {
        $GLOBALS['log']->info("About to rename subpanel for module: $moduleName");
        $bean = new $beanName();
        //Get the subpanel def
        $subpanelDefs = $this->getSubpanelDefs($bean);

        if (empty($subpanelDefs)) {
            $GLOBALS['log']->debug("Found empty subpanel defs for $moduleName");
            return;
        }

        $mod_strings = return_module_language($this->selectedLanguage, $moduleName);
        $replacementStrings = array();

        //Iterate over all subpanel entries and see if we need to make a change.
        foreach ($subpanelDefs as $subpanelName => $subpanelMetaData) {
            $GLOBALS['log']->debug("Examining subpanel definition for potential rename: $subpanelName ");
            //For each subpanel def, check if they are in our changed modules set.
            foreach ($this->changedModules as $changedModuleName => $renameFields) {
                if (!(isset($subpanelMetaData['type']) &&  $subpanelMetaData['type'] == 'collection') //Dont bother with collections
                    && isset($subpanelMetaData['module']) && $subpanelMetaData['module'] == $changedModuleName && isset($subpanelMetaData['title_key'])) {
                    $replaceKey = $subpanelMetaData['title_key'];
                    if (!isset($mod_strings[$replaceKey])) {
                        $GLOBALS['log']->info("No module string entry defined for: {$mod_strings[$replaceKey]}");
                        continue;
                    }
                    $oldStringValue = $mod_strings[$replaceKey];
                    //At this point we don't know if we should replace the string with the plural or singular version of the new
                    //strings so we'll try both but with the plural version first since it should be longer than the singular.
                    // The saved old strings are html decoded, so we need to decode the new string first before str_replace.
                    $replacedString = str_replace(html_entity_decode_utf8($renameFields['prev_plural'], ENT_QUOTES), $renameFields['plural'], (string) $oldStringValue);
                    if ($replacedString == $oldStringValue) {
                        // continue to replace singular only if nothing been replaced yet
                        $replacedString = str_replace(html_entity_decode_utf8($renameFields['prev_singular'], ENT_QUOTES), $renameFields['singular'], $replacedString);
                    }
                    $replacementStrings[$replaceKey] = $replacedString;
                }
            }
        }

        //Now we can write out the replaced language strings for each module
        if (count($replacementStrings) > 0) {
            $GLOBALS['log']->debug("Writing out labels for subpanel changes for module $moduleName, labels: " . var_export($replacementStrings, true));
            ParserLabel::addLabels($this->selectedLanguage, $replacementStrings, $moduleName);
            $this->renamedModules[$moduleName] = true;
        }
    }

    /**
     * Retrieve the subpanel definitions for a given SugarBean object. Unforunately we can't reuse
     * any of the SubPanelDefinion.php functions.
     *
     * @param  SugarBean $bean
     * @return array The subpanel definitions.
     */
    private function getSubpanelDefs($bean)
    {
        if (empty($bean->module_dir)) {
            return array();
        }

        $layout_defs = array();

        if (file_exists('modules/' . $bean->module_dir . '/metadata/subpaneldefs.php')) {
            require('modules/' . $bean->module_dir . '/metadata/subpaneldefs.php');
        }

        if (file_exists('custom/modules/' . $bean->module_dir . '/Ext/Layoutdefs/layoutdefs.ext.php')) {
            require('custom/modules/' . $bean->module_dir . '/Ext/Layoutdefs/layoutdefs.ext.php');
        }

        return isset($layout_defs[$bean->module_dir]['subpanel_setup']) ? $layout_defs[$bean->module_dir]['subpanel_setup'] : $layout_defs;
    }

    /**
     * Rename all related linked within the application
     *
     * @return RenameModules
     */
    private function renameAllRelatedLinks()
    {
        global $beanList;

        foreach ($beanList as $moduleName => $beanName) {
            if (class_exists($beanName)) {
                $this->renameModuleRelatedLinks($moduleName, $beanName);
            } else {
                $GLOBALS['log']->fatal("Class $beanName does not exist, unable to rename.");
            }
        }

        return $this;
    }

    /**
     * Rename the related links within a module.
     *
     * @param  string $moduleName The module to be renamed
     * @param  string $moduleClass The class name of the module to be renamed
     * @return void
     */
    private function renameModuleRelatedLinks($moduleName, $moduleClass)
    {
        global $app_strings;
        $GLOBALS['log']->info("Begining to renameModuleRelatedLinks for $moduleClass\n");
        $bean = BeanFactory::getBean($moduleName);
        if (!$bean instanceof SugarBean) {
            $GLOBALS['log']->info("Unable to get linked fields for module $moduleClass\n");
            return;
        }

        $arrayToRename = array();
        $replacementStrings = array();
        $mod_strings = return_module_language($this->selectedLanguage, $moduleName);
        $changedModules = array_keys($this->changedModules);

        $relatedFields = $bean->get_related_fields();
        foreach ($relatedFields as $field => $defs) {
            if (isset($defs['module']) && in_array($defs['module'], $changedModules)) {
                $arrayToRename[$field] = $defs;
            }
        }
        $linkedFields = $bean->get_linked_fields();
        foreach ($linkedFields as $field => $defs) {
            $fieldName = $defs['name'];
            if ($bean->load_relationship($fieldName)) {
                $relModule = $bean->$fieldName->getRelatedModuleName();
                if (in_array($relModule, $changedModules)) {
                    $defs['module'] = $relModule;
                    $arrayToRename[$field] = $defs;
                }
            }
        }

        foreach ($arrayToRename as $link => $linkEntry) {
            $GLOBALS['log']->debug("Begining to rename for link field {$link}");
            if (!isset($linkEntry['vname'])
                || (!isset($mod_strings[$linkEntry['vname']]) && !isset($app_strings[$linkEntry['vname']]))) {
                $GLOBALS['log']->debug("No label attribute for link $link, continuing.");
                continue;
            }

            $replaceKey = $linkEntry['vname'];
            $oldStringValue = translate($replaceKey, $moduleName);
            $renameFields = $this->changedModules[$linkEntry['module']];

            if (strlen((string) $renameFields['prev_plural']) > strlen((string) $renameFields['prev_singular']) && strpos((string) $oldStringValue, (string) $renameFields['prev_plural']) !== false) {
                $key = 'plural';
            } else {
                $key = 'singular';
            }
            $replacedString = str_replace(html_entity_decode_utf8($renameFields['prev_' . $key], ENT_QUOTES), $renameFields[$key], (string) $oldStringValue);
            $replacementStrings[$replaceKey] = $replacedString;
        }

        //Now we can write out the replaced language strings for each module
        if (count($replacementStrings) > 0) {
            $GLOBALS['log']->debug("Writing out labels for link changes for module $moduleName, labels: " . var_export($replacementStrings, true));
            ParserLabel::addLabels($this->selectedLanguage, $replacementStrings, $moduleName);
            $this->renamedModules[$moduleName] = true;
        }
    }

    /**
     * Clear all related language cache files.
     *
     * @return void
     */
    private function clearLanguageCaches()
    {
        //remove the js language files
        LanguageManager::removeJSLanguageFiles();

        //remove lanugage cache files
        LanguageManager::clearLanguageCache();
    }

    /**
     * Rename all module strings within the application for dashlets.
     *
     * @return RenameModules
     */
    private function renameAllDashlets()
    {
        //Load the Dashlet metadata so we know what needs to be changed
        if (!is_file(sugar_cached('dashlets/dashlets.php'))) {
            require_once('include/Dashlets/DashletCacheBuilder.php');
            $dc = new DashletCacheBuilder();
            $dc->buildCache();
        }

        include(sugar_cached('dashlets/dashlets.php'));

        foreach ($this->changedModules as $moduleName => $replacementLabels) {
            $this->changeModuleDashletStrings($moduleName, $replacementLabels, $dashletsFiles);
        }

        return $this;
    }

    /*
     * Rename the title value for all dashlets associated with a particular module
     *
     */
    private function changeModuleDashletStrings($moduleName, $replacementLabels, $dashletsFiles)
    {
        $GLOBALS['log']->debug("Beginning to change module dashlet labels for: $moduleName ");
        $replacementStrings = array();

        foreach ($dashletsFiles as $dashletName => $dashletData) {
            if (isset($dashletData['module']) && $dashletData['module'] == $moduleName && file_exists($dashletData['meta'])) {
                require($dashletData['meta']);
                $dashletTitle = $dashletMeta[$dashletName]['title'];
                $currentModuleStrings = return_module_language($this->selectedLanguage, $moduleName);
                $modStringKey = array_search($dashletTitle, $currentModuleStrings, true);
                if ($modStringKey !== false) {
                    $replacedString = str_replace(html_entity_decode_utf8($replacementLabels['prev_plural'], ENT_QUOTES), $replacementLabels['plural'], (string) $dashletTitle);
                    if ($replacedString == $dashletTitle) {
                        $replacedString = str_replace(html_entity_decode_utf8($replacementLabels['prev_singular'], ENT_QUOTES), $replacementLabels['singular'], $replacedString);
                    }
                    $replacementStrings[$modStringKey] = $replacedString;
                }
            }
        }

        //Now we can write out the replaced language strings for each module
        if (count($replacementStrings) > 0) {
            $GLOBALS['log']->debug("Writing out labels for dashlet changes for module $moduleName, labels: " . var_export($replacementStrings, true));
            ParserLabel::addLabels($this->selectedLanguage, $replacementStrings, $moduleName);
        }
    }


    /**
     * Rename all module strings within the application.
     *
     * @return RenameModules
     */
    private function changeAllModuleModStrings()
    {
        foreach ($this->changedModules as $moduleName => $replacementLabels) {
            $this->changeModuleModStrings($moduleName, $replacementLabels);
        }

        return $this;
    }

    /**
      * Rename all module strings within the leads module.
      *
      * @param  string $targetModule The name of the module that owns the labels to be changed.
      * @param  array $labelKeysToReplace The labels to be changed.
      * @return RenameModules
      */
    private function renameCertainModuleModStrings($targetModule, $labelKeysToReplace)
    {
        $GLOBALS['log']->debug("Beginning to rename labels for $targetModule module");
        foreach ($this->changedModules as $moduleName => $replacementLabels) {
            $this->changeCertainModuleModStrings($moduleName, $replacementLabels, $targetModule, $labelKeysToReplace);
        }

        return $this;
    }

    /**
     * For a particular module, rename any relevant module strings that need to be replaced.
     *
     * @param  string $moduleName The name of the module to be renamed.
     * @param  $replacementLabels
     * @param  string $targetModule The name of the module that owns the labels to be changed.
     * @param  array $labelKeysToReplace The labels to be changed.
     * @return void
     */
    private function changeCertainModuleModStrings($moduleName, $replacementLabels, $targetModule, $labelKeysToReplace)
    {
        $GLOBALS['log']->debug("Beginning to change module labels for : $moduleName");
        $currentModuleStrings = return_module_language($this->selectedLanguage, $targetModule);

        $replacedLabels = array();
        foreach ($labelKeysToReplace as $entry) {
            if (!isset($entry['source']) || $entry['source'] != $moduleName) {
                // skip this entry if the source module does not match the module being renamed
                continue;
            }

            $formattedLanguageKey = $this->formatModuleLanguageKey($entry['name'], $replacementLabels);

            //If the static of dynamic key exists it should be replaced.
            if (isset($currentModuleStrings[$formattedLanguageKey])) {
                $oldStringValue = $currentModuleStrings[$formattedLanguageKey];
                $newStringValue = $this->replaceSingleLabel($oldStringValue, $replacementLabels, $entry);
                if ($oldStringValue != $newStringValue) {
                    $replacedLabels[$formattedLanguageKey] = $newStringValue;
                }
            }
        }

        //Save all entries
        ParserLabel::addLabels($this->selectedLanguage, $replacedLabels, $targetModule);
        $this->renamedModules[$targetModule] = true;
    }


    /**
     * For a particular module, rename any relevant module strings that need to be replaced.
     *
     * @param  string $moduleName The name of the module to be renamed.
     * @param  $replacementLabels
     * @return void
     */
    private function changeModuleModStrings($moduleName, $replacementLabels)
    {
        $GLOBALS['log']->info("Begining to change module labels for: $moduleName");
        $currentModuleStrings = return_module_language($this->selectedLanguage, $moduleName);
        $labelKeysToReplace = array(
            array('name' => 'LNK_NEW_RECORD', 'type' => 'plural'), //Module built modules, Create <moduleName>
            array('name' => 'LNK_LIST', 'type' => 'plural'), //Module built modules, View <moduleName>
            array('name' => 'LNK_NEW_###MODULE_SINGULAR###', 'type' => 'singular'),
            array('name' => 'LNK_###MODULE_SINGULAR###_LIST', 'type' => 'plural'),
            array('name' => 'LNK_###MODULE_SINGULAR###_REPORTS', 'type' => 'singular'),
            array('name' => 'LNK_IMPORT_VCARD', 'type' => 'singular'),
            array('name' => 'LNK_IMPORT_###MODULE_PLURAL###', 'type' => 'plural'),
            array('name' => 'MSG_SHOW_DUPLICATES', 'type' => 'singular', 'case' => 'both'),
            array('name' => 'LBL_SAVE_###MODULE_SINGULAR###', 'type' => 'singular'),
            array('name' => 'LBL_LIST_FORM_TITLE', 'type' => 'singular'), //Popup title
            array('name' => 'LBL_SEARCH_FORM_TITLE', 'type' => 'singular'), //Popup title
        );

        $replacedLabels = array();
        foreach ($labelKeysToReplace as $entry) {
            $formattedLanguageKey = $this->formatModuleLanguageKey($entry['name'], $replacementLabels);

            //If the static of dynamic key exists it should be replaced.
            if (isset($currentModuleStrings[$formattedLanguageKey])) {
                $oldStringValue = $currentModuleStrings[$formattedLanguageKey];
                $replacedLabels[$formattedLanguageKey] = $this->replaceSingleLabel($oldStringValue, $replacementLabels, $entry);
                if (isset($entry['case']) && $entry['case'] === 'both') {
                    $replacedLabels[$formattedLanguageKey] = $this->replaceSingleLabel($replacedLabels[$formattedLanguageKey], $replacementLabels, $entry, 'strtolower');
                }
            }
        }

        //Save all entries
        ParserLabel::addLabels($this->selectedLanguage, $replacedLabels, $moduleName);
        $this->renamedModules[$moduleName] = true;
    }

    /**
     * Format our dynamic keys containing module strings to a valid key depending on the module.
     *
     * @param  string $unformatedKey
     * @param  string $replacementStrings
     * @return string
     */
    private function formatModuleLanguageKey($unformatedKey, $replacementStrings)
    {
        $unformatedKey = str_replace('###MODULE_SINGULAR###', strtoupper($replacementStrings['key_singular']), $unformatedKey);
        return str_replace('###MODULE_PLURAL###', strtoupper($replacementStrings['key_plural']), $unformatedKey);
    }

    /**
     * Replace a label with a new value based on metadata which specifies the label as either singular or plural.
     *
     * @param  string $oldStringValue
     * @param  string $replacementLabels
     * @param  array $replacementMetaData
     * @return string
     */
    private function replaceSingleLabel($oldStringValue, $replacementLabels, $replacementMetaData, $modifier = '')
    {
        $replaceKey = 'prev_' . $replacementMetaData['type'];
        $search = html_entity_decode_utf8($replacementLabels[$replaceKey], ENT_QUOTES);
        $replace = $replacementLabels[$replacementMetaData['type']];
        if (!empty($modifier)) {
            $search = call_user_func($modifier, $search);
            $replace = call_user_func($modifier, $replace);
        }

        // Bug 47957
        // If nothing was replaced - try to replace original string
        $result = '';
        $replaceCount = 0;
        $result = str_replace($search, $replace, $oldStringValue, $replaceCount);
        if (!$replaceCount) {
            $replaceKey = 'key_' . $replacementMetaData['type'];
            $search = html_entity_decode_utf8($replacementLabels[$replaceKey], ENT_QUOTES);
            $result = str_replace($search, $replace, $oldStringValue, $replaceCount);
        }
        return $result;
    }


    /**
     * Save changes to the module names to the app string entries for both the moduleList and moduleListSingular entries.
     *
     * @return RenameModules
     */
    private function changeAppStringEntries()
    {
        $GLOBALS['log']->debug('Begining to save app string entries');
        //Save changes to the moduleList app string entry
        (new DropDownHelper())->saveDropDown($_REQUEST);

        //Save changes to the moduleListSingular app string entry
        $newParams = array();
        $newParams['dropdown_name'] = 'moduleListSingular';
        $newParams['dropdown_lang'] = isset($_REQUEST['dropdown_lang']) ? $_REQUEST['dropdown_lang'] : '';
        $newParams['use_push'] = true;
        (new DropDownHelper())->saveDropDown($this->createModuleListSingularPackage($newParams, $this->changedModules));

        //Save changes to the "*type_display*" app_list_strings entry.
        global $app_list_strings;

        $typeDisplayList = getTypeDisplayList();

        foreach (array_keys($this->changedModules)as $moduleName) {
            foreach ($typeDisplayList as $typeDisplay) {
                if (isset($app_list_strings[$typeDisplay]) && isset($app_list_strings[$typeDisplay][$moduleName])) {
                    $newParams['dropdown_name'] = $typeDisplay;
                    (new DropDownHelper())->saveDropDown($this->createModuleListSingularPackage($newParams, array($moduleName => $this->changedModules[$moduleName])));
                }
            }
        }
        return $this;
    }

    /**
     * Create an array entry that can be passed to the DropDownHelper:saveDropDown function so we can re-utilize
     * the save logic.
     *
     * @param  array $params
     * @param  array $changedModules
     * @return
     */
    private function createModuleListSingularPackage($params, $changedModules)
    {
        $count = 0;
        foreach ($changedModules as $moduleName => $package) {
            $singularString = $package['singular'];

            $params['slot_' . $count] = $count;
            $params['key_' . $count] = $moduleName;
            $params['value_' . $count] = $singularString;
            $params['delete_' . $count] = '';

            $count++;
        }

        return $params;
    }

    /**
     * Determine which modules have been updated and return an array with the module name as the key
     * and the singular/plural entries as the value.
     *
     * @return array
     */
    private function getChangedModules()
    {
        $count = 0;
        $allModuleEntries = array();
        $results = array();
        $params = $_REQUEST;

        $selected_lang = (!empty($params['dropdown_lang'])?$params['dropdown_lang']:$_SESSION['authenticated_user_language']);
        $current_app_list_string = return_app_list_strings_language($selected_lang);

        while (isset($params['slot_' . $count])) {
            $index = $params['slot_' . $count];

            $key = (isset($params['key_' . $index]))?SugarCleaner::stripTags($params['key_' . $index]): 'BLANK';
            $value = (isset($params['value_' . $index]))?SugarCleaner::stripTags($params['value_' . $index]): '';
            $svalue = (isset($params['svalue_' . $index]))?SugarCleaner::stripTags($params['svalue_' . $index]): $value;
            if ($key == 'BLANK') {
                $key = '';
            }

            $key = trim($key);
            $value = trim($value);
            $svalue = trim($svalue);

            //If the module key dne then do not continue with this rename.
            if (isset($current_app_list_string['moduleList'][$key])) {
                $allModuleEntries[$key] = array('s' => $svalue, 'p' => $value);
            } else {
                $_REQUEST['delete_' . $count] = true;
            }


            $count++;
        }


        foreach ($allModuleEntries as $k => $e) {
            $svalue = $e['s'];
            $pvalue = $e['p'];
            $prev_plural = $current_app_list_string['moduleList'][$k];
            $prev_singular = isset($current_app_list_string['moduleListSingular'][$k]) ? $current_app_list_string['moduleListSingular'][$k] : $prev_plural;
            if (strcmp($prev_plural, $pvalue) != 0 || (strcmp($prev_singular, $svalue) != 0)) {
                $results[$k] = array('singular' => $svalue, 'plural' => $pvalue, 'prev_singular' => $prev_singular, 'prev_plural' => $prev_plural,
                                     'key_plural' => $k, 'key_singular' => $this->getModuleSingularKey($k)
                );
            }
        }

        return $results;
    }


    /**
     * Return the 'singular' name of a module (Eg. Opportunity for Opportunities) given a moduleName which is a key
     * in the app string moduleList array.  If no entry is found, simply return the moduleName as this is consistant with modules
     * built by moduleBuilder.
     *
     * @param  string $moduleName
     * @return string The 'singular' name of a module.
     */
    private function getModuleSingularKey($moduleName)
    {
        $className = isset($GLOBALS['beanList'][$moduleName]) ? $GLOBALS['beanList'][$moduleName] : null;
        if (is_null($className) || ! class_exists($className)) {
            $GLOBALS['log']->error("Unable to get module singular key for class: $className");
            return $moduleName;
        }

        $tmp = new $className();
        if (property_exists($tmp, 'object_name')) {
            return $tmp->object_name;
        } else {
            return $moduleName;
        }
    }

    /**
     * Return an array of the modules whos mod_strings have been modified.
     *
     * @return array
     */
    public function getRenamedModules()
    {
        return $this->renamedModules;
    }
}
