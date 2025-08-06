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

use SuiteCRM\Utility\SuiteValidator;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once("include/utils.php");
require_once("include/ytree/Tree.php");
require_once("include/ytree/ExtNode.php");
require_once("include/SugarFolders/SugarFolders.php");
require_once 'include/Exceptions/SuiteException.php';


#[\AllowDynamicProperties]
class EmailUI
{
    public $db;
    public $folder; // place holder for SugarFolder object
    public $folderStates = array(); // array of folderPath names and their states (1/0)
    public $smarty;
    public $addressSeparators = array(";", ",");
    public $rolloverStyle = "<style>div#rollover {position: relative;float: left;margin: none;text-decoration: none;}div#rollover a:hover {padding: 0;}div#rollover a span {display: none;}div#rollover a:hover span {text-decoration: none;display: block;width: 250px;margin-top: 5px;margin-left: 5px;position: absolute;padding: 10px;color: #333;	border: 1px solid #ccc;	background-color: #fff;	font-size: 12px;z-index: 1000;}</style>\n";
    public $groupCss = "<span class='groupInbox'>";
    public $cacheTimeouts = array(
        'messages' => 86400,    // 24 hours
        'folders' => 300,        // 5 mins
        'attachments' => 86400,    // 24 hours
    );
    public $userCacheDir = '';
    public $coreDynamicFolderQuery = "SELECT emails.id polymorphic_id, 'Emails' polymorphic_module FROM emails
                                   JOIN emails_text on emails.id = emails_text.email_id
                                   WHERE (type = '::TYPE::' OR status = '::STATUS::') AND assigned_user_id = '::USER_ID::' AND emails.deleted = '0'";

    /**
     * Setting this to false will prevent the email tick to be appended to the compose email link
     *
     * @var bool
     */
    public $appendTick = true;

    /**
     * Sole constructor
     */
    public function __construct()
    {
        global $sugar_config;
        global $current_user;

        $folderStateSerial = $current_user->getPreference('folderOpenState', 'Emails');

        if (!empty($folderStateSerial)) {
            $this->folderStates = sugar_unserialize($folderStateSerial);
        }

        $this->smarty = new Sugar_Smarty();
        $this->folder = new SugarFolder();
        $this->userCacheDir = sugar_cached("modules/Emails/{$current_user->id}");
        $this->db = DBManagerFactory::getInstance();
    }



    ///////////////////////////////////////////////////////////////////////////
    ////	CORE
    /**
     * Renders the frame for emails
     * @throws RuntimeException
     */
    public function displayEmailFrame($baseTpl = "modules/Emails/templates/_baseEmail.tpl")
    {
        require_once("include/OutboundEmail/OutboundEmail.php");

        global $app_strings, $app_list_strings;
        global $mod_strings;
        global $sugar_config;
        global $current_user;
        global $locale;
        global $timedate;
        global $theme;
        global $sugar_version;
        global $sugar_flavor;
        global $current_language;
        global $server_unique_key;

        $this->preflightUserCache();
        $ie = BeanFactory::newBean('InboundEmail');

        // focus listView
        $list = array(
            'mbox' => 'Home',
            'ieId' => '',
            'name' => 'Home',
            'unreadChecked' => 0,
            'out' => array(),
        );

        $this->_generateComposeConfigData('email_compose');


        //Check quick create module access
        $QCAvailableModules = $this->_loadQuickCreateModules();

        //Get the quickSearch js needed for assigned user id on Search Tab
        require_once('include/QuickSearchDefaults.php');
        $qsd = QuickSearchDefaults::getQuickSearchDefaults();
        $qsd->setFormName('advancedSearchForm');
        $quicksearchAssignedUser = "if(typeof sqs_objects == 'undefined'){var sqs_objects = new Array;}";
        $quicksearchAssignedUser .= "sqs_objects['advancedSearchForm_assigned_user_name']=" . json_encode($qsd->getQSUser()) . ";";
        $qsd->setFormName('Distribute');
        $quicksearchAssignedUser .= "sqs_objects['Distribute_assigned_user_name']=" . json_encode($qsd->getQSUser()) . ";";
        $this->smarty->assign('quickSearchForAssignedUser', $quicksearchAssignedUser);


        ///////////////////////////////////////////////////////////////////////
        ////	BASIC ASSIGNS
        $this->smarty->assign("currentUserId", $current_user->id);
        $this->smarty->assign("CURRENT_USER_EMAIL", $current_user->email1);
        $this->smarty->assign("currentUserName", $current_user->name);
        $this->smarty->assign('yuiPath', 'modules/Emails/javascript/yui-ext/');
        $this->smarty->assign('app_strings', $app_strings);
        $this->smarty->assign('mod_strings', $mod_strings);
        $this->smarty->assign('theme', $theme);
        $this->smarty->assign('sugar_config', $sugar_config);
        $this->smarty->assign('is_admin', $current_user->is_admin);
        $this->smarty->assign('sugar_version', $sugar_version);
        $this->smarty->assign('sugar_flavor', $sugar_flavor);
        $this->smarty->assign('current_language', $current_language);
        $this->smarty->assign('server_unique_key', $server_unique_key);
        $this->smarty->assign('qcModules', json_encode($QCAvailableModules));
        $extAllDebugValue = "ext-all.js";
        $this->smarty->assign('extFileName', $extAllDebugValue);

        $useRequestedRecord = false;
        if (isset($_REQUEST['record']) && $_REQUEST['record'] && $_REQUEST['record'] != $current_user->id) {
            $useRequestedRecord = true;
        }

        $user = $current_user;
        if ($useRequestedRecord) {
            $user = $current_user->getRequestedUserRecord();
        }

        // settings: general
        $e2UserPreferences = $this->getUserPreferencesJS($useRequestedRecord);
        $emailSettings = $e2UserPreferences['emailSettings'];

        ///////////////////////////////////////////////////////////////////////
        ////	USER SETTINGS
        // settings: accounts

        $cuDatePref = $user->getUserDateTimePreferences();
        $this->smarty->assign('dateFormat', $cuDatePref['date']);
        $this->smarty->assign(
            'dateFormatExample',
            str_replace(array("Y", "m", "d"), array("yyyy", "mm", "dd"), (string) $cuDatePref['date'])
        );
        $this->smarty->assign('calFormat', $timedate->get_cal_date_format());
        $this->smarty->assign('TIME_FORMAT', $timedate->get_user_time_format());

        $ieAccounts = $ie->retrieveByGroupId($user->id);
        $ieAccountsOptions = "<option value=''>{$app_strings['LBL_NONE']}</option>\n";

        foreach ($ieAccounts as $k => $v) {
            $disabled = (!$v->is_personal) ? "DISABLED" : "";
            $group = (!$v->is_personal) ? $app_strings['LBL_EMAIL_GROUP'] . "." : "";
            $ieAccountsOptions .= "<option value='{$v->id}' $disabled>{$group}{$v->name}</option>\n";
        }

        $this->smarty->assign('ieAccounts', $ieAccountsOptions);
        $this->smarty->assign('rollover', $this->rolloverStyle);

        $protocol = filterInboundEmailPopSelection($app_list_strings['dom_email_server_type']);
        $this->smarty->assign('PROTOCOL', get_select_options_with_id($protocol, ''));
        $this->smarty->assign(
            'MAIL_SSL_OPTIONS',
            get_select_options_with_id($app_list_strings['email_settings_for_ssl'], '')
        );
        $this->smarty->assign('ie_mod_strings', return_module_language($current_language, 'InboundEmail'));

        $charsetSelectedValue = isset($emailSettings['defaultOutboundCharset']) ? $emailSettings['defaultOutboundCharset'] : false;
        if (!$charsetSelectedValue) {
            $charsetSelectedValue = $user->getPreference('default_export_charset', 'global');
            if (!$charsetSelectedValue) {
                $charsetSelectedValue = $locale->getPrecedentPreference('default_email_charset');
            }
        }
        $charset = array(
            'options' => $locale->getCharsetSelect(),
            'selected' => $charsetSelectedValue,
        );
        $this->smarty->assign('charset', $charset);

        $emailCheckInterval = array(
            'options' => $app_strings['LBL_EMAIL_CHECK_INTERVAL_DOM'],
            'selected' => $emailSettings['emailCheckInterval']
        );
        $this->smarty->assign('emailCheckInterval', $emailCheckInterval);
        $this->smarty->assign('attachmentsSearchOptions', $app_list_strings['checkbox_dom']);
        $this->smarty->assign('sendPlainTextChecked', ($emailSettings['sendPlainText'] == 1) ? 'CHECKED' : '');
        $this->smarty->assign(
            'showNumInList',
            get_select_options_with_id($app_list_strings['email_settings_num_dom'], $emailSettings['showNumInList'])
        );

        ////	END USER SETTINGS
        ///////////////////////////////////////////////////////////////////////

        ///////////////////////////////////////////////////////////////////////
        ////	SIGNATURES

        $prependSignature = $user->getPreference('signature_prepend') ?
            'true' :
            'false';
        $defaultSignatureId = $user->getPreference('signature_default');
        $this->smarty->assign(
            'signatures',
            $user->getSignatures(false, $defaultSignatureId, false, 'signature_id')
        );
        $this->smarty->assign(
            'signaturesSettings',
            $user->getSignatures(false, $defaultSignatureId, false, 'signature_id')
        );
        $this->smarty->assign(
            'signaturesAccountSettings',
            $user->getEmailAccountSignatures(false, $defaultSignatureId, false, 'account_signature_id')
        );
        $signatureButtons = $user->getSignatureButtons(
            'SUGAR.email2.settings.createSignature',
            !empty($defaultSignatureId)
        );
        if (!empty($defaultSignatureId)) {
            $signatureButtons = $signatureButtons . '<span name="delete_sig" id="delete_sig" style="visibility:inherit;"><input class="button" onclick="SUGAR.email2.settings.deleteSignature();" value="' . $app_strings['LBL_EMAIL_DELETE'] . '" type="button" tabindex="392">&nbsp;
                    </span>';
        } else {
            $signatureButtons = $signatureButtons . '<span name="delete_sig" id="delete_sig" style="visibility:hidden;"><input class="button" onclick="SUGAR.email2.settings.deleteSignature();" value="' . $app_strings['LBL_EMAIL_DELETE'] . '" type="button" tabindex="392">&nbsp;
                    </span>';
        }
        $this->smarty->assign('signatureButtons', $signatureButtons);
        $this->smarty->assign('signaturePrepend', $prependSignature == 'true' ? 'CHECKED' : '');
        ////	END SIGNATURES
        ///////////////////////////////////////////////////////////////////////

        ///////////////////////////////////////////////////////////////////////
        ////	EMAIL TEMPLATES
        $email_templates_arr = $this->getEmailTemplatesArray();
        natcasesort($email_templates_arr);
        $this->smarty->assign('EMAIL_TEMPLATE_OPTIONS', get_select_options_with_id($email_templates_arr, ''));
        ////	END EMAIL TEMPLATES
        ///////////////////////////////////////////////////////////////////////

        ///////////////////////////////////////////////////////////////////////
        ////	FOLDERS & TreeView
        $this->smarty->assign(
            'groupUserOptions',
            $ie->getGroupsWithSelectOptions(array('' => $app_strings['LBL_EMAIL_CREATE_NEW']))
        );

        $tree = $this->getMailboxNodes();

        // preloaded folder
        $preloadFolder = 'lazyLoadFolder = ';
        $focusFolderSerial = $current_user->getPreference('focusFolder', 'Emails');
        if (!empty($focusFolderSerial)) {
            $focusFolder = sugar_unserialize($focusFolderSerial);
            //$focusFolder['ieId'], $focusFolder['folder']
            $preloadFolder .= json_encode($focusFolder) . ";";
        } else {
            $preloadFolder .= "new Object();";
        }
        ////	END FOLDERS
        ///////////////////////////////////////////////////////////////////////

        $out = "";
        $out .= $this->smarty->fetch($baseTpl);
        $out .= $tree->generate_header();
        $out .= $tree->generateNodesNoInit(true, 'email2treeinit');
        $out .= <<<eoq
            <script type="text/javascript" language="javascript">

                var loader = new YAHOO.util.YUILoader({
                    require : [
                        "layout", "element", "tabview", "menu",
                        "cookie", "sugarwidgets"
                    ],
                    loadOptional: true,
                    skin: { base: 'blank', defaultSkin: '' },
                    onSuccess: email2init,
                    allowRollup: true,
                    base: "include/javascript/yui/build/"
                });
                loader.addModule({
                    name :"sugarwidgets",
                    type : "js",
                    fullpath: "include/javascript/sugarwidgets/SugarYUIWidgets.js",
                    varName: "YAHOO.SUGAR",
                    requires: ["datatable", "dragdrop", "treeview", "tabview", "calendar"]
                });
                loader.insert();

                {$preloadFolder};

            </script>
eoq;


        return $out;
    }

    /**
     * Generate the frame needed for the quick compose email UI.  This frame is loaded dynamically
     * by an ajax call.
     *
     * @return JSON An object containing html markup and js script variables.
     */
    public function displayQuickComposeEmailFrame()
    {
        $this->preflightUserCache();

        $this->_generateComposeConfigData('email_compose_light');
        $javascriptOut = $this->smarty->fetch("modules/Emails/templates/_baseConfigData.tpl");

        $divOut = $this->smarty->fetch("modules/Emails/templates/overlay.tpl");
        $divOut .= $this->smarty->fetch("modules/Emails/templates/addressSearchContent.tpl");

        $outData = array('jsData' => $javascriptOut, 'divData' => $divOut);
        $out = json_encode($outData);

        return $out;
    }

    /**
     * Load the modules from the metadata file and include in a custom one if it exists
     *
     * @return array
     */
    protected function _loadQuickCreateModules()
    {
        $QCAvailableModules = array();
        $QCModules = array();

        include('modules/Emails/metadata/qcmodulesdefs.php');
        if (file_exists('custom/modules/Emails/metadata/qcmodulesdefs.php')) {
            include('custom/modules/Emails/metadata/qcmodulesdefs.php');
        }

        foreach ($QCModules as $module) {
            $seed = SugarModule::get($module)->loadBean();
            if (($seed instanceof SugarBean) && $seed->ACLAccess('edit')) {
                $QCAvailableModules[] = $module;
            }
        }

        return $QCAvailableModules;
    }

    /**
     * Given an email link url (eg. index.php?action=Compose&parent_type=Contacts...) break up the
     * request components and create a compose package that can be used by the quick compose UI. The
     * result is typically passed into the js call SUGAR.quickCompose.init which initalizes the quick compose
     * UI.
     *
     * @param String $emailLinkUrl
     * @return JSON Object containing the composePackage and full link url
     */
    public function generateComposePackageForQuickCreateFromComposeUrl($emailLinkUrl, $lazyLoad = false)
    {
        $composeData = explode("&", $emailLinkUrl);
        $a_composeData = array();
        foreach ($composeData as $singleRequest) {
            $tmp = explode("=", $singleRequest);
            $a_composeData[$tmp[0]] = urldecode($tmp[1]);
        }

        return $this->generateComposePackageForQuickCreate($a_composeData, $emailLinkUrl, $lazyLoad);
    }

    /**
     * @param string $module_name
     * @param string $record_id
     * @param string $name
     * @param string $addr
     * @param string $text
     * @return string
     */
    private function createEmailLink($module_name, $record_id, $name, $addr, $text)
    {
        global $current_user;

        if ($current_user->getEmailClient() == 'sugar') {
            $html =<<<HTML
            <a class="email-link" href="mailto:{$addr}"
                    onclick="$(document).openComposeViewModal(this);"
                    data-module="{$module_name}" data-record-id="{$record_id}"
                    data-module-name="{$name}" data-email-address="{$addr}"
                >{$text}</a>
HTML;
        } else {
            $html =<<<HTML
                <a class="email-link" href="mailto:{$addr}">{$text}</a>
HTML;
        }

        return $html;
    }

    /**
     *
     * @global SugarBean $focus
     * @param SugarBean|null $bean
     * @param string $emailField
     * @param bool $checkAllEmail
     * @param string|null $innerText
     * @param string|null $composeData
     * @return string
     * @throws RuntimeException
     * @throws InvalidArgumentException
     */
    public function populateComposeViewFields(
        $bean = null,
        $emailField = 'email1',
        $checkAllEmail = true,
        $innerText = null,
        $composeData = null
    ) {
        global $focus;
        $myBean = $focus;
        $configurator = new Configurator();

        $enableConfirmedOptIn = null;
        if (isset($configurator->config['email_enable_confirm_opt_in'])) {
            $enableConfirmedOptIn = $configurator->config['email_enable_confirm_opt_in'];
        } else {
            LoggerManager::getLogger()->warn('EmailUI::populateComposeViewFields: $configurator->config[email_enable_confirm_opt_in] is not set');
        }

        if (!empty($bean)) {
            $myBean = $bean;
        } else {
            $GLOBALS['log']->warn('EmailUI::populateComposeViewFields - $bean is empty');
        }

        $emailLink = $this->createEmailLink(
            $myBean->module_name,
            $myBean->id,
            $myBean->name,
            '',
            $innerText
        );

        // focus is set?
        if (!is_object($myBean)) {
            $GLOBALS['log']->warn('incorrect bean');
        } else {
            if (is_array($emailField)) {
                $emailFields = $emailField;
            } else {
                $emailFields = array($emailField);
            }


            if ($checkAllEmail) {
                $i = 1;
                $emailField = 'email' . $i;
                while (isset($myBean->{$emailField})) {
                    $emailFields[] = $emailField;
                    $i++;
                    $emailField = 'email' . $i;
                }
                $emailFields = array_unique($emailFields);
            }

            foreach ($emailFields as $emailField) {
                if (!empty($composeData)) {
                    $emailLink = $this->createEmailLink(
                        $composeData['parent_type'],
                        $composeData['parent_id'],
                        $composeData['parent_name'],
                        $composeData['to_addrs'],
                        ''
                    );
                } elseif (is_object($myBean) && (property_exists($myBean, $emailField))) {
                    $email_tick = $this->getEmailAddressConfirmOptInTick($myBean, $emailField);
                    $optOut = false;
                    $invalid = false;

                    if ($enableConfirmedOptIn === SugarEmailAddress::COI_STAT_DISABLED) {
                        $emailLink = $this->createEmailLink(
                            $myBean->module_name,
                            $myBean->id,
                            $myBean->name,
                            $myBean->{$emailField},
                            $myBean->{$emailField}
                        );
                        return $emailLink;
                    }


                    if (isset($myBean->emailAddress->addresses)) {
                        if (
                            isset($myBean->emailAddress)
                            && isset($myBean->emailAddress->addresses)
                        ) {
                            $addresses = $myBean->emailAddress->addresses;
                            foreach ($addresses as $address) {
                                if ($address['email_address'] === $myBean->{$emailField}) {
                                    if (!empty($myBean->id)) {
                                        $myBean->retrieve();
                                    }

                                    if ((int)$address['opt_out'] === 1) {
                                        $optOut = true;
                                    }

                                    if ((int)$address['invalid_email'] === 1) {
                                        $invalid = true;
                                    }

                                    if (
                                        $optOut === true
                                        || $invalid === true
                                    ) {
                                        $emailText = '';
                                        if ($this->appendTick) {
                                            $emailText .= $email_tick;
                                        }
                                        $emailText .= '<span class="email-line-through">';
                                        $emailText .= $myBean->{$emailField};
                                        $emailLink .= '</span>';

                                        $emailLink = $this->createEmailLink(
                                            $myBean->module_name,
                                            $myBean->id,
                                            $myBean->name,
                                            $myBean->{$emailField},
                                            $emailText
                                        );
                                    } else {
                                        $emailText = '';
                                        if ($this->appendTick) {
                                            $emailText .= $email_tick;
                                        }

                                        $emailText .= $myBean->{$emailField};

                                        $emailLink = $this->createEmailLink(
                                            $myBean->module_name,
                                            $myBean->id,
                                            $myBean->name,
                                            $myBean->{$emailField},
                                            $emailText
                                        );
                                    }
                                    return $emailLink;
                                }
                            }
                        }
                    } else {
                        $GLOBALS['log']->warn(get_class($myBean) . ' does not have email1 field');
                    }
                }
            }

            return $emailLink;
        }
    }

    /**
     * @return string
     */
    public function createBulkActionEmailLink()
    {
        global $app_strings;
        $emailLink = '<a class="email-link" href="javascript:void(0);"'
            . ' onclick="$(document).openComposeViewModal(this);"'
            . ' data-module="" data-record-id="" data-module-name="" data-email-address="">';
        $emailLink .= $app_strings['LBL_EMAIL_COMPOSE'];
        $emailLink .= '</a>';

        return $emailLink;
    }

    /**
     *
     * @param Basic|Object $myBean
     * @param string $emailField
     * @throws RuntimeException
     * @throws InvalidArgumentException
     * @return string
     */
    private function getEmailAddressConfirmOptInTick($myBean, $emailField)
    {
        $log = LoggerManager::getLogger();
        $tick = '';
        if ($myBean instanceof Basic) {
            $emailAddress = $myBean->getEmailAddressFromEmailField($emailField);
            if ($emailAddress instanceof SugarEmailAddress) {
                $tick = $emailAddress->getOptInStatusTickHTML();
            } else {
                $log->warn('Trying to get an email field of non-Basic object');
            }
        } else {
            $log->warn('Trying to get an email field of non-Basic object');
        }

        return $tick;
    }


    /**
     * Generate the composePackage for the quick compose email UI.  The package contains
     * key/value pairs generated by the Compose.php file which are then set into the
     * quick compose email UI (eg. to addr, parent id, parent type, etc)
     *
     * @param Array $composeData Associative array read and processed by generateComposeDataPackage.
     * @param String $fullLinkUrl A link that contains all pertinant information so the user can be
     *                              directed to the full compose screen if needed
     * @param SugarBean $bean Optional - the parent object bean with data
     * @return JSON Object containg composePackage and fullLinkUrl
     */
    public function generateComposePackageForQuickCreate($composeData, $fullLinkUrl, $lazyLoad = false, $bean = null)
    {
        $_REQUEST['forQuickCreate'] = true;

        if (!$lazyLoad) {
            require_once('modules/Emails/Compose.php');
            $composePackage = generateComposeDataPackage($composeData, false, $bean);
        } else {
            $composePackage = $composeData;
        }

        // JSON object is passed into the function defined within the a href onclick event
        // which is delimeted by '.  Need to escape all single quotes and &, <, >
        // but not double quotes since json would escape them
        foreach ($composePackage as $key => $singleCompose) {
            if (is_string($singleCompose)) {
                $composePackage[$key] = str_replace("&nbsp;", " ", (string) from_html($singleCompose));
            }
        }

        $quickComposeOptions = array('fullComposeUrl' => $fullLinkUrl, 'composePackage' => $composePackage);

        $j_quickComposeOptions = JSON::encode($quickComposeOptions, false, true);


        return $j_quickComposeOptions;
    }

    /**
     * Generate the config data needed for the Full Compose UI and the Quick Compose UI.  The set of config data
     * returned is the minimum set needed by the quick compose UI.
     *
     * @param String $type Drives which tinyMCE options will be included.
     * @throws RuntimeException
     */
    public function _generateComposeConfigData($type = "email_compose_light")
    {
        global $app_list_strings, $current_user, $app_strings, $mod_strings, $current_language, $locale;

        //Link drop-downs
        $parent_types = $app_list_strings['record_type_display'];
        $disabled_parent_types = ACLController::disabledModuleList($parent_types, false, 'list');

        foreach ($disabled_parent_types as $disabled_parent_type) {
            unset($parent_types[$disabled_parent_type]);
        }
        asort($parent_types);
        $linkBeans = json_encode(get_select_options_with_id($parent_types, ''));

        //TinyMCE Config
        require_once("include/SugarTinyMCE.php");
        $tiny = new SugarTinyMCE();
        $tinyConf = $tiny->getConfig($type);

        //Generate Language Packs
        $lang = "var app_strings = new Object();\n";
        foreach ($app_strings as $k => $v) {
            if (strpos((string) $k, 'LBL_EMAIL_') !== false) {
                $vJS = json_encode($v);
                $lang .= "app_strings.{$k} = {$vJS};\n";
            }
        }
        //Get the email mod strings but don't use the global variable as this may be overridden by
        //other modules when the quick create is rendered.
        $email_mod_strings = return_module_language($current_language, 'Emails');
        $modStrings = "var mod_strings = new Object();\n";
        foreach ($email_mod_strings as $k => $v) {
            $v = str_replace("'", "\'",str_replace("\\'", "'", (string) $v));
            $modStrings .= "mod_strings.{$k} = '{$v}';\n";
        }
        $lang .= "\n\n{$modStrings}\n";

        //Grab the Inboundemail language pack
        $ieModStrings = "var ie_mod_strings = new Object();\n";
        $ie_mod_strings = return_module_language($current_language, 'InboundEmail');
        foreach ($ie_mod_strings as $k => $v) {
            $v = str_replace("'", "\'", (string) $v);
            $ieModStrings .= "ie_mod_strings.{$k} = '{$v}';\n";
        }
        $lang .= "\n\n{$ieModStrings}\n";

        $this->smarty->assign('linkBeans', $linkBeans);
        $this->smarty->assign('linkBeansOptions', $parent_types);
        $this->smarty->assign('tinyMCE', $tinyConf);
        $this->smarty->assign('lang', $lang);
        $this->smarty->assign('app_strings', $app_strings);
        $this->smarty->assign('mod_strings', $email_mod_strings);
        $ie1 = BeanFactory::newBean('InboundEmail');

        //Signatures
        $defsigID = $current_user->getPreference('signature_default');
        $defaultSignature = $current_user->getDefaultSignature();
        $sigJson = !empty($defaultSignature) ? json_encode(array($defaultSignature['id'] => from_html($defaultSignature['signature_html']))) : "new Object()";
        $this->smarty->assign('defaultSignature', $sigJson);
        $this->smarty->assign('signatureDefaultId', (isset($defaultSignature['id'])) ? $defaultSignature['id'] : "");
        //User Preferences
        $this->smarty->assign('userPrefs', json_encode($this->getUserPrefsJS()));

        $useRequestedRecord = false;
        if (isset($_REQUEST['record']) && $_REQUEST['record'] && $_REQUEST['record'] != $current_user->id) {
            $useRequestedRecord = true;
        }

        $user = $current_user;
        if ($useRequestedRecord) {
            $user = $current_user->getRequestedUserRecord();
        }

        $defaultSignature = $user->getDefaultSignature();
        $sigJson = !empty($defaultSignature) ?
            json_encode(array($defaultSignature['id'] => from_html($defaultSignature['signature_html']))) :
            'new Object()';
        $this->smarty->assign('defaultSignature', $sigJson);
        $this->smarty->assign(
            'signatureDefaultId',
            isset($defaultSignature['id']) ? $defaultSignature['id'] : ''
        );
        //User Preferences
        $this->smarty->assign(
            'userPrefs',
            json_encode($this->getUserPreferencesJS($useRequestedRecord))
        );

        //Get the users default outbound id
        $defaultOutID = $ie1->getUsersDefaultOutboundServerId($user);
        $this->smarty->assign('defaultOutID', $defaultOutID);

        //Character Set
        $charsets = json_encode($locale->getCharsetSelect());
        $this->smarty->assign('emailCharsets', $charsets);

        //Relateable List of People for address book search
        //#20776 jchi
        $peopleTables = array(
            "users",
            "contacts",
            "leads",
            "prospects",
            "accounts"
        );
        $filterPeopleTables = array();
        global $app_list_strings, $app_strings;
        $filterPeopleTables['LBL_DROPDOWN_LIST_ALL'] = $app_strings['LBL_DROPDOWN_LIST_ALL'];
        foreach ($peopleTables as $table) {
            $module = ucfirst($table);
            $class = substr($module, 0, strlen($module) - 1);
            require_once("modules/{$module}/{$class}.php");
            $person = new $class();

            if (!$person->ACLAccess('list')) {
                continue;
            }
            $filterPeopleTables[$person->table_name] = $app_list_strings['moduleList'][$person->module_dir];
        }
        $this->smarty->assign('listOfPersons', get_select_options_with_id($filterPeopleTables, ''));
    }



    ////	END CORE
    ///////////////////////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////////////////////
    ////	ADDRESS BOOK
    /**
     * Retrieves all relationship metadata for a user's address book
     * @return array
     */
    public function getContacts()
    {
        global $current_user;

        $q = "SELECT * FROM address_book WHERE assigned_user_id = '{$current_user->id}' ORDER BY bean DESC";
        $r = $this->db->query($q);

        $ret = array();

        while ($a = $this->db->fetchByAssoc($r)) {
            $ret[$a['bean_id']] = array(
                'id' => $a['bean_id'],
                'module' => $a['bean'],
            );
        }

        return $ret;
    }

    /**
     * Saves changes to a user's address book
     * @param array contacts
     * @throws SuiteException
     */
    public function setContacts($contacts)
    {
        global $current_user;

        $oldContacts = $this->getContacts();

        foreach ($contacts as $cid => $contact) {
            if (!in_array($contact['id'], $oldContacts, true)) {
                $contactId = $contact['id'];
                $isValidator = new SuiteValidator();
                if (!$isValidator->isValidId($contactId)) {
                    throw new SuiteException('Invalid contact ID: ' . $contactId);
                }

                $q = "INSERT INTO address_book (assigned_user_id, bean, bean_id) VALUES ('{$current_user->id}', '{$contact['module']}', '{$contact['id']}')";
                $r = $this->db->query($q, true);
            }
        }
    }

    /**
     * Removes contacts from the user's address book
     * @param array ids
     * @throws SuiteException
     */
    public function removeContacts($ids)
    {
        global $current_user;

        $concat = '';

        foreach ($ids as $id) {
            if (!empty($concat)) {
                $concat .= ', ';
            }

            $isValidator = new SuiteValidator();
            if (!$isValidator->isValidId($id)) {
                throw new SuiteException('Invalid contact ID' . $id);
            }
            $concat .= "'{$id}'";
        }

        $q = "DELETE FROM address_book WHERE assigned_user_id = '{$current_user->id}' AND bean_id IN ({$concat})";
        $r = $this->db->query($q);
    }

    /**
     * saves editted Contact info
     * @param string $str JSON serialized object
     */
    public function saveContactEdit($str)
    {
        $json = getJSONobj();

        $str = from_html($str);
        $obj = $json->decode($str);

        $contact = BeanFactory::newBean('Contacts');
        $contact->retrieve($obj['contact_id']);
        $contact->first_name = $obj['contact_first_name'];
        $contact->last_name = $obj['contact_last_name'];
        $contact->save();

        // handle email address changes
        $addresses = array();

        foreach ($obj as $k => $req) {
            if (strpos((string) $k, 'emailAddress') !== false) {
                $addresses[$k] = $req;
            }
        }

        // prefill some REQUEST vars for emailAddress save
        $_REQUEST['emailAddressOptOutFlag'] = $obj['optOut'];
        $_REQUEST['emailAddressInvalidFlag'] = $obj['invalid'];
        $contact->emailAddress->saveEmail($obj['contact_id'], 'Contacts', $addresses, $obj['primary'], '');
    }

    /**
     * Prepares the Edit Contact mini-form via template assignment
     * @param string id ID of contact in question
     * @param string module Module in focus
     * @return array
     */
    public function getEditContact($id, $module)
    {
        global $app_strings;


        if (!class_exists("Contact")) {
        }

        $contact = BeanFactory::newBean('Contacts');
        $contact->retrieve($_REQUEST['id']);
        $ret = array();

        if ($contact->ACLAccess('edit')) {
            $contactMeta = array();
            $contactMeta['id'] = $contact->id;
            $contactMeta['module'] = $contact->module_dir;
            $contactMeta['first_name'] = $contact->first_name;
            $contactMeta['last_name'] = $contact->last_name;

            $this->smarty->assign("app_strings", $app_strings);
            $this->smarty->assign(
                "contact_strings",
                return_module_language(get_current_language(), 'Contacts')
            );
            $this->smarty->assign("contact", $contactMeta);

            $ea = new SugarEmailAddress();
            $newEmail = $ea->getEmailAddressWidgetEditView($id, $module, true);
            $this->smarty->assign("emailWidget", $newEmail['html']);

            $ret['form'] = $this->smarty->fetch("modules/Emails/templates/editContact.tpl");
            $ret['prefillData'] = $newEmail['prefillData'];
        } else {
            $id = "";
            $ret['form'] = $app_strings['LBL_EMAIL_ERROR_NO_ACCESS'];
            $ret['prefillData'] = '{}';
        }

        $ret['id'] = $id;
        $ret['contactName'] = $contact->full_name;

        return $ret;
    }


    /**
     * Retrieves a concatenated list of contacts, those with assigned_user_id = user's id and those in the address_book
     * table
     * @param array $contacts Array of contact types -> IDs
     * @param object $user User in focus
     * @return array
     */
    public function getUserContacts($contacts, $user = null)
    {
        global $current_user;
        global $locale;

        if (empty($user)) {
            $user = $current_user;
        }

        $emailAddress = new SugarEmailAddress();
        $ret = array();

        $union = '';

        $modules = array();
        foreach ($contacts as $contact) {
            if (!isset($modules[$contact['module']])) {
                $modules[$contact['module']] = array();
            }
            $modules[$contact['module']][] = $contact;
        }

        foreach ($modules as $module => $contacts) {
            if (!empty($union)) {
                $union .= " UNION ALL ";
            }

            $table = strtolower($module);
            $idsSerial = '';

            foreach ($contacts as $contact) {
                if (!empty($idsSerial)) {
                    $idsSerial .= ",";
                }
                $idsSerial .= "'{$contact['id']}'";
            }

            $union .= "(SELECT id, first_name, last_name, title, '{$module}' module FROM {$table} WHERE id IN({$idsSerial}) AND deleted = 0 )";
        }
        if (!empty($union)) {
            $union .= " ORDER BY last_name";
        }

        $r = $user->db->query($union);

        while ($a = $user->db->fetchByAssoc($r)) {
            $c = array();

            $c['name'] = $locale->getLocaleFormattedName(
                $a['first_name'],
                "<b>{$a['last_name']}</b>",
                '',
                $a['title'],
                '',
                $user
            );
            $c['id'] = $a['id'];
            $c['module'] = $a['module'];
            $c['email'] = $emailAddress->getAddressesByGUID($a['id'], $a['module']);
            $ret[$a['id']] = $c;
        }

        return $ret;
    }
    ////	END ADDRESS BOOK
    ///////////////////////////////////////////////////////////////////////////


    ///////////////////////////////////////////////////////////////////////////
    ////	EMAIL 2.0 Preferences
    /**
     * @param bool $useRequestedRecord
     * @return array
     * @throws RuntimeException
     */
    public function getUserPreferencesJS($useRequestedRecord = false)
    {
        global $current_user;

        $user = $current_user;
        if ($useRequestedRecord) {
            $user = $current_user->getRequestedUserRecord();
        }

        // sort order per mailbox view
        $sortSerial = $user->getPreference('folderSortOrder', 'Emails');
        $sortArray = array();
        if (!empty($sortSerial)) {
            $sortArray = sugar_unserialize($sortSerial);
        }

        // treeview collapsed/open states
        $folderStateSerial = $user->getPreference('folderOpenState', 'Emails');
        $folderStates = array();
        if (!empty($folderStateSerial)) {
            $folderStates = sugar_unserialize($folderStateSerial);
        }

        // subscribed accounts
        $showFolders = sugar_unserialize(base64_decode($user->getPreference('showFolders', 'Emails')));

        // general settings
        $emailSettings = $user->getPreference('emailSettings', 'Emails');

        if (empty($emailSettings)) {
            $emailSettings = array();
            $emailSettings['emailCheckInterval'] = -1;
            $emailSettings['autoImport'] = '';
            $emailSettings['alwaysSaveOutbound'] = '1';
            $emailSettings['sendPlainText'] = '';
            $emailSettings['defaultOutboundCharset'] = $GLOBALS['sugar_config']['default_email_charset'];
            $emailSettings['showNumInList'] = 20;
        }

        // focus folder
        $focusFolder = $user->getPreference('focusFolder', 'Emails');
        $focusFolder = !empty($focusFolder) ? sugar_unserialize($focusFolder) : array();

        // unread only flag
        $showUnreadOnly = $user->getPreference('showUnreadOnly', 'Emails');

        $listViewSort = array(
            "sortBy" => 'date',
            "sortDirection" => 'DESC',
        );

        // signature prefs
        $signaturePrepend = $user->getPreference('signature_prepend') ? 'true' : 'false';
        $signatureDefault = $user->getPreference('signature_default');
        $signatures = array(
            'signature_prepend' => $signaturePrepend,
            'signature_default' => $signatureDefault
        );


        // current_user
        $user = array(
            'emailAddresses' => $user->emailAddress->getAddressesByGUID($user->id, 'Users'),
            'full_name' => from_html($user->full_name),
        );

        $userPreferences = array();
        $userPreferences['sort'] = $sortArray;
        $userPreferences['folderStates'] = $folderStates;
        $userPreferences['showFolders'] = $showFolders;
        $userPreferences['emailSettings'] = $emailSettings;
        $userPreferences['focusFolder'] = $focusFolder;
        $userPreferences['showUnreadOnly'] = $showUnreadOnly;
        $userPreferences['listViewSort'] = $listViewSort;
        $userPreferences['signatures'] = $signatures;
        $userPreferences['current_user'] = $user;

        return $userPreferences;
    }

    public function getUserPrefsJS($useRequestedRecord = false)
    {
        return $this->getUserPreferencesJS($useRequestedRecord);
    }

    ///////////////////////////////////////////////////////////////////////////
    ////	FOLDER FUNCTIONS

    /**
     * Creates a new Sugar folder
     * @param string $nodeLabel New sugar folder name
     * @param string $parentLabel Parent folder name
     */
    public function saveNewFolder($nodeLabel, $parentId, $isGroup = 0)
    {
        global $current_user;

        $this->folder->name = $nodeLabel;
        $this->folder->is_group = $isGroup;
        $this->folder->parent_folder = ($parentId == 'Home') ? "" : $parentId;
        $this->folder->has_child = 0;
        $this->folder->created_by = $current_user->id;
        $this->folder->modified_by = $current_user->id;
        $this->folder->date_modified = $this->folder->date_created = TimeDate::getInstance()->nowDb();

        $this->folder->save();

        return array(
            'action' => 'newFolderSave',
            'id' => $this->folder->id,
            'name' => $this->folder->name,
            'is_group' => $this->folder->is_group,
            'is_dynamic' => $this->folder->is_dynamic
        );
    }

    /**
     * Saves user sort prefernces
     */
    public function saveListViewSortOrder($ieId, $focusFolder, $sortBy, $sortDir)
    {
        global $current_user;

        $sortArray = array();

        $sortSerial = $current_user->getPreference('folderSortOrder', 'Emails');
        if (!empty($sortSerial)) {
            $sortArray = sugar_unserialize($sortSerial);
        }

        $sortArray[$ieId][$focusFolder]['current']['sort'] = $sortBy;
        $sortArray[$ieId][$focusFolder]['current']['direction'] = $sortDir;
        $sortSerial = serialize($sortArray);
        $current_user->setPreference('folderSortOrder', $sortSerial, '', 'Emails');
    }

    /**
     * Stickies folder collapse/open state
     */
    public function saveFolderOpenState($focusFolder, $focusFolderOpen)
    {
        global $current_user;

        $folderStateSerial = $current_user->getPreference('folderOpenState', 'Emails');
        $folderStates = array();

        if (!empty($folderStateSerial)) {
            $folderStates = sugar_unserialize($folderStateSerial);
        }

        $folderStates[$focusFolder] = $focusFolderOpen;
        $newFolderStateSerial = serialize($folderStates);
        $current_user->setPreference('folderOpenState', $newFolderStateSerial, '', 'Emails');
    }

    /**
     * saves a folder's view state
     */
    public function saveListView($ieId, $folder)
    {
        global $current_user;

        $saveState = array();
        $saveState['ieId'] = $ieId;
        $saveState['folder'] = $folder;
        $saveStateSerial = serialize($saveState);
        $current_user->setPreference('focusFolder', $saveStateSerial, '', 'Emails');
    }

    /**
     * Generates cache folder structure
     */
    public function preflightEmailCache($cacheRoot)
    {
        // base
        if (!file_exists($cacheRoot)) {
            mkdir_recursive(clean_path($cacheRoot));
        }

        // folders
        if (!file_exists($cacheRoot . "/folders")) {
            mkdir_recursive(clean_path("{$cacheRoot}/folders"));
        }

        // messages
        if (!file_exists($cacheRoot . "/messages")) {
            mkdir_recursive(clean_path("{$cacheRoot}/messages"));
        }

        // attachments
        if (!file_exists($cacheRoot . "/attachments")) {
            mkdir_recursive(clean_path("{$cacheRoot}/attachments"));
        }
    }

    public function deleteEmailCacheForFolders($cacheRoot)
    {
        $filePath = $cacheRoot . "/folders/folders.php";
        if (file_exists($filePath)) {
            unlink($filePath);
        }
    }
    ///////////////////////////////////////////////////////////////////////////
    ////	IMAP FUNCTIONS
    /**
     * Identifies subscribed mailboxes and empties the trash
     * @param object $ie InboundEmail
     */
    public function emptyTrash(&$ie)
    {
        global $current_user;

        $showFolders = sugar_unserialize(base64_decode($current_user->getPreference('showFolders', 'Emails')));

        if (is_array($showFolders)) {
            foreach ($showFolders as $ieId) {
                if (!empty($ieId)) {
                    $ie->retrieve($ieId);
                    $ie->emptyTrash();
                }
            }
        }
    }

    /**
     * returns an array of nodes that correspond to IMAP mailboxes.
     * @param bool $forceRefresh
     * @param User|null $user User
     * @return object TreeView object
     */
    public function getMailboxNodes($forceRefresh = false, $user = null)
    {
        global $sugar_config;
        global $current_user;
        global $app_strings;

        if (!$user) {
            $user = $current_user;
        }

        $tree = new Tree("frameFolders");
        $tree->tree_style = 'include/ytree/TreeView/css/check/tree.css';

        $nodes = array();
        $ie = BeanFactory::newBean('InboundEmail');
        $refreshOffset = $this->cacheTimeouts['folders']; // 5 mins.  this will be set via user prefs

        $rootNode = new ExtNode($app_strings['LBL_EMAIL_HOME_FOLDER'], $app_strings['LBL_EMAIL_HOME_FOLDER']);
        $rootNode->dynamicloadfunction = '';
        $rootNode->expanded = true;
        $rootNode->dynamic_load = true;
        $showFolders = sugar_unserialize(base64_decode($user->getPreference('showFolders', 'Emails')));

        if (empty($showFolders)) {
            $showFolders = array();
        }

        // INBOX NODES
        if ($user->hasPersonalEmail()) {
            $personals = $ie->retrieveByGroupId($user->id);

            foreach ($personals as $k => $personalAccount) {
                if (in_array($personalAccount->id, $showFolders)) {
                    // check for cache value
                    $cacheRoot = sugar_cached("modules/Emails/{$personalAccount->id}");
                    $this->preflightEmailCache($cacheRoot);

                    if ($this->validCacheFileExists($personalAccount->id, 'folders', "folders.php")) {
                        $mailboxes = $this->getMailBoxesFromCacheValue($personalAccount);
                    } else {
                        $mailboxes = $personalAccount->getMailboxes();
                    }

                    $acctNode = new ExtNode('Home::' . $personalAccount->name, $personalAccount->name);
                    $acctNode->dynamicloadfunction = '';
                    $acctNode->expanded = false;
                    $acctNode->set_property('cls', 'ieFolder');
                    $acctNode->set_property('ieId', $personalAccount->id);
                    $acctNode->set_property('protocol', $personalAccount->protocol);

                    if (array_key_exists('Home::' . $personalAccount->name, $this->folderStates)) {
                        if ($this->folderStates['Home::' . $personalAccount->name] == 'open') {
                            $acctNode->expanded = true;
                        }
                    }
                    $acctNode->dynamic_load = true;

                    $nodePath = $acctNode->_properties['id'];

                    foreach ($mailboxes as $k => $mbox) {
                        $acctNode->add_node($this->buildTreeNode(
                            $k,
                            $k,
                            $mbox,
                            $personalAccount->id,
                            $nodePath,
                            false,
                            $personalAccount
                        ));
                    }

                    $rootNode->add_node($acctNode);
                }
            }
        }

        // GROUP INBOX NODES
        $beans = $ie->retrieveAllByGroupId($user->id, false);
        foreach ($beans as $k => $groupAccount) {
            if (in_array($groupAccount->id, $showFolders)) {
                // check for cache value
                $cacheRoot = sugar_cached("modules/Emails/{$groupAccount->id}");
                $this->preflightEmailCache($cacheRoot);
                //$groupAccount->connectMailserver();

                if ($this->validCacheFileExists($groupAccount->id, 'folders', "folders.php")) {
                    $mailboxes = $this->getMailBoxesFromCacheValue($groupAccount);
                } else {
                    $mailboxes = $groupAccount->getMailBoxesForGroupAccount();
                }

                $acctNode = new ExtNode($groupAccount->name, "group.{$groupAccount->name}");
                $acctNode->dynamicloadfunction = '';
                $acctNode->expanded = false;
                $acctNode->set_property('isGroup', 'true');
                $acctNode->set_property('ieId', $groupAccount->id);
                $acctNode->set_property('protocol', $groupAccount->protocol);

                if (array_key_exists('Home::' . $groupAccount->name, $this->folderStates)) {
                    if ($this->folderStates['Home::' . $groupAccount->name] == 'open') {
                        $acctNode->expanded = true;
                    }
                }
                $acctNode->dynamic_load = true;
                $nodePath = $rootNode->_properties['id'] . "::" . $acctNode->_properties['id'];

                foreach ($mailboxes as $k => $mbox) {
                    $acctNode->add_node($this->buildTreeNode(
                        $k,
                        $k,
                        $mbox,
                        $groupAccount->id,
                        $nodePath,
                        true,
                        $groupAccount
                    ));
                }

                $rootNode->add_node($acctNode);
            }
        }

        // SugarFolder nodes
        /* SugarFolders are built at onload when the UI renders */

        $tree->add_node($rootNode);

        return $tree;
    }

    public function getMailBoxesFromCacheValue($mailAccount)
    {
        $cacheRoot = sugar_cached("modules/Emails/{$mailAccount->id}");
        $foldersCache = $this->getCacheValue($mailAccount->id, 'folders', "folders.php", 'foldersCache');
        $mailboxes = $foldersCache['mailboxes'];
        $mailboxesArray = $mailAccount->generateFlatArrayFromMultiDimArray(
            $mailboxes,
            $mailAccount->retrieveDelimiter()
        );
        $mailAccount->saveMailBoxFolders($mailboxesArray);
        $this->deleteEmailCacheForFolders($cacheRoot);

        return $mailboxes;
    } // fn

    /**
     * Builds up a TreeView Node object
     * @param mixed
     * @param mixed
     * @param string
     * @param string ID of InboundEmail instance
     * @param string nodePath Serialized path from root node to current node
     * @param bool isGroup
     * @param bool forceRefresh
     * @return mixed
     */
    public function buildTreeNode($key, $label, $mbox, $ieId, $nodePath, $isGroup, $ie)
    {
        global $sugar_config;

        // get unread counts
        $exMbox = explode("::", $nodePath);
        $unseen = 0;
        $GLOBALS['log']->debug("$key --- $nodePath::$label");

        if (count($exMbox) >= 2) {
            $mailbox = "";
            $exMboxCount = count($exMbox);
            for ($i = 2; $i < $exMboxCount; $i++) {
                if ($mailbox != "") {
                    $mailbox .= ".";
                }
                $mailbox .= $exMbox[$i];
            }

            $mailbox = substr((string) $key, strpos((string) $key, '.'));

            $unseen = $this->getUnreadCount($ie, $mailbox);

            if ($unseen > 0) {
                //$label = " <span id='span{$ie->id}{$ie->mailbox}' style='font-weight:bold'>{$label} (<span id='span{$ie->id}{$ie->mailbox}nums'>{$unseen}</span>)</span>";
            }
        }

        $nodePath = $nodePath . "::" . $label;
        $node = new ExtNode($nodePath, $label);
        $node->dynamicloadfunction = '';
        $node->expanded = false;
        $node->set_property('labelStyle', "remoteFolder");


        if (array_key_exists($nodePath, $this->folderStates)) {
            if ($this->folderStates[$nodePath] == 'open') {
                $node->expanded = true;
            }
        }

        $group = ($isGroup) ? 'true' : 'false';
        $node->dynamic_load = true;
        //$node->set_property('href', " SUGAR.email2.listView.populateListFrame(YAHOO.namespace('frameFolders').selectednode, '{$ieId}', 'false');");
        $node->set_property('isGroup', $group);
        $node->set_property('isDynamic', 'false');
        $node->set_property('ieId', $ieId);
        $node->set_property('mbox', $key);
        $node->set_property('unseen', $unseen);
        $node->set_property('cls', 'ieFolder');

        if (is_array($mbox)) {
            foreach ($mbox as $k => $v) {
                $node->add_node($this->buildTreeNode("$key.$k", $k, $v, $ieId, $nodePath, $isGroup, $ie));
            }
        }

        return $node;
    }

    /**
     * Totals the unread emails
     */
    public function getUnreadCount(&$ie, $mailbox)
    {
        global $sugar_config;
        $unseen = 0;

        // use cache
        return $ie->getCacheUnreadCount($mailbox);
    }

    ///////////////////////////////////////////////////////////////////////////
    ////	DISPLAY CODE
    /**
     * Used exclusively by draft code.  Returns Notes and Documents as attachments.
     * @param array $ret
     * @return array
     */
    public function getDraftAttachments($ret)
    {
        $db = DBManagerFactory::getInstance();

        // $ret['uid'] is the draft Email object's GUID
        $ret['attachments'] = array();

        $q = "SELECT id, filename FROM notes WHERE parent_id = '{$ret['uid']}' AND deleted = 0";
        $r = $db->query($q);

        while ($a = $db->fetchByAssoc($r)) {
            $ret['attachments'][$a['id']] = array(
                'id' => $a['id'],
                'filename' => $a['filename'],
            );
        }

        return $ret;
    }

    public function createCopyOfInboundAttachment($ie, $ret, $uid)
    {
        global $sugar_config;

        $cacheFile = [];

        if ($ie->isPop3Protocol()) {
            // get the UIDL from database;
            $cachedUIDL = md5($uid);
            $cache = sugar_cached("modules/Emails/{$ie->id}/messages/{$ie->mailbox}{$cachedUIDL}.php");
        } else {
            $cache = sugar_cached("modules/Emails/{$ie->id}/messages/{$ie->mailbox}{$uid}.php");
        }
        if (file_exists($cache)) {
            include($cache); // profides $cacheFile
            $metaOut = unserialize($cacheFile['out'], ['allowed_classes' => false]);
            $meta = $metaOut['meta']['email'];
            if (isset($meta['attachments'])) {
                $attachmentHtmlData = $meta['attachments'];
                $actualAttachmentInfo = array();
                $this->parseAttachmentInfo($actualAttachmentInfo, $attachmentHtmlData);
                if ((is_countable($actualAttachmentInfo) ? count($actualAttachmentInfo) : 0) > 0) {
                    foreach ($actualAttachmentInfo as $key => $value) {
                        $info_vars = array();
                        parse_str($value, $info_vars);
                        $fileName = $info_vars['tempName'];
                        $attachmentid = $info_vars['id'];
                        $guid = create_guid();
                        $destination = clean_path("{$this->userCacheDir}/{$guid}");

                        $attachmentFilePath = sugar_cached("modules/Emails/{$ie->id}/attachments/{$attachmentid}");
                        copy($attachmentFilePath, $destination);
                        $ret['attachments'][$guid] = array();
                        $ret['attachments'][$guid]['id'] = $guid . $fileName;
                        $ret['attachments'][$guid]['filename'] = $fileName;
                    } // for
                } // if
            } // if
        } // if

        return $ret;
    } // fn

    public function parseAttachmentInfo(&$actualAttachmentInfo, $attachmentHtmlData)
    {
        $downLoadPHP = strpos((string) $attachmentHtmlData, "index.php?entryPoint=download&");
        while ($downLoadPHP) {
            $attachmentHtmlData = substr((string) $attachmentHtmlData, $downLoadPHP + 30);
            $final = strpos($attachmentHtmlData, "\">");
            $actualAttachmentInfo[] = substr($attachmentHtmlData, 0, $final);
            $attachmentHtmlData = substr($attachmentHtmlData, $final);
            $downLoadPHP = strpos($attachmentHtmlData, "index.php?entryPoint=download&");
        } // while
    }

    /**
     * Renders the QuickCreate form from Smarty and returns HTML
     * @param array $vars request variable global
     * @param object $email Fetched email object
     * @param bool $addToAddressBook
     * @return array
     */
    public function getQuickCreateForm($vars, $email, $addToAddressBookButton = false)
    {
        require_once("include/EditView/EditView2.php");
        global $app_strings;
        global $mod_strings;
        global $current_user;
        global $beanList;
        global $beanFiles;
        global $current_language;

        //Setup the current module languge
        $mod_strings = return_module_language($current_language, $_REQUEST['qc_module']);

        $bean = $beanList[$_REQUEST['qc_module']];
        $class = $beanFiles[$bean];
        require_once($class);

        $focus = new $bean();

        $people = array(
            'Contact'
        ,
            'Lead'
        );
        $emailAddress = array();

        // people
        if (in_array($bean, $people)) {
            // lead specific
            $focus->lead_source = 'Email';
            $focus->lead_source_description = trim($email->name);

            $from = (isset($email->from_name) && !empty($email->from_name)) ? $email->from_name : $email->from_addr;

            if (isset($_REQUEST['sugarEmail']) && !empty($_REQUEST['sugarEmail'])) {
                if ($email->status == "sent") {
                    $from = (isset($email->to_addrs_names) && !empty($email->to_addrs_names)) ? $email->to_addrs_names : $email->to_addrs;
                } else {
                    $from = (isset($email->from_name) && !empty($email->from_name)) ? $email->from_name : $email->from_addr_name;
                }
            }

            $name = explode(" ", trim($from));

            $address = trim(array_pop($name));
            $address = str_replace(array("<", ">", "&lt;", "&gt;"), "", $address);

            isValidEmailAddress($address);

            $emailAddress[] = array(
                'email_address' => $address,
                'primary_address' => 1,
                'invalid_email' => 0,
                'opt_out' => 0,
                'reply_to_address' => 1
            );

            $focus->email1 = $address;

            if (!empty($name)) {
                $focus->last_name = trim(array_pop($name));

                foreach ($name as $first) {
                    if (!empty($focus->first_name)) {
                        $focus->first_name .= " ";
                    }
                    $focus->first_name .= trim($first);
                }
            }
        } else {
            // bugs, cases, tasks
            $focus->name = trim($email->name);
        }

        $focus->description = trim(strip_tags($email->description));
        $focus->assigned_user_id = $current_user->id;


        $EditView = new EditView();
        $EditView->ss = new Sugar_Smarty();
        //MFH BUG#20283 - checks for custom quickcreate fields
        $EditView->setup(
            $_REQUEST['qc_module'],
            $focus,
            'custom/modules/' . $focus->module_dir . '/metadata/editviewdefs.php',
            'include/EditView/EditView.tpl'
        );
        $EditView->process();
        $EditView->render();

        $EditView->defs['templateMeta']['form']['buttons'] = array(
            'email2save' => array(
                'id' => 'e2AjaxSave',
                'customCode' => '<input type="button" class="button" value="   ' . $app_strings['LBL_SAVE_BUTTON_LABEL']
                    . '   " onclick="SUGAR.email2.detailView.saveQuickCreate(false);" />'
            ),
            'email2saveandreply' => array(
                'id' => 'e2SaveAndReply',
                'customCode' => '<input type="button" class="button" value="   ' . $app_strings['LBL_EMAIL_SAVE_AND_REPLY']
                    . '   " onclick="SUGAR.email2.detailView.saveQuickCreate(\'reply\');" />'
            ),
            'email2cancel' => array(
                'id' => 'e2cancel',
                'customCode' => '<input type="button" class="button" value="   ' . $app_strings['LBL_EMAIL_CANCEL']
                    . '   " onclick="SUGAR.email2.detailView.quickCreateDialog.hide();" />'
            )
        );


        if ($addToAddressBookButton) {
            $EditView->defs['templateMeta']['form']['buttons']['email2saveAddToAddressBook'] = array(
                'id' => 'e2addToAddressBook',
                'customCode' => '<input type="button" class="button" value="   ' . $app_strings['LBL_EMAIL_ADDRESS_BOOK_SAVE_AND_ADD']
                    . '   " onclick="SUGAR.email2.detailView.saveQuickCreate(true);" />'
            );
        }

        //Get the module language for javascript
        if (!is_file(sugar_cached('jsLanguage/') . $_REQUEST['qc_module'] . '/' . $GLOBALS['current_language'] . '.js')) {
            require_once('include/language/jsLanguage.php');
            jsLanguage::createModuleStringsCache($_REQUEST['qc_module'], $GLOBALS['current_language']);
        }
        $jsLanguage = getVersionedScript(
            "cache/jsLanguage/{$_REQUEST['qc_module']}/{$GLOBALS['current_language']}.js",
            $GLOBALS['sugar_config']['js_lang_version']
        );


        $EditView->view = 'EmailQCView';
        $EditView->defs['templateMeta']['form']['headerTpl'] = 'include/EditView/header.tpl';
        $EditView->defs['templateMeta']['form']['footerTpl'] = 'include/EditView/footer.tpl';
        $meta = array();
        $meta['html'] = $jsLanguage . $EditView->display(false, true);
        $meta['html'] = str_replace(
            "src='" . getVersionedPath('include/SugarEmailAddress/SugarEmailAddress.js') . "'",
            '',
            $meta['html']
        );
        $meta['emailAddress'] = $emailAddress;

        $mod_strings = return_module_language($current_language, 'Emails');

        return $meta;
    }

    /**
     * Renders the Import form from Smarty and returns HTML
     * @param array $vars request variable global
     * @param object $email Fetched email object
     * @param bool $addToAddressBook
     * @return array
     */
    public function getImportForm($vars, $email, $formName = 'ImportEditView')
    {
        require_once("include/EditView/EditView2.php");
        require_once("include/TemplateHandler/TemplateHandler.php");
        require_once('include/QuickSearchDefaults.php');
        $qsd = QuickSearchDefaults::getQuickSearchDefaults();
        $qsd->setFormName($formName);

        global $app_strings;
        global $current_user;
        global $app_list_strings;
        $sqs_objects = array(
            "{$formName}_parent_name" => $qsd->getQSParent(),
        );
        $smarty = new Sugar_Smarty();
        $smarty->assign("APP", $app_strings);
        $smarty->assign('formName', $formName);
        $showAssignTo = false;
        if (!isset($vars['showAssignTo']) || $vars['showAssignTo'] == true) {
            $showAssignTo = true;
        } // if
        if ($showAssignTo) {
            if (empty($email->assigned_user_id) && empty($email->id)) {
                $email->assigned_user_id = $current_user->id;
            }
            if (empty($email->assigned_name) && empty($email->id)) {
                $email->assigned_user_name = $current_user->user_name;
            }
            $sqs_objects["{$formName}_assigned_user_name"] = $qsd->getQSUser();
        }
        $smarty->assign("showAssignedTo", $showAssignTo);

        $showDelete = false;
        if (!isset($vars['showDelete']) || $vars['showDelete'] == true) {
            $showDelete = true;
        }
        $smarty->assign("showDelete", $showDelete);

        $smarty->assign("userId", $email->assigned_user_id);
        $smarty->assign("userName", $email->assigned_user_name);
        $parent_types = $app_list_strings['record_type_display'];
        $smarty->assign('parentOptions', get_select_options_with_id($parent_types, $email->parent_type));

        $quicksearch_js = '<script type="text/javascript" language="javascript">sqs_objects = ' . json_encode($sqs_objects) . '</script>';
        $smarty->assign('SQS', $quicksearch_js);

        $meta = array();
        $meta['html'] = $smarty->fetch("modules/Emails/templates/importRelate.tpl");

        return $meta;
    }

    /**
     * This function returns the detail view for email in new 2.0 interface
     *
     */
    public function getDetailViewForEmail2($emailId)
    {
        global $app_strings, $app_list_strings, $mod_strings;

        $detailView = null;
        $meta = [];

        require_once('include/DetailView/DetailView.php');

        $smarty = new Sugar_Smarty();

        // SETTING DEFAULTS
        $focus = BeanFactory::newBean('Emails');
        $focus->retrieve($emailId);
        $detailView->ss = new Sugar_Smarty();
        $detailView = new DetailView();
        $title = "";
        $offset = 0;
        if ($focus->type == 'out') {
            $title = getClassicModuleTitle('Emails', array($mod_strings['LBL_SENT_MODULE_NAME'], $focus->name), true);
        } elseif ($focus->type == 'draft') {
            $title = getClassicModuleTitle(
                'Emails',
                array($mod_strings['LBL_LIST_FORM_DRAFTS_TITLE'], $focus->name),
                true
            );
        } elseif ($focus->type == 'inbound') {
            $title = getClassicModuleTitle('Emails', array($mod_strings['LBL_INBOUND_TITLE'], $focus->name), true);
        }
        $smarty->assign("emailTitle", $title);

        // DEFAULT TO TEXT IF NO HTML CONTENT:
        $html = trim(from_html($focus->description_html));
        if (empty($html)) {
            $smarty->assign('SHOW_PLAINTEXT', 'true');
        } else {
            $smarty->assign('SHOW_PLAINTEXT', 'false');
        }

        //if not empty or set to test (from test campaigns)
        if (!empty($focus->parent_type) && $focus->parent_type != 'test') {
            $smarty->assign('PARENT_MODULE', $focus->parent_type);
            $smarty->assign('PARENT_TYPE', $app_list_strings['record_type_display'][$focus->parent_type] . ":");
        }

        global $gridline;
        $smarty->assign('MOD', $mod_strings);
        $smarty->assign('APP', $app_strings);
        $smarty->assign('GRIDLINE', $gridline);
        $smarty->assign('PRINT_URL', 'index.php?' . $GLOBALS['request_string']);
        $smarty->assign('ID', $focus->id);
        $smarty->assign('TYPE', $focus->type);
        $smarty->assign('PARENT_NAME', $focus->parent_name);
        $smarty->assign('PARENT_ID', $focus->parent_id);
        $smarty->assign('NAME', $focus->name);
        $smarty->assign('ASSIGNED_TO', $focus->assigned_user_name);
        $smarty->assign('DATE_MODIFIED', $focus->date_modified);
        $smarty->assign('DATE_ENTERED', $focus->date_entered);
        $smarty->assign('DATE_START', $focus->date_start);
        $smarty->assign('TIME_START', $focus->time_start);
        $smarty->assign('FROM', $focus->from_addr);
        isValidEmailAddress($focus->from_addr);
        $smarty->assign('TO', nl2br($focus->to_addrs));
        $smarty->assign('CC', nl2br($focus->cc_addrs));
        $smarty->assign('BCC', nl2br($focus->bcc_addrs));
        $smarty->assign('CREATED_BY', $focus->created_by_name);
        $smarty->assign('MODIFIED_BY', $focus->modified_by_name);
        $smarty->assign('DATE_SENT_RECEIVED', $focus->date_sent_received);
        $smarty->assign('EMAIL_NAME', 'RE: ' . $focus->name);
        $smarty->assign("TAG", $focus->listviewACLHelper());
        $smarty->assign("SUGAR_VERSION", $GLOBALS['sugar_version']);
        $smarty->assign("JS_CUSTOM_VERSION", $GLOBALS['sugar_config']['js_custom_version']);
        if (!empty($focus->reply_to_email)) {
            $replyTo = "
                <tr>
                <td class=\"tabDetailViewDL\"><slot>" . $mod_strings['LBL_REPLY_TO_NAME'] . "</slot></td>
                <td colspan=3 class=\"tabDetailViewDF\"><slot>" . $focus->reply_to_addr . "</slot></td>
                </tr>";
            $smarty->assign("REPLY_TO", $replyTo);
        }
        ///////////////////////////////////////////////////////////////////////////////
        ////	JAVASCRIPT VARS
        $jsVars = '';
        $jsVars .= "var showRaw = '{$mod_strings['LBL_BUTTON_RAW_LABEL']}';";
        $jsVars .= "var hideRaw = '{$mod_strings['LBL_BUTTON_RAW_LABEL_HIDE']}';";
        $smarty->assign("JS_VARS", $jsVars);
        ///////////////////////////////////////////////////////////////////////////////
        ////	NOTES (attachements, etc.)
        ///////////////////////////////////////////////////////////////////////////////

        $note = BeanFactory::newBean('Notes');
        $where = "notes.parent_id='{$focus->id}'";
        //take in account if this is from campaign and the template id is stored in the macros.

        if (isset($macro_values) && isset($macro_values['email_template_id'])) {
            $where = "notes.parent_id='{$macro_values['email_template_id']}'";
        }
        $notes_list = $note->get_full_list("notes.name", $where, true);

        if (!isset($notes_list)) {
            $notes_list = array();
        }

        $attachments = '';
        $notes_listCount = count($notes_list);
        for ($i = 0; $i < $notes_listCount; $i++) {
            $the_note = $notes_list[$i];
            $attachments .= "<a href=\"index.php?entryPoint=download&id={$the_note->id}&type=Notes\">" . $the_note->name . "</a><br />";
            $focus->cid2Link($the_note->id, $the_note->file_mime_type);
        }
        $smarty->assign('DESCRIPTION', nl2br($focus->description));
        $smarty->assign('DESCRIPTION_HTML', from_html($focus->description_html));
        $smarty->assign("ATTACHMENTS", $attachments);
        ///////////////////////////////////////////////////////////////////////////////
        ////    SUBPANELS
        ///////////////////////////////////////////////////////////////////////////////
        $show_subpanels = true;
        if ($show_subpanels) {
            require_once('include/SubPanel/SubPanelTiles.php');
            $subpanel = new SubPanelTiles($focus, 'Emails');
            $smarty->assign("SUBPANEL", $subpanel->display());
        }
        $meta['html'] = $smarty->fetch("modules/Emails/templates/emailDetailView.tpl");

        return $meta;
    } // fn

    /**
     * Sets the "read" flag in the overview cache
     */
    public function setReadFlag($ieId, $mbox, $uid)
    {
        $this->markEmails('read', $ieId, $mbox, $uid);
    }

    /**
     * Marks emails with the passed flag type.  This will be applied to local
     * cache files as well as remote emails.
     * @param string $type Flag type
     * @param string $ieId
     * @param string $folder IMAP folder structure or SugarFolder GUID
     * @param string $uids Comma sep list of UIDs or GUIDs
     */
    public function markEmails($type, $ieId, $folder, $uids)
    {
        global $app_strings;
        $uids = $this->_cleanUIDList($uids);
        $exUids = explode($app_strings['LBL_EMAIL_DELIMITER'], $uids);

        if (strpos($folder, 'sugar::') !== false) {
            // dealing with a sugar email object, uids are GUIDs
            foreach ($exUids as $id) {
                $email = BeanFactory::newBean('Emails');
                $email->retrieve($id);

                // BUG FIX BEGIN
                // Bug 50973 - marking unread in group inbox removes message
                if (empty($email->assigned_user_id)) {
                    $email->setFieldNullable('assigned_user_id');
                }
                // BUG FIX END

                switch ($type) {
                    case "unread":
                        $email->status = 'unread';
                        $email->save();
                        break;

                    case "read":
                        $email->status = 'read';
                        $email->save();
                        break;

                    case "deleted":
                        $email->delete();
                        break;

                    case "flagged":
                        $email->flagged = 1;
                        $email->save();
                        break;

                    case "unflagged":
                        $email->flagged = 0;
                        $email->save();
                        break;

                }

                // BUG FIX BEGIN
                // Bug 50973 - reset assigned_user_id field defs
                if (empty($email->assigned_user_id)) {
                    $email->revertFieldNullable('assigned_user_id');
                }
                // BUG FIX END
            }
        } else {
            /* dealing with IMAP email, uids are IMAP uids */
            global $ie; // provided by EmailUIAjax.php
            if (empty($ie)) {
                $ie = BeanFactory::newBean('InboundEmail');
            }
            $ie->retrieve($ieId);
            $ie->mailbox = $folder;
            $ie->connectMailserver();
            // mark cache files
            if ($type == 'deleted') {
                $ie->deleteMessageOnMailServer($uids);
                $ie->deleteMessageFromCache($uids);
            } else {
                $overviews = $ie->getCacheValueForUIDs($ie->mailbox, $exUids);
                $manipulated = array();

                foreach ($overviews['retArr'] as $k => $overview) {
                    if (in_array($overview->uid, $exUids)) {
                        switch ($type) {
                            case "unread":
                                $overview->seen = 0;
                                break;

                            case "read":
                                $overview->seen = 1;
                                break;

                            case "flagged":
                                $overview->flagged = 1;
                                break;

                            case "unflagged":
                                $overview->flagged = 0;
                                break;
                        }
                        $manipulated[] = $overview;
                    }
                }

                if (!empty($manipulated)) {
                    $ie->setCacheValue($ie->mailbox, array(), $manipulated);
                    /* now mark emails on email server */
                    $ie->markEmails(implode(",", explode($app_strings['LBL_EMAIL_DELIMITER'], $uids)), $type);
                }
            } // end not type == deleted
        }
    }

    public function doAssignment($distributeMethod, $ieid, $folder, $uids, $users)
    {
        global $app_strings;
        $users = explode(",", $users);
        $emailIds = explode($app_strings['LBL_EMAIL_DELIMITER'], $uids);
        $out = "";
        if ($folder != 'sugar::Emails') {
            $emailIds = array();
            $uids = explode($app_strings['LBL_EMAIL_DELIMITER'], $uids);
            $ie = BeanFactory::newBean('InboundEmail');
            $ie->retrieve($ieid);
            $messageIndex = 1;
            // dealing with an inbound email data so we need to import an email and then
            foreach ($uids as $uid) {
                $ie->mailbox = $folder;
                $ie->connectMailserver();
                $msgNo = $uid;
                if (!$ie->isPop3Protocol()) {
                    $msgNo = $ie->getImap()->getMessageNo($uid);
                } else {
                    $msgNo = $ie->getCorrectMessageNoForPop3($uid);
                }

                if (!empty($msgNo)) {
                    if ($ie->importOneEmail($msgNo, $uid)) {
                        $emailIds[] = $ie->email->id;
                        $ie->deleteMessageOnMailServer($uid);
                        //$ie->retrieve($ieid);
                        //$ie->connectMailserver();
                        $ie->mailbox = $folder;
                        $ie->deleteMessageFromCache(($uids[] = $uid));
                    } else {
                        $out = $out . "Message No : " . $messageIndex . " failed. Reason : Message already imported \r\n";
                    }
                }
                $messageIndex++;
            } // for
        } // if

        if ((is_countable($emailIds) ? count($emailIds) : 0) > 0) {
            $this->doDistributionWithMethod($users, $emailIds, $distributeMethod);
        } // if

        return $out;
    } // fn

    /**
     * get team id and team set id from request
     * @return  array
     */
    public function getTeams()
    {
    }

    public function doDistributionWithMethod($users, $emailIds, $distributionMethod)
    {
        // we have users and the items to distribute
        if ($distributionMethod == 'roundRobin') {
            $this->distRoundRobin($users, $emailIds);
        } elseif ($distributionMethod == 'leastBusy') {
            $this->distLeastBusy($users, $emailIds);
        } elseif ($distributionMethod == 'direct') {
            if ((is_countable($users) ? count($users) : 0) > 1) {
                // only 1 user allowed in direct assignment
                $error = 1;
            } else {
                $user = $users[0];
                $this->distDirect($user, $emailIds);
            } // else
        } // elseif
    } // fn

    /**
     * distributes emails to users on Round Robin basis
     * @param    $userIds    array of users to dist to
     * @param    $mailIds    array of email ids to push on those users
     * @return  boolean        true on success
     */
    public function distRoundRobin($userIds, $mailIds)
    {
        // check if we have a 'lastRobin'
        $lastRobin = $userIds[0];
        foreach ($mailIds as $k => $mailId) {
            $userIdsKeys = array_flip($userIds); // now keys are values
            $thisRobinKey = $userIdsKeys[$lastRobin] + 1;
            if (!empty($userIds[$thisRobinKey])) {
                $thisRobin = $userIds[$thisRobinKey];
                $lastRobin = $userIds[$thisRobinKey];
            } else {
                $thisRobin = $userIds[0];
                $lastRobin = $userIds[0];
            }

            $email = BeanFactory::newBean('Emails');
            $email->retrieve($mailId);
            $email->assigned_user_id = $thisRobin;
            $email->status = 'unread';
            $email->save();
        }

        return true;
    }

    /**
     * distributes emails to users on Least Busy basis
     * @param    $userIds    array of users to dist to
     * @param    $mailIds    array of email ids to push on those users
     * @return  boolean        true on success
     */
    public function distLeastBusy($userIds, $mailIds)
    {
        foreach ($mailIds as $k => $mailId) {
            $email = BeanFactory::newBean('Emails');
            $email->retrieve($mailId);
            foreach ($userIds as $k => $id) {
                $r = $this->db->query("SELECT count(*) AS c FROM emails WHERE assigned_user_id = '.$id.' AND status = 'unread'");
                $a = $this->db->fetchByAssoc($r);
                $counts[$id] = $a['c'];
            }
            asort($counts); // lowest to highest
            $countsKeys = array_flip($counts); // keys now the 'count of items'
            $leastBusy = array_shift($countsKeys); // user id of lowest item count
            $email->assigned_user_id = $leastBusy;
            $email->status = 'unread';
            $email->save();
        }

        return true;
    }

    /**
     * distributes emails to 1 user
     * @param    $user        users to dist to
     * @param    $mailIds    array of email ids to push
     * @return  boolean        true on success
     */
    public function distDirect($user, $mailIds)
    {
        foreach ($mailIds as $k => $mailId) {
            $email = BeanFactory::newBean('Emails');
            $email->retrieve($mailId);
            $email->assigned_user_id = $user;
            $email->status = 'unread';

            $email->save();
        }

        return true;
    }

    /**
     * @param array $userIds
     * @return array
     */
    public function getAssignedEmailsCountForUsers($userIds)
    {
        $counts = [];
        foreach ($userIds as $id) {
            $idQuoted = $this->db->quoted($id);
            $r = $this->db->query("SELECT count(*) AS c FROM emails WHERE assigned_user_id = $idQuoted AND status = 'unread'");
            $a = $this->db->fetchByAssoc($r);
            $counts[$id] = $a['c'];
        }

        return $counts;
    }

    public function getLastRobin($ie)
    {
        $lastRobin = "";
        if ($this->validCacheFileExists($ie->id, 'folders', "robin.cache.php")) {
            $lastRobin = $this->getCacheValue($ie->id, 'folders', "robin.cache.php", 'robin');
        } // if

        return $lastRobin;
    } // fn

    public function setLastRobin($ie, $lastRobin)
    {
        global $sugar_config;
        $cacheFolderPath = sugar_cached("modules/Emails/{$ie->id}/folders");
        if (!file_exists($cacheFolderPath)) {
            mkdir_recursive($cacheFolderPath);
        }
        $this->writeCacheFile('robin', $lastRobin, $ie->id, 'folders', "robin.cache.php");
    } // fn

    /**
     * returns the metadata defining a single email message for display.  Uses cache file if it exists
     * @return array
     */
    public function getSingleMessage($ie)
    {
        global $timedate;
        global $app_strings, $mod_strings;

        $ie->retrieve($_REQUEST['ieId']);
        $noCache = true;
        $out = [];

        $ie->mailbox = $_REQUEST['mbox'];
        $filename = $_REQUEST['mbox'] . $_REQUEST['uid'] . ".php";
        $md5uidl = "";
        if ($ie->isPop3Protocol()) {
            $md5uidl = md5($_REQUEST['uid']);
            $filename = $_REQUEST['mbox'] . $md5uidl . ".php";
        } // if

        if (isset($filename) && strpos($filename, "..") !== false) {
            die("Directory navigation attack denied.");
        }

        if ($this->validCacheFileExists($_REQUEST['ieId'], 'messages', $filename)) {
            $out = $this->getCacheValue($_REQUEST['ieId'], 'messages', $filename, 'out');
            $noCache = false;

            // something fubar'd the cache?
            if (empty($out['meta']['email']['name']) && empty($out['meta']['email']['description'])) {
                $noCache = true;
            } else {
                // When sending data from cache, convert date into users preffered format
                $dateTimeInGMTFormat = $out['meta']['email']['date_start'];
                $out['meta']['email']['date_start'] = $timedate->to_display_date_time($dateTimeInGMTFormat);
            } // else
        }

        if ($noCache) {
            $writeToCacheFile = true;
            if ($ie->isPop3Protocol()) {
                $status = $ie->setEmailForDisplay($_REQUEST['uid'], true, true, true);
            } else {
                $status = $ie->setEmailForDisplay($_REQUEST['uid'], false, true, true);
            }
            $out = $ie->displayOneEmail($_REQUEST['uid'], $_REQUEST['mbox']);
            // modify the out object to store date in GMT format on the local cache file
            $dateTimeInUserFormat = $out['meta']['email']['date_start'];
            $out['meta']['email']['date_start'] = $timedate->to_db($dateTimeInUserFormat);
            if ($status == 'error') {
                $writeToCacheFile = false;
            }
            if ($writeToCacheFile) {
                if ($ie->isPop3Protocol()) {
                    $this->writeCacheFile(
                        'out',
                        $out,
                        $_REQUEST['ieId'],
                        'messages',
                        "{$_REQUEST['mbox']}{$md5uidl}.php"
                    );
                } else {
                    $this->writeCacheFile(
                        'out',
                        $out,
                        $_REQUEST['ieId'],
                        'messages',
                        "{$_REQUEST['mbox']}{$_REQUEST['uid']}.php"
                    );
                } // else
                // restore date in the users preferred format to be send on to UI for diaply
                $out['meta']['email']['date_start'] = $dateTimeInUserFormat;
            } // if
        }
        $out['meta']['email']['toaddrs'] = $this->generateExpandableAddrs($out['meta']['email']['toaddrs']);
        if (!empty($out['meta']['email']['cc_addrs'])) {
            $ccs = $this->generateExpandableAddrs($out['meta']['email']['cc_addrs']);
            $out['meta']['cc'] = <<<eoq
                <tr>
                    <td NOWRAP valign="top" class="displayEmailLabel">
                        {$app_strings['LBL_EMAIL_CC']}:
                    </td>
                    <td class="displayEmailValue">
                        {$ccs}
                    </td>
                </tr>
eoq;
        }

        if (empty($out['meta']['email']['description'])) {
            $out['meta']['email']['description'] = $mod_strings['LBL_EMPTY_EMAIL_BODY'];
        }

        if ($noCache) {
            $GLOBALS['log']->debug("EMAILUI: getSingleMessage() NOT using cache file");
        } else {
            $GLOBALS['log']->debug("EMAILUI: getSingleMessage() using cache file [ " . $_REQUEST['mbox'] . $_REQUEST['uid'] . ".php ]");
        }

        $this->setReadFlag($_REQUEST['ieId'], $_REQUEST['mbox'], $_REQUEST['uid']);

        return $out;
    }


    /**
     * Returns the HTML for a list of emails in a given folder
     * @param GUID $ieId GUID to InboundEmail instance
     * @param string $mbox Mailbox path name in dot notation
     * @param int $folderListCacheOffset Seconds for valid cache file
     * @return string HTML render of list.
     */
    public function getListEmails($ieId, $mbox, $folderListCacheOffset, $forceRefresh = 'false')
    {
        global $sugar_config;

        $ie = BeanFactory::newBean('InboundEmail');
        $ie->retrieve($ieId);
        $list = $ie->displayFolderContents($mbox, $forceRefresh);

        return $list;
    }

    /**
     * Returns the templatized compose screen.  Used by reply, forwards and draft status messages.
     * @param object email Email bean in focus
     */
    public function displayComposeEmail($email)
    {

        global $locale;
        global $current_user;


        $ea = new SugarEmailAddress();

        if (!empty($email)) {
            $email->cids2Links();
            $description = (empty($email->description_html)) ? $email->description : $email->description_html;
        }

        $toAddresses = '';
        $ccAddresses = '';

        //Get the most complete address list availible for this email
        $addresses = array('toAddresses' => 'to', 'ccAddresses' => 'cc', 'bccAddresses' => 'bcc');
        foreach ($addresses as $var => $type) {
            ${$var} = "";
            foreach (array("{$type}_addrs_names", "{$type}addrs", "{$type}_addrs") as $emailVar) {
                if (!empty($email->$emailVar)) {
                    ${$var} = $email->$emailVar;
                    break;
                }
            }
        }

        $ret = array();
        $ret['type'] = $email->type;
        $ret['name'] = $email->name;
        $ret['description'] = $description;
        $ret['from'] = (isset($_REQUEST['composeType']) && $_REQUEST['composeType'] == 'forward') ? "" : $email->from_addr;
        isValidEmailAddress($ret['from']);
        $ret['to'] = from_html($toAddresses);
        $ret['uid'] = $email->id;
        $ret['parent_name'] = $email->parent_name;
        $ret['parent_type'] = $email->parent_type;
        $ret['parent_id'] = $email->parent_id;

        if ($email->type == 'draft') {
            $ret['cc'] = from_html($ccAddresses);
            $ret['bcc'] = $bccAddresses;
        }
        // reply all
        if (isset($_REQUEST['composeType']) && $_REQUEST['composeType'] == 'replyAll') {
            $ret['cc'] = from_html($ccAddresses);
            $ret['bcc'] = $bccAddresses;

            $userEmails = array();
            $userEmailsMeta = $ea->getAddressesByGUID($current_user->id, 'Users');
            foreach ($userEmailsMeta as $emailMeta) {
                $userEmails[] = from_html(strtolower(trim($emailMeta['email_address'])));
            }
            $fromAddr = from_html(strtolower(trim($email->from_addr)));
            isValidEmailAddress($fromAddr);
            $userEmails[] = $fromAddr;

            $ret['cc'] = from_html($email->cc_addrs);
            $toAddresses = from_html($toAddresses);
            $to = str_replace($this->addressSeparators, "::", (string) $toAddresses);
            $exTo = explode("::", $to);

            if (is_array($exTo)) {
                foreach ($exTo as $addr) {
                    $addr = strtolower(trim($addr));
                    if (!in_array($addr, $userEmails)) {
                        if (!empty($ret['cc'])) {
                            $ret['cc'] = $ret['cc'] . ", ";
                        }
                        $ret['cc'] = $ret['cc'] . trim($addr);
                    }
                }
            } elseif (!empty($exTo)) {
                $exTo = trim($exTo);
                if (!in_array($exTo, $userEmails)) {
                    $ret['cc'] = $ret['cc'] . ", " . $exTo;
                }
            }
        }

        return $ret;
    }

    /**
     * Formats email body on reply/forward
     * @param object email Email object in focus
     * @param string type
     * @return object email
     */
    public function handleReplyType($email, $type)
    {
        global $mod_strings;

        $GLOBALS['log']->debug("****At Handle Reply Type: $type");

        $header = '';
        switch ($type) {
            case "reply":
            case "replyAll":
                $header = $email->getReplyHeader();
                if (!preg_match('/^(re:)+/i', (string) $email->name)) {
                    $email->name = "{$mod_strings['LBL_RE']} {$email->name}";
                }
                if ($type == "reply") {
                    $email->cc_addrs = "";
                    if (!empty($email->reply_to_addr)) {
                        $email->from_addr = $email->reply_to_addr;
                        isValidEmailAddress($email->from_addr);
                    } // if
                } else {
                    if (!empty($email->reply_to_addr)) {
                        $email->to_addrs = $email->to_addrs . "," . $email->reply_to_addr;
                    } // if
                } // else
                break;

            case "forward":
                $header = $email->getForwardHeader();
                if (!preg_match('/^(fw:)+/i', (string) $email->name)) {
                    $email->name = "{$mod_strings['LBL_FW']} {$email->name}";
                }
                $email->cc_addrs = "";
                break;

            case "replyCase":
                $GLOBALS['log']->debug("EMAILUI: At reply case");
                $header = $email->getReplyHeader();

                $myCase = BeanFactory::newBean('Cases');
                $myCase->retrieve($email->parent_id);
                $myCaseMacro = $myCase->getEmailSubjectMacro();
                $email->parent_name = $myCase->name;
                $GLOBALS['log']->debug("****Case # : {$myCase->case_number} macro: $myCaseMacro");
                if (!strpos((string) $email->name, str_replace('%1', $myCase->case_number, (string) $myCaseMacro))) {
                    $GLOBALS['log']->debug("Replacing");
                    $email->name = str_replace('%1', $myCase->case_number, (string) $myCaseMacro) . ' ' . $email->name;
                }
                $email->name = "{$mod_strings['LBL_RE']} {$email->name}";
                break;
        }

        $html = trim($email->description_html);
        $plain = trim($email->description);

        $desc = (!empty($html)) ? $html : $plain;

        $email->description = $header . $email->quoteHtmlEmailForNewEmailUI($desc);

        return $email;
    }

    ///////////////////////////////////////////////////////////////////////////
    ////	PRIVATE HELPERS
    /**
     * Generates a UNION query to get one list of users, contacts, leads, and
     * prospects; used specifically for the addressBook
     */
    public function _getPeopleUnionQuery($whereArr, $person)
    {
        global $current_user, $app_strings;
        $db = DBManagerFactory::getInstance();
        if (!isset($person) || $person === 'LBL_DROPDOWN_LIST_ALL') {
            $peopleTables = array(
                "users",
                "contacts",
                "leads",
                "prospects",
                "accounts"
            );
        } else {
            $peopleTables = array($person);
        }
        $q = '';

        $whereAdd = "";

        foreach ($whereArr as $column => $clause) {
            if (!empty($whereAdd)) {
                $whereAdd .= " AND ";
            }
            $clause = $current_user->db->quote($clause);
            $whereAdd .= "{$column} LIKE '{$clause}%'";
        }

        foreach ($peopleTables as $tableName) {
            $module = ucfirst($tableName);
            $personBean = BeanFactory::getBean($module);

            if ($personBean !== false || !$personBean->ACLAccess('list')) {
                continue;
            } // if
            $table = $personBean->getTableName();
            $where = "({$table}.deleted = 0 AND eabr.primary_address = 1 AND {$table}.id <> '{$current_user->id}')";

            $accessWhere = $personBean->buildAccessWhere('list');
            if (!empty($accessWhere)) {
                $where .= ' AND '. $accessWhere;
            }

            if (!empty($whereAdd)) {
                $where .= " AND ({$whereAdd})";
            }

            if ($personBean instanceof Company) {
                $t = "SELECT {$table}.id, '' first_name, {$table}.name, eabr.primary_address, ea.email_address, '{$module}' module ";
            } else {
                $t = "SELECT {$table}.id, {$table}.first_name, {$table}.last_name, eabr.primary_address, ea.email_address, '{$module}' module ";
            }
            $t .= "FROM {$table} ";
            $t .= "JOIN email_addr_bean_rel eabr ON ({$table}.id = eabr.bean_id and eabr.deleted=0) ";
            $t .= "JOIN email_addresses ea ON (eabr.email_address_id = ea.id) ";
            $t .= " WHERE {$where}";

            if (!empty($q)) {
                $q .= "\n UNION ALL \n";
            }

            $q .= "({$t})";
        }
        $countq = "SELECT count(people.id) c from ($q) people";
        $q .= "ORDER BY last_name";

        return array('query' => $q, 'countQuery' => $countq);
    }

    /**
     * get emails of related bean for a given bean id
     * @param $beanType
     * @param $condition array of conditions inclued bean id
     * @return array('query' => $q, 'countQuery' => $countq);
     */
    public function getRelatedEmail($beanType, $whereArr, $relatedBeanInfoArr = '')
    {
        global $beanList, $current_user, $app_strings;
        $finalQuery = '';
        $searchBeans = null;
        if ($beanType === 'LBL_DROPDOWN_LIST_ALL') {
            $searchBeans = array(
                "users",
                "contacts",
                "leads",
                "prospects",
                "accounts"
            );
        }

        if ($relatedBeanInfoArr == '' || empty($relatedBeanInfoArr['related_bean_type'])) {
            if ($searchBeans != null) {
                $q = array();
                foreach ($searchBeans as $searchBean) {
                    $searchq = $this->findEmailFromBeanIds('', $searchBean, $whereArr);
                    if (!empty($searchq)) {
                        $q[] = "($searchq)";
                    }
                }
                if (!empty($q)) {
                    $finalQuery .= implode("\n UNION ALL \n", $q);
                }
            } else {
                $finalQuery = $this->findEmailFromBeanIds('', $beanType, $whereArr);
            }
        } else {
            $class = $beanList[$relatedBeanInfoArr['related_bean_type']];
            $focus = new $class();
            $focus->retrieve($relatedBeanInfoArr['related_bean_id']);
            if ($searchBeans != null) {
                $q = array();
                foreach ($searchBeans as $searchBean) {
                    if ($focus->load_relationship($searchBean)) {
                        $data = $focus->$searchBean->get();
                        if ((is_countable($data) ? count($data) : 0) != 0) {
                            $q[] = '(' . $this->findEmailFromBeanIds($data, $searchBean, $whereArr) . ')';
                        }
                    }
                }
                if (!empty($q)) {
                    $finalQuery .= implode("\n UNION ALL \n", $q);
                }
            } else {
                if ($focus->load_relationship($beanType)) {
                    $data = $focus->$beanType->get();
                    if ((is_countable($data) ? count($data) : 0) != 0) {
                        $finalQuery = $this->findEmailFromBeanIds($data, $beanType, $whereArr);
                    }
                }
            }
        }
        $countq = "SELECT count(people.id) c from ($finalQuery) people";

        return array('query' => $finalQuery, 'countQuery' => $countq);
    }

    public function findEmailFromBeanIds($beanIds, $beanType, $whereArr)
    {
        global $current_user;
        $t = '';
        $q = '';
        $whereAdd = "";
        $relatedIDs = '';
        if ($beanIds != '') {
            foreach ($beanIds as $key => $value) {
                $beanIds[$key] = '\'' . $value . '\'';
            }
            $relatedIDs = implode(',', $beanIds);
        }

        $module = ucfirst($beanType);
        $personBean = BeanFactory::getBean($module);
        if ($personBean !== false  && $personBean->ACLAccess('list')) {

            if ($personBean instanceof Company) {
                if (isset($whereArr['first_name'])) {
                    $whereArr['name'] = $whereArr['first_name'];
                }
                unset($whereArr['last_name']);
                unset($whereArr['first_name']);
            }

            foreach ($whereArr as $column => $clause) {
                if (!empty($whereAdd)) {
                    $whereAdd .= " OR ";
                }
                $clause = $current_user->db->quote($clause);
                $whereAdd .= "{$column} LIKE '{$clause}%'";
            }


            $table = $personBean->getTableName();
            if ($relatedIDs !== '') {
                $where = "({$table}.deleted = 0 AND eabr.primary_address = 1 AND {$table}.id in ($relatedIDs))";
            } else {
                $where = "({$table}.deleted = 0 AND eabr.primary_address = 1)";
            }

            $accessWhere = $personBean->buildAccessWhere('list');
            if (!empty($accessWhere)) {
                $where .= ' AND '. $accessWhere;
            }

            if (!empty($whereAdd)) {
                $where .= " AND ({$whereAdd})";
            }

            if ($personBean instanceof Company) {
                $t = "SELECT {$table}.id, '' first_name, {$table}.name last_name, eabr.primary_address, ea.email_address, '{$module}' module ";
            } else {
                $t = "SELECT {$table}.id, {$table}.first_name, {$table}.last_name, eabr.primary_address, ea.email_address, '{$module}' module ";
            }

            $t .= "FROM {$table} ";
            $t .= "JOIN email_addr_bean_rel eabr ON ({$table}.id = eabr.bean_id and eabr.deleted=0) ";
            $t .= "JOIN email_addresses ea ON (eabr.email_address_id = ea.id) ";
            $t .= " WHERE {$where}";
        } // if

        return $t;
    }

    /**
     * Cleans UID lists
     * @param mixed $uids
     * @param bool $returnString False will return an array
     * @return mixed
     */
    public function _cleanUIDList($uids, $returnString = false)
    {
        global $app_strings;
        $GLOBALS['log']->debug("_cleanUIDList: before - [ {$uids} ]");

        if (!is_array($uids)) {
            $returnString = true;

            $exUids = explode($app_strings['LBL_EMAIL_DELIMITER'], $uids);
            $uids = $exUids;
        }

        $cleanUids = array();
        foreach ($uids as $uid) {
            $cleanUids[$uid] = $uid;
        }

        sort($cleanUids);

        if ($returnString) {
            $cleanImplode = implode($app_strings['LBL_EMAIL_DELIMITER'], $cleanUids);
            $GLOBALS['log']->debug("_cleanUIDList: after - [ {$cleanImplode} ]");

            return $cleanImplode;
        }

        return $cleanUids;
    }

    /**
     * Creates Mail folder
     *
     * @param object $user User in focus
     * @param array $folder_params Array of parameters for folder creation
     */
    protected function createFolder($user, $folder_params)
    {
        $folder = new SugarFolder();
        foreach ($folder_params as $key => $val) {
            $folder->$key = $val;
        }

        $folder->save();

        return $folder;
    }

    /**
     * Creates defaults for the User
     * @param object $user User in focus
     */
    public function preflightUser(&$user)
    {
        global $mod_strings;
        $folder_types = array();

        $params = array(
            // My Emails
            "inbound" => array(
                'name' => $mod_strings['LNK_MY_INBOX'],
                'folder_type' => "inbound",
                'has_child' => 1,
                'dynamic_query' => $this->generateDynamicFolderQuery("inbound", $user->id),
                'is_dynamic' => 1,
                'created_by' => $user->id,
                'modified_by' => $user->id,
            ),
            // My Drafts
            "draft" => array(
                'name' => $mod_strings['LNK_MY_DRAFTS'],
                'folder_type' => "draft",
                'has_child' => 0,
                'dynamic_query' => $this->generateDynamicFolderQuery("draft", $user->id),
                'is_dynamic' => 1,
                'created_by' => $user->id,
                'modified_by' => $user->id,
            ),
            // Sent Emails
            "sent" => array(
                'name' => $mod_strings['LNK_SENT_EMAIL_LIST'],
                'folder_type' => "sent",
                'has_child' => 0,
                'dynamic_query' => $this->generateDynamicFolderQuery("sent", $user->id),
                'is_dynamic' => 1,
                'created_by' => $user->id,
                'modified_by' => $user->id,
            ),
            // Archived Emails
            "archived" => array(
                'name' => $mod_strings['LBL_LIST_TITLE_MY_ARCHIVES'],
                'folder_type' => "archived",
                'has_child' => 0,
                'dynamic_query' => '',
                'is_dynamic' => 1,
                'created_by' => $user->id,
                'modified_by' => $user->id,
            ),
        );

        $q = "SELECT * FROM folders f WHERE f.created_by = '{$user->id}' AND f.deleted = 0 AND coalesce(" . $user->db->convert(
            "f.folder_type",
            "length"
        ) . ",0) > 0";
        $r = $user->db->query($q);

        while ($row = DBManagerFactory::getInstance()->fetchByAssoc($r)) {
            if ($row['folder_type'] == 'inbound') {
                $parent_id = $row['id'];
            }
            if (!in_array($row['folder_type'], $folder_types)) {
                array_push($folder_types, $row['folder_type']);
            }
            if (isset($params[$row['folder_type']])) {
                unset($params[$row['folder_type']]);
            }
        }

        require_once("include/SugarFolders/SugarFolders.php");

        foreach ($params as $type => $type_params) {
            if ($type == "inbound") {
                $folder = $this->createFolder($user, $params[$type]);

                $parent_id = $folder->id;

                // handle the case where inbound folder was deleted, but other folders exist
                if (count($folder_types) != 0) {
                    // This update query will exclude inbound parent, and any custom created folders.
                    // For others, it will update their parent_id for the current user.
                    $q = "UPDATE folders SET parent_folder = '" . $parent_id .
                        "' WHERE folder_type IN ('draft', 'sent', 'archived') AND created_by = '" . $user->id . "'";
                    $q = "UPDATE folders SET parent_folder = '" . $parent_id .
                        "' WHERE folder_type IN ('draft', 'sent', 'archived') AND created_by = '" . $user->id . "'";
                    $r = $user->db->query($q);
                }
            } else {
                $params[$type]['parent_folder'] = $parent_id;

                $this->createFolder($user, $params[$type]);
            }
        }
    }

    /**
     * Parses the core dynamic folder query
     * @param string $type 'inbound', 'draft', etc.
     * @param string $userId
     * @return string
     */
    public function generateDynamicFolderQuery($type, $userId)
    {
        $q = $this->coreDynamicFolderQuery;

        $status = $type;

        if ($type == "sent") {
            $type = "out";
        }

        $replacee = array("::TYPE::", "::STATUS::", "::USER_ID::");
        $replacer = array($type, $status, $userId);

        $ret = str_replace($replacee, $replacer, (string) $q);

        if ($type == 'inbound') {
            $ret .= " AND status NOT IN ('sent', 'archived', 'draft') AND type NOT IN ('out', 'archived', 'draft')";
        } else {
            $ret .= " AND status NOT IN ('archived') AND type NOT IN ('archived')";
        }

        return $ret;
    }

    /**
     * Preps the User's cache dir
     */
    public function preflightUserCache()
    {
        $path = clean_path($this->userCacheDir);
        if (!file_exists($this->userCacheDir)) {
            mkdir_recursive($path);
        }

        $files = findAllFiles($path, array());

        foreach ($files as $file) {
            unlink($file);
        }
    }

    public function clearInboundAccountCache($ieId)
    {
        global $sugar_config;
        $cacheRoot = sugar_cached("modules/Emails/{$ieId}");
        $files = findAllFiles($cacheRoot . "/messages/", array());
        foreach ($files as $file) {
            unlink($file);
        } // fn
        $files = findAllFiles($cacheRoot . "/attachments/", array());
        foreach ($files as $file) {
            unlink($file);
        } // for
    } // fn

    /**
     * returns an array of EmailTemplates that the user has access to for the compose email screen
     * @return array
     */
    public function getEmailTemplatesArray()
    {
        global $app_strings;

        if (ACLController::checkAccess('EmailTemplates', 'list', true) && ACLController::checkAccess(
            'EmailTemplates',
            'view',
            true
        )
        ) {
            $et = BeanFactory::newBean('EmailTemplates');
            $etResult = $et->db->query($et->create_new_list_query(
                '',
                "(email_templates.type IS NULL OR email_templates.type='' OR email_templates.type='email')",
                array(),
                array(),
                ''
            ));
            $email_templates_arr = array('' => $app_strings['LBL_NONE']);
            while ($etA = $et->db->fetchByAssoc($etResult)) {
                $email_templates_arr[$etA['id']] = $etA['name'];
            }
        } else {
            $email_templates_arr = array('' => $app_strings['LBL_NONE']);
        }

        return $email_templates_arr;
    }

    public function getFromAccountsArray($ie)
    {
        global $current_user;
        global $app_strings;

        $ieAccountsFull = $ie->retrieveAllByGroupIdWithGroupAccounts($current_user->id);
        $ieAccountsFrom = array();

        $oe = new OutboundEmail();
        $system = $oe->getSystemMailerSettings();
        $ret = $current_user->getUsersNameAndEmail();
        $ret['name'] = from_html($ret['name']);
        $useMyAccountString = true;

        if (empty($ret['email'])) {
            $systemReturn = $current_user->getSystemDefaultNameAndEmail();
            $ret['email'] = $systemReturn['email'];
            $ret['name'] = from_html($systemReturn['name']);
            $useMyAccountString = false;
        } // if

        $myAccountString = '';
        if ($useMyAccountString) {
            $myAccountString = " - {$app_strings['LBL_MY_ACCOUNT']}";
        } // if

        //Check to make sure that the user has set the associated inbound email account -> outbound account is active.
        $showFolders = sugar_unserialize(base64_decode($current_user->getPreference('showFolders', 'Emails')));
        $sf = new SugarFolder();
        $groupSubs = $sf->getSubscriptions($current_user);

        foreach ($ieAccountsFull as $k => $v) {
            $personalSelected = (!empty($showFolders) && in_array($v->id, $showFolders));

            $allowOutboundGroupUsage = isTrue($v->get_stored_options('allow_outbound_group_usage', false) ?? false);
            $groupSelected = (in_array($v->groupfolder_id, $groupSubs) && $allowOutboundGroupUsage);
            $selected = ($personalSelected || $groupSelected);

            if (!$selected) {
                $GLOBALS['log']->debug("Inbound Email {$v->name}, not selected and will not be available for selection within compose UI.");
                continue;
            }

            $name = $v->get_stored_options('from_name');
            $addr = $v->get_stored_options('from_addr');
            isValidEmailAddress($addr);
            if ($name != null && $addr != null) {
                $name = from_html($name);
                if (!$v->is_personal) {
                    $ieAccountsFrom[] = array(
                        "value" => $v->id,
                        "text" => "{$name} ({$addr}) - {$app_strings['LBL_EMAIL_UPPER_CASE_GROUP']}"
                    );
                } else {
                    $ieAccountsFrom[] = array("value" => $v->id, "text" => "{$name} ({$addr})");
                } // else
            } // if
        } // foreach


        $userSystemOverride = $oe->getUsersMailerForSystemOverride($current_user->id);
        //Substitute in the users system override if its available.
        if ($userSystemOverride != null) {
            $system = $userSystemOverride;
        }

        if (!empty($system->mail_smtpserver)) {
            $admin = BeanFactory::newBean('Administration');
            $admin->retrieveSettings(); //retrieve all admin settings.
            $ieAccountsFrom[] = array(
                "value" => $system->id,
                "text" =>
                    "{$ret['name']} ({$ret['email']}){$myAccountString}"
            );
        }

        return $ieAccountsFrom;
    } // fn

    /**
     * This function will return all the accounts this user has access to based on the
     * match of the emailId passed in as a parameter
     * @deprecate 7.9
     * @param unknown_type $ie
     * @return unknown
     */
    public function getFromAllAccountsArray($ie, $ret)
    {
        global $current_user;
        global $app_strings;

        $ret['fromAccounts'] = array();
        if (!isset($ret['to']) && !empty($ret['from'])) {
            $ret['fromAccounts']['status'] = false;

            return $ret;
        }
        $ieAccountsFull = $ie->retrieveAllByGroupIdWithGroupAccounts($current_user->id);
        $foundInPersonalAccounts = false;
        $foundInGroupAccounts = false;
        $foundInSystemAccounts = false;

        //$toArray = array();
        if ($ret['type'] == "draft") {
            $toArray = explode(",", $ret['from']);
        } else {
            $toArray = $ie->email->email2ParseAddressesForAddressesOnly($ret['to']);
        } // else
        foreach ($ieAccountsFull as $k => $v) {
            $storedOptions = sugar_unserialize(base64_decode($v->stored_options));
            if (array_search_insensitive($storedOptions['from_addr'], $toArray)) {
                if ($v->is_personal) {
                    $foundInPersonalAccounts = true;
                    break;
                } else {
                    $foundInGroupAccounts = true;
                } // else
            } // if
        } // foreach

        $oe = new OutboundEmail();
        $system = $oe->getSystemMailerSettings();

        $return = $current_user->getUsersNameAndEmail();
        $return['name'] = from_html($return['name']);
        $useMyAccountString = true;

        if (empty($return['email'])) {
            $systemReturn = $current_user->getSystemDefaultNameAndEmail();
            $return['email'] = $systemReturn['email'];
            $return['name'] = from_html($systemReturn['name']);
            $useMyAccountString = false;
        } // if

        $myAccountString = '';
        if ($useMyAccountString) {
            $myAccountString = " - {$app_strings['LBL_MY_ACCOUNT']}";
        } // if

        if (!empty($system->id)) {
            $admin = BeanFactory::newBean('Administration');
            $admin->retrieveSettings(); //retrieve all admin settings.
            if (in_array(trim($return['email']), $toArray)) {
                $foundInSystemAccounts = true;
            } // if
        } // if

        if (!$foundInPersonalAccounts && !$foundInGroupAccounts && !$foundInSystemAccounts) {
            $ret['fromAccounts']['status'] = false;

            return $ret;
        } // if

        $ieAccountsFrom = array();
        foreach ($ieAccountsFull as $k => $v) {
            $storedOptions = sugar_unserialize(base64_decode($v->stored_options));
            $storedOptionsName = from_html($storedOptions['from_name']);

            $selected = false;
            if (array_search_insensitive($storedOptions['from_addr'], $toArray)) {
                //if ($ret['to'] == $storedOptions['from_addr']) {
                $selected = true;
            } // if
            if ($foundInPersonalAccounts) {
                if ($v->is_personal) {
                    $ieAccountsFrom[] = array(
                        "value" => $v->id,
                        "selected" => $selected,
                        "text" => "{$storedOptionsName} ({$storedOptions['from_addr']})"
                    );
                } // if
            } else {
                $ieAccountsFrom[] = array(
                    "value" => $v->id,
                    "selected" => $selected,
                    "text" => "{$storedOptionsName} ({$storedOptions['from_addr']}) - {$app_strings['LBL_EMAIL_UPPER_CASE_GROUP']}"
                );
            } // else
        } // foreach

        if (!empty($system->id)) {
            if (!$foundInPersonalAccounts && !$foundInGroupAccounts && $foundInSystemAccounts) {
                $ieAccountsFrom[] = array(
                    "value" => $system->id,
                    "selected" => true,
                    "text" =>
                        "{$return['name']} ({$return['email']}){$myAccountString}"
                );
            } else {
                $ieAccountsFrom[] = array(
                    "value" => $system->id,
                    "text" =>
                        "{$return['name']} ({$return['email']}){$myAccountString}"
                );
            } // else
        } // if

        $ret['fromAccounts']['status'] = ($foundInPersonalAccounts || $foundInGroupAccounts || $foundInSystemAccounts) ? true : false;
        $ret['fromAccounts']['data'] = $ieAccountsFrom;

        return $ret;
    } // fn


    /**
     * takes an array and creates XML
     * @param array Array to convert
     * @param string Name to wrap highest level items in array
     * @return string XML
     */
    public function arrayToXML($a, $paramName)
    {
        if (!is_array($a)) {
            return '';
        }

        $bad = array("<", ">", "'", '"', "&");
        $good = array("&lt;", "&gt;", "&#39;", "&quot;", "&amp;");

        $ret = "";
        $aCount = count($a);

        for ($i = 0; $i < $aCount; $i++) {
            $email = $a[$i];
            $ret .= "\n<{$paramName}>";

            foreach ($email as $k => $v) {
                $ret .= "\n\t<{$k}>" . str_replace($bad, $good, (string) $v) . "</{$k}>";
            }
            $ret .= "\n</{$paramName}>";
        }

        return $ret;
    }

    /**
     * Re-used option getter for Show Accounts multiselect pane
     */
    public function getShowAccountsOptions(&$ie)
    {
        global $current_user;
        global $app_strings;
        global $mod_strings;

        $ieAccountsFull = $ie->retrieveAllByGroupId($current_user->id);
        $ieAccountsShowOptionsMeta = array();
        $showFolders = sugar_unserialize(base64_decode($current_user->getPreference('showFolders', 'Emails')));
        $defaultIEAccount = $ie->getUsersDefaultOutboundServerId($current_user);

        foreach ($ieAccountsFull as $k => $v) {
            $default = $defaultIEAccount == $v->id;
            $has_groupfolder = !empty($v->groupfolder_id);
            $type = $v->is_personal ? $mod_strings['LBL_MAILBOX_TYPE_PERSONAL'] : $mod_strings['LBL_MAILBOX_TYPE_GROUP'];

            $personalSelected = (!empty($showFolders) && in_array($v->id, $showFolders, true));
            $allowOutboundGroupUsage = isTrue($v->get_stored_options('allow_outbound_group_usage', false) ?? false);
            $selected = $personalSelected || $allowOutboundGroupUsage  || is_admin($current_user);

            if (!$selected) {
                LoggerManager::getLogger()->debug("Inbound Email {$v->name}, not selected and will not be available for selection within compose UI.");
                continue;
            }

            $ieAccountsShowOptionsMeta[] = array(
                "id" => $v->id,
                "name" => $v->name,
                'is_active' => $selected,
                'server_url' => $v->server_url,
                'is_group' => !$v->is_personal,
                'group_id' => $v->group_id,
                'is_default' => $default,
                'has_groupfolder' => $has_groupfolder,
                'type' => $type
            );
        }

        //Retrieve the grou folders
        $f = new SugarFolder();
        $groupFolders = $f->getGroupFoldersForSettings($current_user);

        foreach ($groupFolders as $singleGroup) {
            //Retrieve the related IE accounts.
            $relatedIEAccounts = $ie->retrieveByGroupFolderId($singleGroup['id']);

            if ((is_countable($relatedIEAccounts) ? count($relatedIEAccounts) : 0) == 0) {
                $server_url = $app_strings['LBL_EMAIL_MULT_GROUP_FOLDER_ACCOUNTS_EMPTY'];
            } else {
                if ((is_countable($relatedIEAccounts) ? count($relatedIEAccounts) : 0) == 1) {
                    if ($relatedIEAccounts[0]->status != 'Active' || $relatedIEAccounts[0]->mailbox_type == 'bounce') {
                        continue;
                    }

                    $server_url = $relatedIEAccounts[0]->server_url;
                } else {
                    $server_url = $app_strings['LBL_EMAIL_MULT_GROUP_FOLDER_ACCOUNTS'];
                }
            }


            $type = $mod_strings['LBL_MAILBOX_TYPE_GROUP_FOLDER'];
            $ieAccountsShowOptionsMeta[] = array(
                "id" => $singleGroup['id'],
                "name" => $singleGroup['origName'],
                'is_active' => $singleGroup['selected'],
                'server_url' => $server_url,
                'is_group' => true,
                'group_id' => $singleGroup['id'],
                'is_default' => false,
                'has_groupfolder' => true,
                'type' => $type
            );
        }


        return $ieAccountsShowOptionsMeta;
    }

    public function getShowAccountsOptionsForSearch(&$ie)
    {
        global $current_user;
        global $app_strings;

        $ieAccountsFull = $ie->retrieveAllByGroupId($current_user->id);
        //$ieAccountsShowOptions = "<option value=''>{$app_strings['LBL_NONE']}</option>\n";
        $ieAccountsShowOptionsMeta = array();
        $ieAccountsShowOptionsMeta[] = array("value" => "", "text" => $app_strings['LBL_NONE'], 'selected' => '');
        $showFolders = sugar_unserialize(base64_decode($current_user->getPreference('showFolders', 'Emails')));

        foreach ($ieAccountsFull as $k => $v) {
            if (!in_array($v->id, $showFolders)) {
                continue;
            }
            $group = (!$v->is_personal) ? $app_strings['LBL_EMAIL_GROUP'] . "." : "";
            $ieAccountsShowOptionsMeta[] = array(
                "value" => $v->id,
                "text" => $group . $v->name,
                'protocol' => $v->protocol
            );
        }

        return $ieAccountsShowOptionsMeta;
    }

    /**
     * Formats a display message on successful async call
     * @param string $type Type of message to display
     */
    public function displaySuccessMessage($type)
    {
        global $app_strings;

        switch ($type) {
            case "delete":
                $message = $app_strings['LBL_EMAIL_DELETE_SUCCESS'];
                break;

            default:
                $message = "NOOP: invalid type";
                break;
        }

        $this->smarty->assign('app_strings', $app_strings);
        $this->smarty->assign('message', $message);
        echo $this->smarty->fetch("modules/Emails/templates/successMessage.tpl");
    }

    /**
     * Validates existence and expiration of a cache file
     * @param string $ieId
     * @param string $type Type of cache file: folders, messages, etc.
     * @param string $file The cachefile name
     * @param int refreshOffset Refresh time in secs.
     * @return mixed.
     */
    public function validCacheFileExists($ieId, $type, $file, $refreshOffset = -1)
    {
        global $sugar_config;

        if ($refreshOffset == -1) {
            $refreshOffset = $this->cacheTimeouts[$type]; // use defaults
        }

        $cacheFilePath = sugar_cached("modules/Emails/{$ieId}/{$type}/{$file}");
        if (file_exists($cacheFilePath)) {
            return true;
        }

        return false;
    }

    /**
     * retrieves the cached value
     * @param string $ieId
     * @param string $type Type of cache file: folders, messages, etc.
     * @param string $file The cachefile name
     * @param string $key name of cache value
     * @return mixed
     */
    public function getCacheValue($ieId, $type, $file, $key)
    {
        global $sugar_config;

        $cleanIeId = cleanDirName($ieId);
        $cleanType = cleanDirName($type);
        $cleanFile = cleanFileName($file);
        $cacheFilePath = sugar_cached("modules/Emails/{$cleanIeId}/{$cleanType}/{$cleanFile}");
        $cacheFile = array();

        if (file_exists($cacheFilePath)) {
            include($cacheFilePath); // provides $cacheFile

            if (isset($cacheFile[$key])) {
                return unserialize($cacheFile[$key], ['allowed_classes' => false]);
            }
        } else {
            $GLOBALS['log']->debug("EMAILUI: cache file not found [ {$cacheFilePath} ] - creating blank cache file");
            $this->writeCacheFile('retArr', array(), $ieId, $type, $file);
        }

        return null;
    }

    /**
     * retrieves the cache file last touched time
     * @param string $ieId
     * @param string $type Type of cache file: folders, messages, etc.
     * @param string $file The cachefile name
     * @return string
     */
    public function getCacheTimestamp($ieId, $type, $file)
    {
        global $sugar_config;

        $cacheFilePath = sugar_cached("modules/Emails/{$ieId}/{$type}/{$file}");
        $cacheFile = array();

        if (file_exists($cacheFilePath)) {
            include($cacheFilePath); // provides $cacheFile['timestamp']

            if (isset($cacheFile['timestamp'])) {
                $GLOBALS['log']->debug("EMAILUI: found timestamp [ {$cacheFile['timestamp']} ]");

                return $cacheFile['timestamp'];
            }
        }

        return '';
    }

    /**
     * Updates the timestamp for a cache file - usually to mark a "check email"
     * process
     * @param string $ieId
     * @param string $type Type of cache file: folders, messages, etc.
     * @param string $file The cachefile name
     */
    public function setCacheTimestamp($ieId, $type, $file)
    {
        global $sugar_config;

        $cacheFilePath = sugar_cached("modules/Emails/{$ieId}/{$type}/{$file}");
        $cacheFile = array();

        if (file_exists($cacheFilePath)) {
            include($cacheFilePath); // provides $cacheFile['timestamp']

            if (isset($cacheFile['timestamp'])) {
                $cacheFile['timestamp'] = strtotime('now');
                $GLOBALS['log']->debug("EMAILUI: setting updated timestamp [ {$cacheFile['timestamp']} ]");

                return $this->_writeCacheFile($cacheFile, $cacheFilePath);
            }
        }
    }


    /**
     * Writes caches to flat file in cache dir.
     * @param string $key Key to the main cache entry (not timestamp)
     * @param mixed $var Variable to be cached
     * @param string $ieId I-E focus ID
     * @param string $type Folder in cache
     * @param string $file Cache file name
     */
    public function writeCacheFile($key, $var, $ieId, $type, $file)
    {
        global $sugar_config;

        $cleanIeId = cleanDirName($ieId);
        $cleanType = cleanDirName($type);
        $cleanFile = cleanFileName($file);
        $the_file = sugar_cached("modules/Emails/{$cleanIeId}/{$cleanType}/{$cleanFile}");
        $timestamp = strtotime('now');
        $array = array();
        $array['timestamp'] = $timestamp;
        $array[$key] = serialize($var); // serialized since varexport_helper() can't handle PHP objects

        return $this->_writeCacheFile($array, $the_file);
    }

    /**
     * Performs the actual file write.  Abstracted from writeCacheFile() for
     * flexibility
     * @param array $array The array to write to the cache
     * @param string $file Full path (relative) with cache file name
     * @return bool
     */
    public function _writeCacheFile($array, $file)
    {
        global $sugar_config;

        $arrayString = var_export_helper($array);

        $date = date("r");
        $the_string = <<<eoq
<?php // created: {$date}
    \$cacheFile = {$arrayString};
?>
eoq;
        if ($fh = @sugar_fopen($file, "w")) {
            fwrite($fh, $the_string);
            fclose($fh);

            return true;
        } else {
            $GLOBALS['log']->debug("EMAILUI: Could not write cache file [ {$file} ]");

            return false;
        }
    }

    /**
     * Generate JSON encoded data to be consumed by yui datatable.
     *
     * @param array $data
     * @param string $resultsParam The resultsList name
     * @return string
     */
    public function jsonOuput($data, $resultsParam, $count = 0, $fromCache = true, $unread = -1)
    {

        global $app_strings;

        $count = ($count > 0) ? $count : 0;

        $cached = ($fromCache) ? 1 : 0;

        if ($data['mbox'] == 'undefined' || empty($data['mbox'])) {
            $data['mbox'] = $app_strings['LBL_NONE'];
        }

        $jsonOut = array(
            'TotalCount' => $count,
            'FromCache' => $cached,
            'UnreadCount' => $unread,
            $resultsParam => $data['out']
        );

        return json_encode($jsonOut);
    }

    /**
     * generates XML output from an array
     * @param array
     * @param string master list Item
     * @return string
     */
    public function xmlOutput($a, $paramName, $count = 0, $fromCache = true, $unread = -1)
    {
        global $app_strings;
        $count = ($count > 0) ? $count : 0;

        if (isset($a['fromCache'])) {
            $cached = ($a['fromCache'] == 1) ? 1 : 0;
        } else {
            $cached = ($fromCache) ? 1 : 0;
        }

        if ($a['mbox'] == 'undefined' || empty($a['mbox'])) {
            $a['mbox'] = $app_strings['LBL_NONE'];
        }

        $xml = $this->arrayToXML($a['out'], $paramName);

        $ret = <<<eoq
<?xml version="1.0" encoding="UTF-8"?>
<EmailPage>
<TotalCount>{$count}</TotalCount>
<UnreadCount>{$unread}</UnreadCount>
<FromCache> {$cached} </FromCache>
<{$paramName}s>
{$xml}
</{$paramName}s>
</EmailPage>
eoq;

        return $ret;
    }

    /**
     * Generate to/cc addresses string in email detailview.
     *
     * @param string $str
     * @param string $target values: to, cc
     * @param int $defaultNum
     * @return string $str
     */
    public function generateExpandableAddrs($str)
    {
        global $mod_strings;
        $tempStr = $str . ',';
        $tempStr = html_entity_decode($tempStr);
        $tempStr = $this->unifyEmailString($tempStr);
        $defaultNum = 2;
        $pattern = '/@.*,/U';
        preg_match_all($pattern, $tempStr, $matchs);
        $totalCount = is_countable($matchs[0]) ? count($matchs[0]) : 0;

        if (!empty($matchs[0]) && $totalCount > $defaultNum) {
            $position = strpos($tempStr, (string) $matchs[0][$defaultNum]);
            $hiddenCount = $totalCount - $defaultNum;
            $frontStr = substr($tempStr, 0, $position);
            $backStr = substr($tempStr, $position, -1);
            $str = htmlentities($frontStr) . '<a class="utilsLink" onclick="SUGAR.email2.detailView.displayAllAddrs(this);">...[' . $mod_strings['LBL_EMAIL_DETAIL_VIEW_SHOW'] . $hiddenCount . $mod_strings['LBL_EMAIL_DETAIL_VIEW_MORE'] . ']</a><span style="display: none;">' . htmlentities($backStr) . '</span>';
        }

        return $str;
    }

    /**
     * Unify the separator as ,
     *
     * @param String $str email address string
     * @return String converted string
     */
    public function unifyEmailString($str)
    {
        $new = [];
        preg_match_all('/@.*;/U', $str, $matches);
        if (!empty($matches[0])) {
            foreach ($matches[0] as $key => $value) {
                $new[] = str_replace(";", ",", (string) $value);
            }

            return str_replace($matches[0], $new, $str);
        }

        return $str;
    }
} // end class def
