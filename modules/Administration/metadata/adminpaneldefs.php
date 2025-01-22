<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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

global $current_user, $admin_group_header;

//users and security.
$admin_option_defs = [];
$admin_option_defs['Users']['user_management'] = [
    'UserManagement',
    'LBL_MANAGE_USERS_TITLE',
    'LBL_MANAGE_USERS',
    './index.php?module=Users&action=index',
    'user-management'
];
$admin_option_defs['Administration']['password_management'] = [
    'Password',
    'LBL_MANAGE_PASSWORD_TITLE',
    'LBL_MANAGE_PASSWORD',
    './index.php?module=Administration&action=PasswordManager',
    'password'
];
$admin_option_defs['Administration']['oauth2_clients'] = [
    'Password',
    'LBL_OAUTH2_CLIENTS_TITLE',
    'LBL_OAUTH2_CLIENTS',
    './index.php?module=OAuth2Clients&action=index',
    'oauth2'
];
require_once 'include/SugarOAuthServer.php';
if (SugarOAuthServer::enabled()) {
    $admin_option_defs['Administration']['oauth_keys'] = [
        'Password',
        'LBL_OAUTH_TITLE',
        'LBL_OAUTH',
        './index.php?module=OAuthKeys&action=index',
        'oauth-keys'
    ];
}
$admin_option_defs['Users']['roles_management'] = [
    'Roles',
    'LBL_MANAGE_ROLES_TITLE',
    'LBL_MANAGE_ROLES',
    './index.php?module=ACLRoles&action=index',
    'roles'
];
$admin_option_defs['Administration']['securitygroup_management'] = [
    'SecuritySuiteGroupManagement',
    'LBL_MANAGE_SECURITYGROUPS_TITLE',
    'LBL_MANAGE_SECURITYGROUPS',
    './index.php?module=SecurityGroups&action=index',
    'security'
];
$admin_option_defs['Administration']['securitygroup_config'] = [
    'SecurityGroupsManagement',
    'LBL_CONFIG_SECURITYGROUPS_TITLE',
    'LBL_CONFIG_SECURITYGROUPS',
    './index.php?module=SecurityGroups&action=config',
    'security-suite-group-management'
];

$admin_group_header[] = ['LBL_USERS_TITLE', '', false, $admin_option_defs, 'LBL_USERS_DESC'];

//system.
$admin_option_defs = [];
$admin_option_defs['Administration']['configphp_settings'] = [
    'Administration',
    'LBL_CONFIGURE_SETTINGS_TITLE',
    'LBL_CONFIGURE_SETTINGS',
    './index.php?module=Configurator&action=EditView',
    'system-settings'
];
$admin_option_defs['Administration']['currencies_management'] = [
    'Currencies',
    'LBL_MANAGE_CURRENCIES',
    'LBL_CURRENCY',
    './index.php?module=Currencies&action=index',
    'currencies'
];
$admin_option_defs['Administration']['languages'] = [
    'Currencies',
    'LBL_MANAGE_LANGUAGES',
    'LBL_LANGUAGES',
    './index.php?module=Administration&action=Languages&view=default',
    'languages'
];
$admin_option_defs['Administration']['locale'] = [
    'Currencies',
    'LBL_MANAGE_LOCALE',
    'LBL_LOCALE',
    './index.php?module=Administration&action=Locale&view=default',
    'locale'
];
$admin_option_defs['Administration']['pdf'] = [
    'PDF',
    'LBL_PDF_HEADER',
    'LBL_CHANGE_PDF_SETTINGS',
    './index.php?module=Administration&action=PDFSettings',
    'pdf-settings'
];
$admin_option_defs['Administration']['search_wrapper'] = [
    'icon_SearchForm',
    'LBL_SEARCH_WRAPPER',
    'LBL_SEARCH_WRAPPER_DESC',
    './index.php?module=Administration&action=SearchSettings',
    'global-search'
];
$admin_option_defs['Administration']['elastic_search'] = [
    'ElasticSearchIndexerSettings',
    'LBL_ELASTIC_SEARCH_SETTINGS',
    'LBL_ELASTIC_SEARCH_SETTINGS_DESC',
    './index.php?module=Administration&action=ElasticSearchSettings',
    'global-search'
];
$admin_option_defs['Administration']['scheduler'] = [
    'Schedulers',
    'LBL_SUITE_SCHEDULER_TITLE',
    'LBL_SUITE_SCHEDULER',
    './index.php?module=Schedulers&action=index',
    'scheduler'
];
// Theme Enable/Disable
$admin_option_defs['Administration']['theme_settings'] = [
    'icon_AdminThemes',
    'LBL_THEME_SETTINGS',
    'LBL_THEME_SETTINGS_DESC',
    './index.php?module=Administration&action=ThemeSettings',
    'themes'
];

$admin_group_header[] = ['LBL_ADMINISTRATION_HOME_TITLE', '', false, $admin_option_defs, 'LBL_ADMINISTRATION_HOME_DESC'];

//Module Settings
$admin_option_defs = [];
$admin_option_defs['Administration']['feed_settings'] = [
    'icon_SugarFeed',
    'LBL_SUITEFEED_SETTINGS',
    'LBL_SUITEFEED_SETTINGS_DESC',
    './index.php?module=SugarFeed&action=AdminSettings',
    'activity-streams'
];
$admin_option_defs['Administration']['business_hours'] = [
    'AOBH_BusinessHours',
    'LBL_BUSINESS_HOURS',
    'LBL_AOP_BUSINESS_HOURS_DESC',
    './index.php?module=Administration&action=BusinessHours',
    'aobh-businesshours'
];
$admin_option_defs['Administration']['aop'] = [
    'AOP',
    'LBL_AOP_SETTINGS',
    'LBL_CHANGE_SETTINGS_AOP',
    './index.php?module=Administration&action=AOPAdmin',
    'aop-settings'
];
$admin_option_defs['Administration']['configure_group_tabs'] = [
    'ConfigureTabs',
    'LBL_CONFIGURE_GROUP_TABS',
    'LBL_CONFIGURE_GROUP_TABS_DESC',
    './index.php?action=wizard&module=Studio&wizard=StudioWizard&option=ConfigureGroupTabs',
    'configure-module-menu-filters'
];
$admin_option_defs['Administration']['connector_settings'] = [
    'icon_Connectors',
    'LBL_CONNECTOR_SETTINGS',
    'LBL_CONNECTOR_SETTINGS_DESC',
    './index.php?module=Connectors&action=ConnectorSettings',
    'connectors'
];
$admin_option_defs['Administration']['configure_tabs'] = [
    'ConfigureTabs',
    'LBL_CONFIGURE_TABS_AND_SUBPANELS',
    'LBL_CONFIGURE_TABS_AND_SUBPANELS_DESC',
    './index.php?module=Administration&action=ConfigureTabs',
    'display-modules-and-subpanels'
];
$admin_option_defs['Administration']['history_contacts_emails'] = [
    'ConfigureTabs',
    'LBL_HISTORY_CONTACTS_EMAILS',
    'LBL_HISTORY_CONTACTS_EMAILS_DESC',
    './index.php?module=Configurator&action=historyContactsEmails',
    'history-subpanel'
];
$admin_option_defs['Bugs']['bug_tracker'] = [
    'Releases',
    'LBL_MANAGE_RELEASES',
    'LBL_RELEASE',
    './index.php?module=Releases&action=index',
    'releases'
];
$admin_option_defs['Administration']['aos'] = [
    'AOS',
    'LBL_AOS_SETTINGS',
    'LBL_CHANGE_SETTINGS',
    './index.php?module=Administration&action=AOSAdmin',
    'aos-settings'
];

$admin_group_header['sagility'] = ['LBL_MODULE_ADMIN', '', false, $admin_option_defs, 'LBL_MODULE_ADMIN_HEADER_DESC'];


//email manager.
$admin_option_defs = [];
$admin_option_defs['Emails']['mass_Email_config'] = [
    'EmailMan',
    'LBL_MASS_EMAIL_CONFIG_TITLE',
    'LBL_MASS_EMAIL_CONFIG_DESC',
    './index.php?module=EmailMan&action=config',
    'email-settings'
];

$admin_option_defs['Campaigns']['campaignconfig'] = [
    'EmailCampaigns',
    'LBL_CAMPAIGN_CONFIG_TITLE',
    'LBL_CAMPAIGN_CONFIG_DESC',
    './index.php?module=EmailMan&action=campaignconfig',
    'campaign-email-settings'
];

$admin_option_defs['Emails']['mailboxes'] = [
    'EmailInbound',
    'LBL_MANAGE_MAILBOX',
    'LBL_MAILBOX_DESC',
    './index.php?module=InboundEmail&action=index',
    'inbound-email'
];
$admin_option_defs['Emails']['mailboxes_outbound'] = [
    'EmailOutbound',
    'LBL_MANAGE_MAILBOX_OUTBOUND',
    'LBL_MAILBOX_OUTBOUND_DESC',
    './index.php?module=OutboundEmailAccounts&action=index',
    'outbound-email'
];
$admin_option_defs['Emails']['external_oauth_connections'] = [
    'ExternalOAuthConnection',
    'LBL_MANAGE_EXTERNAL_OAUTH_CONNECTIONS',
    'LBL_MANAGE_EXTERNAL_OAUTH_CONNECTIONS_DESC',
    'index.php?module=ExternalOAuthConnection&action=index',
    'oauth2'
];
$admin_option_defs['Emails']['external_oauth_providers'] = [
    'ExternalOAuthProvider',
    'LBL_MANAGE_EXTERNAL_OAUTH_PROVIDERS',
    'LBL_MANAGE_EXTERNAL_OAUTH_PROVIDERS_DESC',
    'index.php?module=ExternalOAuthProvider&action=index',
    'oauth2'
];
$admin_option_defs['Campaigns']['mass_Email'] = [
    'EmailQueue',
    'LBL_MASS_EMAIL_MANAGER_TITLE',
    'LBL_MASS_EMAIL_MANAGER_DESC',
    './index.php?module=EmailMan&action=index',
    'email-queue'
];


$admin_group_header[] = ['LBL_EMAIL_TITLE', '', false, $admin_option_defs, 'LBL_EMAIL_DESC'];

//admin tools
$admin_option_defs = [];
$admin_option_defs['Administration']['repair'] = [
    'Repair',
    'LBL_UPGRADE_TITLE',
    'LBL_UPGRADE',
    './index.php?module=Administration&action=Upgrade',
    'repair'
];
if (!isset($GLOBALS['sugar_config']['hide_admin_backup']) || !$GLOBALS['sugar_config']['hide_admin_backup']) {
    $admin_option_defs['Administration']['backup_management'] = [
        'Backups',
        'LBL_BACKUPS_TITLE',
        'LBL_BACKUPS',
        './index.php?module=Administration&action=Backups',
        'backups'
    ];
}
if (!isset($GLOBALS['sugar_config']['hide_admin_diagnostics']) || !$GLOBALS['sugar_config']['hide_admin_diagnostics']) {
    $admin_option_defs['Administration']['diagnostic'] = [
        'Diagnostic',
        'LBL_DIAGNOSTIC_TITLE',
        'LBL_DIAGNOSTIC_DESC',
        './index.php?module=Administration&action=Diagnostic',
        'diagnostic'
    ];
}
$admin_option_defs['Administration']['import'] = [
    'Import',
    'LBL_IMPORT_WIZARD',
    'LBL_IMPORT_WIZARD_DESC',
    './index.php?module=Import&action=step1&import_module=Administration',
    'import'
];
$admin_option_defs['Administration']['module_loader'] = [
    'ModuleLoader',
    'LBL_MODULE_LOADER_TITLE',
    'LBL_MODULE_LOADER',
    './index.php?module=Administration&action=UpgradeWizard&view=module',
    'module-loader'
];
if (!defined('TEMPLATE_URL')) {
    $admin_option_defs['Administration']['upgrade_wizard'] = [
        'Upgrade',
        'LBL_UPGRADE_WIZARD_TITLE',
        'LBL_UPGRADE_WIZARD',
        './index.php?module=UpgradeWizard&action=index',
        'upgrade-wizard'
    ];
}

$admin_group_header[] = ['LBL_ADMIN_TOOLS_TITLE', '', false, $admin_option_defs, 'LBL_ADMIN_TOOLS_HEADER_DESC'];

//studio.
$admin_option_defs = [];
$admin_option_defs['studio']['studio'] = [
    'Studio',
    'LBL_STUDIO',
    'LBL_STUDIO_DESC',
    './index.php?module=ModuleBuilder&action=index&type=studio',
    'studio'
];
if (isset($GLOBALS['beanFiles']['iFrame'])) {
    $admin_option_defs['Administration']['portal'] = [
        'iFrames',
        'LBL_IFRAME',
        'DESC_IFRAME',
        './index.php?module=iFrames&action=index'
    ];
}
$admin_option_defs['Administration']['rename_tabs'] = [
    'RenameTabs',
    'LBL_RENAME_TABS',
    'LBL_CHANGE_NAME_MODULES',
    "./index.php?action=wizard&module=Studio&wizard=StudioWizard&option=RenameTabs",
    'rename-modules'
];
$admin_option_defs['Administration']['moduleBuilder'] = [
    'ModuleBuilder',
    'LBL_MODULEBUILDER',
    'LBL_MODULEBUILDER_DESC',
    './index.php?module=ModuleBuilder&action=index&type=mb',
    'module-builder'
];

$admin_option_defs['any']['dropdowneditor'] = [
    'Dropdown',
    'LBL_DROPDOWN_EDITOR',
    'DESC_DROPDOWN_EDITOR',
    './index.php?module=ModuleBuilder&action=index&type=dropdowns',
    'dropdown-editor'
];
$admin_option_defs['any']['workflow_management'] = [
    'Workflow',
    'LBL_WORKFLOW_MANAGER',
    'LBL_WORKFLOW_MANAGER_DESC',
    './index.php?module=AOW_WorkFlow',
    'workflow'
];

$admin_group_header[] = ['LBL_STUDIO_TITLE', '', false, $admin_option_defs, 'LBL_TOOLS_DESC'];

//Google Settings
$admin_option_defs = [];

$admin_option_defs['Administration']['google_calendar_settings'] = [
    'Google Calendar Settings',
    'LBL_GOOGLE_CALENDAR_SETTINGS_TITLE',
    'LBL_GOOGLE_CALENDAR_SETTINGS_DESC',
    './index.php?module=Administration&action=GoogleCalendarSettings',
    'system-settings'
];
$admin_option_defs['jjwg_Maps']['config'] = [
    'Administration',
    'LBL_JJWG_MAPS_ADMIN_CONFIG_TITLE',
    'LBL_JJWG_MAPS_ADMIN_CONFIG_DESC',
    './index.php?module=jjwg_Maps&action=config',
    'google-maps-settings'
];
$admin_option_defs['jjwg_Maps']['geocoded_counts'] = [
    'Geocoded_Counts',
    'LBL_JJWG_MAPS_ADMIN_GEOCODED_COUNTS_TITLE',
    'LBL_JJWG_MAPS_ADMIN_GEOCODED_COUNTS_DESC',
    './index.php?module=jjwg_Maps&action=geocoded_counts',
    'geocoded-counts'
];
$admin_option_defs['jjwg_Maps']['geocoding_test'] = [
    'GeocodingTests',
    'LBL_JJWG_MAPS_ADMIN_GEOCODING_TEST_TITLE',
    'LBL_JJWG_MAPS_ADMIN_GEOCODING_TEST_DESC',
    './index.php?module=jjwg_Maps&action=geocoding_test',
    'geocoding-test'
];
$admin_option_defs['jjwg_Maps']['geocode_addresses'] = [
    'GeocodeAddresses',
    'LBL_JJWG_MAPS_ADMIN_GEOCODE_ADDRESSES_TITLE',
    'LBL_JJWG_MAPS_ADMIN_GEOCODE_ADDRESSES_DESC',
    './index.php?module=jjwg_Maps&action=geocode_addresses',
    'geocode-addresses'
];
$admin_option_defs['jjwg_Maps']['address_cache'] = [
    'Address_Cache',
    'LBL_JJWG_MAPS_ADMIN_ADDRESS_CACHE_TITLE',
    'LBL_JJWG_MAPS_ADMIN_ADDRESS_CACHE_DESC',
    './index.php?module=jjwg_Address_Cache&action=index',
    'address-cache'
];

$admin_group_header[] = [
    'LBL_GOOGLE_SUITE_ADMIN_HEADER',
    '',
    false,
    $admin_option_defs,
    'LBL_GOOGLE_SUITE_ADMIN_DESC'
];

if (file_exists('custom/modules/Administration/Ext/Administration/administration.ext.php')) {
    include('custom/modules/Administration/Ext/Administration/administration.ext.php');
}

//For users with MLA access we need to find which entries need to be shown.
//lets process the $admin_group_header and apply all the access control rules.
$access = $current_user->getDeveloperModules();
foreach ($admin_group_header as $key => $values) {
    $module_index = array_keys($values[3]);  //get the actual links..
    foreach ($module_index as $mod_key => $mod_val) {
        if (is_admin($current_user) ||
            in_array($mod_val, $access) ||
            $mod_val == 'studio' ||
            ($mod_val == 'Forecasts' && in_array('ForecastSchedule', $access)) ||
            ($mod_val == 'any')
        ) {
            if (!is_admin($current_user) && isset($values[3]['Administration'])) {
                unset($values[3]['Administration']);
            }
            if (displayStudioForCurrentUser() == false) {
                unset($values[3]['studio']);
            }

            if (displayWorkflowForCurrentUser() == false) {
                unset($values[3]['any']['workflow_management']);
            }

            // Need this check because Quotes and Products share the header group
            if (!in_array('Quotes', $access) && isset($values[3]['Quotes'])) {
                unset($values[3]['Quotes']);
            }
            if (!in_array('Products', $access) && isset($values[3]['Products'])) {
                unset($values[3]['Products']);
            }

            // Need this check because Emails and Campaigns share the header group
            if (!in_array('Campaigns', $access) && isset($values[3]['Campaigns'])) {
                unset($values[3]['Campaigns']);
            }

            //////////////////
        } else {
            //hide the link
            unset($admin_group_header[$key][3][$mod_val]);
        }
    }
}
