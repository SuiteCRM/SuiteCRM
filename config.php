<?php
/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 * 
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo, "Supercharged by SuiteCRM" logo and “Nonprofitized by SinergiaCRM” logo. 
 * If the display of the logos is not reasonably feasible for technical reasons, 
 * the Appropriate Legal Notices must display the words "Powered by SugarCRM", 
 * "Supercharged by SuiteCRM" and “Nonprofitized by SinergiaCRM”. 
 */

// created: 2022-06-15 12:16:26
$sugar_config = array(
    'DHA_OpenOffice_HOME' => '',
    'DHA_OpenOffice_cde' => '',
    'DHA_OpenOffice_exe' => '',
    'DHA_templates_default_lang' => 'en',
    'DHA_templates_dir' => 'document_templates/',
    'KReports' => array(
        'authCheck' => 'SecurityGroups',
    ),
    'addAjaxBannedModules' => array(
        0 => 'SecurityGroups',
        1 => 'KReports',
        // STIC#1086 - Do not apply Ajax in this module, the same as in Import module.
        2 => 'stic_Import_Validation',
        // END STIC
        // STIC - JCH - 20240118 - Avoid Ajax in module Rules
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/66
        3 => 'stic_Security_Groups_Rules',
        // END STIC
    ),
    'admin_access_control' => false,
    'admin_export_only' => false,
    'allowed_preview' => array(
        0 => 'pdf',
        1 => 'gif',
        2 => 'png',
        3 => 'jpeg',
        4 => 'jpg',
    ),
    'anti_malware_scanners' => array(
        'SuiteCRM\\Utility\\AntiMalware\\Providers\\ClamTCP' => array(
            'name' => 'ClamAntiVirus TCP',
            'support_page' => 'https://www.clamav.net/',
            'enabled' => false,
            'path' => null,
            'options' => array(
                'ip' => '127.0.0.1',
                'port' => 3310,
                'type' => 'local',
            ),
        ),
        'SuiteCRM\\Utility\\AntiMalware\\Providers\\Sophos' => array(
            'name' => 'Sophos Anti Virus (Linux)',
            'support_page' => 'https://www.sophos.com/en-us/products/free-tools/sophos-antivirus-for-linux.aspx',
            'enabled' => false,
            'path' => '/opt/sophos-av/bin/savscan',
            'options' => '-ss',
        ),
    ),
    'aod' => array(
        'enable_aod' => false,
    ),
    'aop' => array(
        'distribution_method' => 'roundRobin',
        'case_closure_email_template_id' => '9bdd16cf-cfc6-cc8a-d636-5d712114f0b6',
        'joomla_account_creation_email_template_id' => 'a2a24046-6c5d-ee54-ee5b-5d71212d51dd',
        'case_creation_email_template_id' => 'a9d6acda-c744-8789-5f32-5d71216acfda',
        'contact_email_template_id' => 'b40e6111-b322-8e81-7b62-5d7121787e6a',
        'user_email_template_id' => 'bae07d7b-3be3-f3d4-f5cf-5d7121bd6121',
    ),
    'aos' => array(
        'version' => '5.3.3',
        'contracts' => array(
            'renewalReminderPeriod' => '14',
        ),
        'lineItems' => array(
            'totalTax' => false,
            'enableGroups' => true,
        ),
        'invoices' => array(
            'initialNumber' => '1',
        ),
        'quotes' => array(
            'initialNumber' => '1',
        ),
    ),
    'cache_dir' => 'cache/',
    'calculate_response_time' => true,
    'calendar' => array(
        'default_view' => 'week',
        'show_calls_by_default' => true,
        'show_tasks_by_default' => true,
        'show_completed_by_default' => true,
        'editview_width' => 990,
        'editview_height' => 485,
        'day_timestep' => 15,
        'week_timestep' => 30,
        'items_draggable' => true,
        'items_resizable' => true,
        'enable_repeat' => true,
        'max_repeat_count' => 1000,
    ),
    'chartEngine' => 'Jit',
    'common_ml_dir' => '',
    'create_default_user' => false,
    'cron' => array(
        'max_cron_jobs' => 10,
        'max_cron_runtime' => 30,
        'min_cron_interval' => 30,
        'allowed_cron_users' => array(
            0 => 'www-data',
        ),
    ),
    'currency' => '',
    'dashlet_auto_refresh_min' => '30',
    'dashlet_display_row_options' => array(
        0 => '1',
        1 => '3',
        2 => '5',
        3 => '10',
    ),
    'date_formats' => array(
        'Y-m-d' => '2010-12-23',
        'm-d-Y' => '12-23-2010',
        'd-m-Y' => '23-12-2010',
        'Y/m/d' => '2010/12/23',
        'm/d/Y' => '12/23/2010',
        'd/m/Y' => '23/12/2010',
        'Y.m.d' => '2010.12.23',
        'd.m.Y' => '23.12.2010',
        'm.d.Y' => '12.23.2010',
    ),
    'datef' => 'd/m/Y',
    'dbconfig' => array(
        'db_host_name' => 'mysql',
        'db_host_instance' => 'SQLEXPRESS',
        'db_user_name' => 'root',
        'db_password' => 'stic',
        'db_name' => 'suitecrm',
        'db_type' => 'mysql',
        'db_port' => '',
        'db_manager' => 'MysqliManager',
    ),
    'dbconfigoption' => array(
        'persistent' => true,
        'autofree' => false,
        'debug' => 0,
        'ssl' => false,
    ),
    'default_action' => 'index',
    'default_charset' => 'UTF-8',
    'default_currencies' => array(
        'AUD' => array(
            'name' => 'Australian Dollars',
            'iso4217' => 'AUD',
            'symbol' => '$',
        ),
        'BRL' => array(
            'name' => 'Brazilian Reais',
            'iso4217' => 'BRL',
            'symbol' => 'R$',
        ),
        'GBP' => array(
            'name' => 'British Pounds',
            'iso4217' => 'GBP',
            'symbol' => '£',
        ),
        'CAD' => array(
            'name' => 'Canadian Dollars',
            'iso4217' => 'CAD',
            'symbol' => '$',
        ),
        'CNY' => array(
            'name' => 'Chinese Yuan',
            'iso4217' => 'CNY',
            'symbol' => '￥',
        ),
        'EUR' => array(
            'name' => 'Euro',
            'iso4217' => 'EUR',
            'symbol' => '€',
        ),
        'HKD' => array(
            'name' => 'Hong Kong Dollars',
            'iso4217' => 'HKD',
            'symbol' => '$',
        ),
        'INR' => array(
            'name' => 'Indian Rupees',
            'iso4217' => 'INR',
            'symbol' => '₨',
        ),
        'KRW' => array(
            'name' => 'Korean Won',
            'iso4217' => 'KRW',
            'symbol' => '₩',
        ),
        'YEN' => array(
            'name' => 'Japanese Yen',
            'iso4217' => 'JPY',
            'symbol' => '¥',
        ),
        'MXN' => array(
            'name' => 'Mexican Pesos',
            'iso4217' => 'MXN',
            'symbol' => '$',
        ),
        'SGD' => array(
            'name' => 'Singaporean Dollars',
            'iso4217' => 'SGD',
            'symbol' => '$',
        ),
        'CHF' => array(
            'name' => 'Swiss Franc',
            'iso4217' => 'CHF',
            'symbol' => 'SFr.',
        ),
        'THB' => array(
            'name' => 'Thai Baht',
            'iso4217' => 'THB',
            'symbol' => '฿',
        ),
        'USD' => array(
            'name' => 'US Dollars',
            'iso4217' => 'USD',
            'symbol' => '$',
        ),
    ),
    'default_currency_iso4217' => 'EUR',
    'default_currency_name' => 'Euro',
    'default_currency_significant_digits' => 2,
    'default_currency_symbol' => '€',
    'default_date_format' => 'd/m/Y',
    'default_decimal_seperator' => ',',
    'default_email_charset' => 'UTF-8',
    'default_email_client' => 'sugar',
    'default_email_editor' => 'html',
    'default_export_charset' => 'UTF-8',
    'default_language' => 'en_us',
    'default_locale_name_format' => 's f l',
    'default_max_tabs' => 10,
    'default_module' => 'Home',
    'default_module_favicon' => false,
    'default_navigation_paradigm' => 'gm',
    'default_number_grouping_seperator' => '.',
    'default_password' => '',
    'default_permissions' => array(
        'dir_mode' => 1533,
        'file_mode' => 436,
        'user' => '',
        'group' => '',
    ),
    'default_subpanel_links' => false,
    'default_subpanel_tabs' => true,
    'default_swap_last_viewed' => false,
    'default_swap_shortcuts' => false,
    'default_theme' => 'SuiteP',
    'default_time_format' => 'H:i',
    'default_user_is_admin' => false,
    'default_user_name' => '',
    'demoData' => 'no',
    'developerMode' => false,
    'disable_convert_lead' => false,
    'disable_export' => false,
    'disable_persistent_connections' => false,
    'display_email_template_variable_chooser' => false,
    'display_inbound_email_buttons' => false,
    'dump_slow_queries' => false,
    'email_address_separator' => ',',
    'email_allow_send_as_user' => false,
    'email_confirm_opt_in_email_template_id' => '',
    'email_default_client' => 'sugar',
    'email_default_delete_attachments' => true,
    'email_default_editor' => 'html',
    'email_enable_auto_send_opt_in' => false,
    'email_enable_confirm_opt_in' => 'not-opt-in',
    'email_warning_notifications' => true,
    'email_xss' => 'YToxMzp7czo2OiJhcHBsZXQiO3M6NjoiYXBwbGV0IjtzOjQ6ImJhc2UiO3M6NDoiYmFzZSI7czo1OiJlbWJlZCI7czo1OiJlbWJlZCI7czo0OiJmb3JtIjtzOjQ6ImZvcm0iO3M6NToiZnJhbWUiO3M6NToiZnJhbWUiO3M6ODoiZnJhbWVzZXQiO3M6ODoiZnJhbWVzZXQiO3M6NjoiaWZyYW1lIjtzOjY6ImlmcmFtZSI7czo2OiJpbXBvcnQiO3M6ODoiXD9pbXBvcnQiO3M6NToibGF5ZXIiO3M6NToibGF5ZXIiO3M6NDoibGluayI7czo0OiJsaW5rIjtzOjY6Im9iamVjdCI7czo2OiJvYmplY3QiO3M6MzoieG1wIjtzOjM6InhtcCI7czo2OiJzY3JpcHQiO3M6Njoic2NyaXB0Ijt9',
    'enable_action_menu' => true,
    'enable_line_editing_detail' => true,
    'enable_line_editing_list' => true,
    'export_delimiter' => ',',
    'export_excel_compatible' => false,
    'filter_module_fields' => array(
        'Users' => array(
            0 => 'show_on_employees',
            1 => 'portal_only',
            2 => 'is_group',
            3 => 'system_generated_password',
            4 => 'external_auth_only',
            5 => 'sugar_login',
            6 => 'authenticate_id',
            7 => 'pwd_last_changed',
            8 => 'is_admin',
            9 => 'user_name',
            10 => 'user_hash',
            11 => 'password',
            12 => 'last_login',
            13 => 'oauth_tokens',
        ),
        'Employees' => array(
            0 => 'show_on_employees',
            1 => 'portal_only',
            2 => 'is_group',
            3 => 'system_generated_password',
            4 => 'external_auth_only',
            5 => 'sugar_login',
            6 => 'authenticate_id',
            7 => 'pwd_last_changed',
            8 => 'is_admin',
            9 => 'user_name',
            10 => 'user_hash',
            11 => 'password',
            12 => 'last_login',
            13 => 'oauth_tokens',
        ),
    ),
    'google_auth_json' => '',
    'hide_subpanels' => true,
    'history_max_viewed' => 50,
    'host_name' => 'localhost',
    'id_validation_pattern' => '/^[a-zA-Z0-9_-]*$/i',
    'imap_test' => false,
    'import_max_execution_time' => 3600,
    'import_max_records_per_file' => 100,
    'import_max_records_total_limit' => '',
    'installer_locked' => true,
    'jobs' => array(
        'min_retry_interval' => 30,
        'max_retries' => 5,
        'timeout' => 86400,
        'soft_lifetime' => 30,
        'hard_lifetime' => 60,
    ),
    'js_custom_version' => 3,
    'js_lang_version' => 15,
    'languages' => array(
        'en_us' => 'English (US)',
        'ca_ES' => 'Català',
        'es_ES' => 'Español',
        'gl_ES' => 'Galego',
    ),
    'large_scale_test' => false,
    'lead_conv_activity_opt' => 'donothing',
    'list_max_entries_per_page' => 20,
    'list_max_entries_per_subpanel' => 10,
    'lock_default_user_name' => false,
    'lock_homepage' => false,
    'lock_subpanels' => false,
    'log_dir' => '.',
    'log_file' => 'suitecrm.log',
    'log_memory_usage' => false,
    'logger' => array(
        'level' => 'error',
        'file' => array(
            'ext' => '.log',
            'name' => 'suitecrm',
            'dateFormat' => '%c',
            'maxSize' => '1MB',
            'maxLogs' => 10,
            'suffix' => '',
        ),
    ),
    'max_dashlets_homepage' => '15',
    'name_formats' => array(
        's f l' => 's f l',
        'f l' => 'f l',
        's l' => 's l',
        'l, s f' => 'l, s f',
        'l, f' => 'l, f',
        's l, f' => 's l, f',
        'l s f' => 'l s f',
        'l f s' => 'l f s',
    ),
    'oauth2_encryption_key' => 'CKEKor9tQeCDIxzl+QWiHHRF/V1FzDvwHRSPKkR3c64=',
    'passwordsetting' => array(
        'SystemGeneratedPasswordON' => '',
        'generatepasswordtmpl' => 'b4477938-f3c4-bef6-292c-5e830dee6ceb',
        'lostpasswordtmpl' => 'bc4c87c3-febd-73d5-cf73-5e830dd3bac0',
        'factoremailtmpl' => '79bc6460-82af-c349-4a86-5d7121f685eb',
        'forgotpasswordON' => true,
        'linkexpiration' => '1',
        'linkexpirationtime' => '30',
        'linkexpirationtype' => '1',
        'systexpiration' => '1',
        'systexpirationtime' => '7',
        'systexpirationtype' => '1',
        'systexpirationlogin' => '',
    ),
    'pdf' => array(
        'defaultEngine' => 'TCPDFEngine',
    ),
    'portal_view' => 'single_user',
    'require_accounts' => true,
    'resource_management' => array(
        'special_query_limit' => 50000,
        'special_query_modules' => array(
            0 => 'Reports',
            1 => 'Export',
            2 => 'Import',
            3 => 'Administration',
            4 => 'Sync',
        ),
        'default_limit' => 50000,
    ),
    'rss_cache_time' => '10800',
    'save_query' => 'all',
    'search' => array(
        'controller' => 'UnifiedSearch',
        'defaultEngine' => 'BasicSearchEngine',
        'pagination' => array(
            'min' => 10,
            'max' => 50,
            'step' => 10,
        ),
        'ElasticSearch' => array(
            'enabled' => false,
            'host' => 'localhost',
            'user' => '',
            'pass' => '',
        ),
    ),
    'search_wildcard_char' => '%',
    'search_wildcard_infront' => false,
    'securitysuite_additive' => true,
    'securitysuite_filter_user_list' => false,
    'securitysuite_inherit_assigned' => true,
    'securitysuite_inherit_creator' => true,
    'securitysuite_inherit_parent' => true,
    'securitysuite_popup_select' => false,
    'securitysuite_strict_rights' => false,
    'securitysuite_user_popup' => true,
    'securitysuite_user_role_precedence' => true,
    'securitysuite_version' => '6.5.17',
    'session_dir' => '',
    'session_gc' => array(
        'enable' => true,
        'gc_probability' => 1,
        'gc_divisor' => 100,
    ),
    'showDetailData' => true,
    'showThemePicker' => true,
    'site_url' => 'http://localhost:2000',
    'slow_query_time_msec' => '100',
    'stackTrace' => false,
    'stack_trace_errors' => false,
    'strict_id_validation' => false,
    'sugar_version' => '6.5.25',
    'sugarbeet' => false,
    'suitecrm_version' => '7.14.5',
    'system_email_templates' => array(
        'confirm_opt_in_template_id' => '9cbe9b2c-0b86-ad83-dd83-5d71212484f4',
    ),
    'time_formats' => array(
        'H:i' => '23:00',
        'h:ia' => '11:00pm',
        'h:iA' => '11:00PM',
        'h:i a' => '11:00 pm',
        'h:i A' => '11:00 PM',
        'H.i' => '23.00',
        'h.ia' => '11.00pm',
        'h.iA' => '11.00PM',
        'h.i a' => '11.00 pm',
        'h.i A' => '11.00 PM',
    ),
    'timef' => 'H:i',
    'tmp_dir' => 'cache/xml/',
    'tracker_max_display_length' => 15,
    'translation_string_prefix' => false,
    'unique_key' => 'fw2v2c4chl6ess6t5i1k7p37bpqy25m6',
    'upload_badext' => array(
        0 => 'php',
        1 => 'php3',
        2 => 'php4',
        3 => 'php5',
        4 => 'php6',
        5 => 'php7',
        6 => 'php8',
        7 => 'pl',
        8 => 'cgi',
        9 => 'py',
        10 => 'asp',
        11 => 'cfm',
        12 => 'js',
        13 => 'vbs',
        14 => 'html',
        15 => 'htm',
        16 => 'phtml',
        17 => 'phar',
    ),
    'upload_dir' => 'upload/',
    'upload_maxsize' => 30000000,
    'use_common_ml_dir' => false,
    'use_real_names' => true,
    'valid_image_ext' => array(
        0 => 'gif',
        1 => 'png',
        2 => 'jpg',
        3 => 'jpeg',
        4 => 'svg',
        5 => 'bmp',
    ),
    'vcal_time' => '2',
    'verify_client_ip' => true,

    // STIC Custom 20240826 JBL - Upgrade SuiteCRM core to 7.14.5
    // https://github.com/SinergiaTIC/SinergiaCRM/pull/315
    'legacy_email_behaviour' => true,
    'valid_imap_ports' => array(
        '110', 
        '143', 
        '993', 
        '995'
    ),
    'web_to_lead_allowed_redirect_hosts' => array(),
    'trusted_hosts' => array(),
    'login_language' => true,
    'upload_bad_ext' => array(
        'php',
        'php3',
        'php4',
        'php5',
        'php6',
        'php7',
        'php8',
        'pl',
        'cgi',
        'py',
        'asp',
        'cfm',
        'js',
        'vbs',
        'html',
        'htm',
        'phtml',
        'phar'
    ),
    // END STIC

    // STIC Custom 20211025 MHP - Related to "SinergiaCRM - Purge database" scheduler. Sets the number of days after which records will be totally deleted from database.
    // STIC#448 y STIC#540
    'stic_purge_database_days' => 90,
    'stic_bookings_calendar_default_event_color' => '#b5bc31',
    // END STIC

    // STIC Custom 20220312 JCH - Param to hold $_REQUEST petitions that should bypass clean_incoming_data() function.
    // STIC#633
    'anti_xss_data_exceptions' => array(
        array('module' => 'Campaigns', 'action' => 'WebToLeadFormSave'), // Last step of SuiteCRM webforms wizard
        array('module' => 'stic_Web_Forms', 'action' => 'assistant', 'step' => 'StepDownload'), // Last step of SinergiaCRM webforms wizard
    ),
    // END STIC

    // STIC Custom 20220422 JCH - Allow use custom minute interval in datetimecombo fields & publish in js
    // STIC#708
    'stic_datetime_combo_minute_interval' => 15,
    // END STIC

    // STIC-Custom 20231122 AAM - Deactivate to allow installation through SticInstall.php
    // STIC#1298
    'stic_install_locked' => true,
    // END STIC

    // STIC Custom 20220422 JCH - publish JS variables
    // STIC#708
    // STIC#760
    // https://github.com/SinergiaTIC/SinergiaCRM/pull/3
    'js_available' => array(
        0 => 'stic_datetime_combo_minute_interval',
        1 => 'stic_sinergiada_public',
        2 => 'stic_security_groups_rules_enabled',
    ),
    // END STIC
    
    // STIC Custom 20231124 JBL - SemVer in SinergiaCRM
    // STIC#1319
    'sinergiacrm_version' => '2.1.0', 
    'js_custom_version' => 16,
    'stic_show_update_alert' => 1,
    // END STIC

    // STIC-Custom 20240117 JCH - Security Groups Module Rules activation config
    // https://github.com/SinergiaTIC/SinergiaCRM/pull/3
    'stic_security_groups_rules_enabled' => false,
    // END STIC

    // STIC-Custom 20240528 JBL - Disable AjaxUI calls
    // https://github.com/SinergiaTIC/SinergiaCRM/pull/253
    'disableAjaxUI' => true,
    // END STIC

    // STIC-Custom 20240920 MHP - Change behavior so that tasks are synchronized by default
    // https://github.com/SinergiaTIC/SinergiaCRM/pull/394
    'show_tasks_as_events' => true,
    // END STIC    

    // STIC-Custom 20240709 JCH - Advanced Tab config (multilevel) 
    // STIC https://github.com/SinergiaTIC/SinergiaCRM/pull/208
    'stic_advanced_menu_enabled' => true,
    'stic_advanced_menu_icons' => '1',
    'stic_advanced_menu_all' => '1',
    // END STIC

    // STIC-Custom 20250404 AAM - Remote monitoring
    // https://github.com/SinergiaTIC/SinergiaCRM/pull/618
    'stic_remote_monitor_enabled' => false,
    'stic_remote_monitor_url' => '', 
    'stic_remote_monitor_duration_threshold' => null,
    'stic_remote_monitor_memory_threshold' => null,
    // END STIC

    // STIC Custom 20241016 ART - Tracker prune interval for the Scheduler
    // https://github.com/SinergiaTIC/SinergiaCRM/pull/211
    // Delete from the tracker table the records of the days prior to the ones configured in config
    'tracker_prune_interval' => 90,
    // END STIC Custom
);
