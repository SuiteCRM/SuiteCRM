<?php
// Auto-install script for SuiteCRM
$config = array(
    'setup_db_host_name' => 'mysql',
    'setup_db_database_name' => 'suitecrm',
    'setup_db_admin_user_name' => 'suitecrm',
    'setup_db_admin_password' => 'suitecrm_pass',
    'demoData' => 'yes',
    'setup_db_drop_tables' => 0,
    'setup_db_create_database' => 1,
    'setup_db_pop_demo_data' => 1,
    'setup_site_admin_user_name' => 'admin',
    'setup_site_admin_password' => 'admin123',
    'setup_site_url' => 'http://localhost:8082',
    'default_currency_iso4217' => 'USD',
    'default_currency_name' => 'US Dollar',
    'default_currency_significant_digits' => '2',
    'default_currency_symbol' => '$',
    'default_date_format' => 'Y-m-d',
    'default_time_format' => 'H:i',
    'default_decimal_seperator' => '.',
    'default_export_charset' => 'UTF-8',
    'default_language' => 'en_us',
    'default_locale_name_format' => 's f l',
    'default_number_grouping_seperator' => ',',
    'export_delimiter' => ',',
    'setup_site_sugarbeet_automatic_checks' => 1,
);

// Write config to file
$config_string = "<?php\n";
foreach($config as $key => $value) {
    $config_string .= "\$_SESSION['setup_" . $key . "'] = '" . $value . "';\n";
}

// Create a silent install config
$silent_install_config = "<?php
\$silent_install_admin_user_name = 'admin';
\$silent_install_admin_password = 'admin123';
\$silent_install_database_type = 'mysql';
\$silent_install_database_host = 'mysql';
\$silent_install_database_port = '3306';
\$silent_install_database_name = 'suitecrm';
\$silent_install_database_username = 'suitecrm';
\$silent_install_database_password = 'suitecrm_pass';
\$silent_install_database_admin_username = 'root';
\$silent_install_database_admin_password = 'suitecrm_root';
\$silent_install_drop_database = false;
\$silent_install_system_name = 'SuiteCRM';
\$silent_install_site_url = 'http://localhost:8082';
\$silent_install_license_acceptance = true;
\$silent_install_locale = 'en_us';
\$silent_install_demo_data = true;
";

file_put_contents('config_si.php', $silent_install_config);

echo "Auto-install configuration created. Run install with: php -r \"require('install.php');\"";
?>