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

require_once 'modules/Administration/controller.php';
class CustomAdministrationController extends AdministrationController
{

    /**
     * Populate the database with example records. All records are assigned and created by the fake user
     * "SinergiaCRM-TEST", which is also created with a special id (9) that will be used
     * in case of later deletion.
     *
     * @return void
     */
    public function action_insertSticData()
    {

        global $mod_strings;

        $db = DBManagerFactory::getInstance();

        // Load from an external file the queries to run.
        $sqlPopulate = file_get_contents('SticInclude/data/InsertSticData.sql');

        // As DBManagerFactory does not allow more than one SQL sentence in the same query,
        // will execute them in a loop.
        $sqlPopulate = explode('REPLACE INTO', $sqlPopulate);

        $dbErrors = '';

        foreach ($sqlPopulate as $key => $value) {
            if (empty(trim($value))) {
                continue;
            }
            $sql = 'REPLACE INTO ' . $value;
            $db->query($sql);
            $dbErrors .= $db->last_error;
        }

        if (empty($dbErrors)) {
            SugarApplication::appendErrorMessage('<div class="alert alert-success">' . $mod_strings['LBL_STIC_TEST_DATA_INSERT_SUCCESS'] . '</div>');
        } else {
            SugarApplication::appendErrorMessage('<div class="alert alert-danger">' . $mod_strings['LBL_STIC_TEST_DATA_INSERT_ERROR'] . '</div>');
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ': Error populating data: ' . $dbErrors);

        }
        SugarApplication::redirect('index.php?module=Administration&action=SticManageTestData');
    }

    /**
     * Remove test records (those created by user with id 9) and the fake user.
     *
     * @return void
     */
    public function action_removeSticData($showMessage = true)
    {
        global $mod_strings;
        $removeId = '9';
        $db = DBManagerFactory::getInstance();

        // Build an array with the database tables to be cleaned
        $tableListResult = $db->query("SELECT table_name FROM information_schema.COLUMNS where table_schema = database() and COLUMN_NAME = 'created_by';");

        $dbErrors = '';

        while ($row = $db->fetchByAssoc($tableListResult)) {
            $table = $row['table_name'];

            // 1) Remove main table records
            $db->query("DELETE FROM {$table} WHERE created_by='{$removeId}';");
            $dbErrors .= $db->last_error;

            // 2) Remove orphan record in _cstm table, if exists
            $cstmTableExists = $db->getOne("SELECT count(*) FROM information_schema.TABLES where table_schema=database() and TABLE_NAME='{$table}_cstm'");
            if ($cstmTableExists == 1) {
                $db->query("DELETE FROM `{$table}_cstm` WHERE id_c NOT IN (SELECT id FROM {$table});");
                $dbErrors .= $db->last_error;
            }
        }

        // Delete user with $removeId
        $db->query("DELETE FROM users WHERE id = '{$removeId}';");
        $dbErrors .= $db->last_error;

        if ($showMessage == true) {
            if (empty($dbErrors)) {
                SugarApplication::appendErrorMessage('<div class="alert alert-success">' . $mod_strings['LBL_STIC_TEST_DATA_REMOVE_SUCCESS'] . '</div>');
            } else {
                SugarApplication::appendErrorMessage('<div class="alert alert-danger">' . $mod_strings['LBL_STIC_TEST_DATA_REMOVE_ERROR'] . '</div>');
                $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ': Error removing test data: ' . $dbErrors);

            }
            SugarApplication::redirect('index.php?module=Administration&action=SticManageTestData');
        }
    }

    public function action_createReportingMySQLViews()
    {
        global $sugar_config;
        $sdaEnabled = $sugar_config['stic_sinergiada']['enabled'] ?? false;

        if (empty($sdaEnabled) || !$sdaEnabled) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "SinergiaDA is disabled");
            die("Sinergia Data Analytics is disabled.");
            return;
        }

        global $current_user, $mod_strings;
        if (is_admin($current_user)) {
            require_once 'SticInclude/SinergiaDARebuild.php';
            $res = SinergiaDARebuild::rebuild(true, $_REQUEST['rebuild_filter'] ?? 'all');

            if ($res != 'ok') {
                $tx = "<script>$(window).on('load', function() {
                    $('#rebuild-feedback').html('<div class=\"container alert alert-danger\"> <div class=\"col-md-1\"><span style=\"font-size:xx-large\" class=\"col-md-1 glyphicon glyphicon-minus-sign center\"></span></div> <strong>{$mod_strings['LBL_STIC_RUN_SDA_ERROR_MSG']}:</strong><p>{$res}</p></div>');
                });</script>";
            } else {
                $tx = "<script>$(window).on('load', function() {
                    $('#rebuild-feedback').html('<div class=\"container alert alert-success\"> <div class=\"col-md-1\"><span style=\"font-size:xx-large\" class=\"col-md-1 glyphicon glyphicon-check center\"></span></div><div class=\"col-md-11\"><strong>{$mod_strings['LBL_STIC_RUN_SDA_SUCCESS_MSG']}</strong></div></div>');
                });</script>";
            }

            SugarApplication::appendSuccessMessage($tx);
            SugarApplication::redirect("index.php?module=Administration&action=sticmanagesdaintegration");

            die();
        } else {
            die('<h1>Operación restringida a administradores</h1>');

        }

    }


    public function action_sticSaveSdaConfig()
    {
        global $current_user, $mod_strings, $sugar_config;

        if (!is_admin($current_user)) {
            sugar_die("Unauthorized access to administration.");
        }

        if ($_SERVER['REQUEST_METHOD'] !== 'POST') {
            SugarApplication::redirect("index.php?module=Administration&action=sticmanagesdaintegration");
            return;
        }

        require_once 'modules/Configurator/Configurator.php';
        require_once 'include/utils/array_utils.php';
        $configurator = new Configurator();

        $oldConfig = $sugar_config['stic_sinergiada'] ?? [];
        $oldPublicUrl = $sugar_config['stic_sinergiada_public']['url'] ?? '';

        // General settings
        $configurator->config['stic_sinergiada']['enabled'] = !empty($_POST['enabled']);
        $configurator->config['stic_sinergiada']['group_permissions_enabled'] = !empty($_POST['group_permissions_enabled']);
        $configurator->config['stic_sinergiada']['auto_rebuild_on_studio_events'] = !empty($_POST['auto_rebuild_on_studio_events']);
        $configurator->config['stic_sinergiada']['max_users_processed'] = ($_POST['max_users_processed'] ?? '') !== '' ? (int) $_POST['max_users_processed'] : '';

        // publish_as_table: handle modules selected via multi-select.
        // Always store an array (empty array when nothing is selected), so the value is
        // consistent with how SinergiaDA consumes it.
        $publish = $_POST['publish_as_table'] ?? [];
        if (!is_array($publish)) {
            $publish = [];
        }
        $configurator->config['stic_sinergiada']['publish_as_table'] = array_values($publish);

        // Cache settings (known fields)
        $knownCacheKeys = ['cache_enabled', 'cache_units', 'cache_quantity', 'cache_hours', 'cache_minutes'];
        $cacheUnits = $_POST['cache_units'] ?? '';
        $configurator->config['stic_sinergiada']['config']['cache_enabled'] = !empty($_POST['cache_enabled']);
        $configurator->config['stic_sinergiada']['config']['cache_units'] = in_array($cacheUnits, ['days', 'hours'], true) ? $cacheUnits : 'days';
        $configurator->config['stic_sinergiada']['config']['cache_quantity'] = ($_POST['cache_quantity'] ?? '') !== '' ? (int) $_POST['cache_quantity'] : '';
        $configurator->config['stic_sinergiada']['config']['cache_hours'] = str_pad((string) max(0, min(23, (int) ($_POST['cache_hours'] ?? 0))), 2, '0', STR_PAD_LEFT);
        $configurator->config['stic_sinergiada']['config']['cache_minutes'] = str_pad((string) max(0, min(59, (int) ($_POST['cache_minutes'] ?? 0))), 2, '0', STR_PAD_LEFT);

        // Extra / unknown config keys (from "Otras configuraciones" section)
        $extraConfig = $_POST['extra_config'] ?? [];
        if (is_array($extraConfig)) {
            foreach ($extraConfig as $ekey => $evalue) {
                if (!in_array($ekey, $knownCacheKeys)) {
                    $configurator->config['stic_sinergiada']['config'][$ekey] = $evalue;
                }
            }
        }

        // Public URL
        // Only persist the URL when it has a value. If left empty, the key is removed so that
        // no empty line is written to config_override.php and a URL defined elsewhere (config.php,
        // manual override, etc.) is not silently overwritten.
        $publicUrl = trim($_POST['public_url'] ?? '');
        if ($publicUrl !== '') {
            $configurator->config['stic_sinergiada_public']['url'] = $publicUrl;
        } else {
            unset($configurator->config['stic_sinergiada_public']['url']);
        }

        // Log changes
        $userId = $current_user->id;
        $changes = [];
        $newConfig = $configurator->config['stic_sinergiada'];
        foreach ($newConfig as $key => $newVal) {
            $oldVal = $oldConfig[$key] ?? '';
            if (json_encode($oldVal) !== json_encode($newVal)) {
                $changes[] = "$key: " . json_encode($oldVal) . " -> " . json_encode($newVal);
            }
        }
        $newPublicUrl = $configurator->config['stic_sinergiada_public']['url'] ?? '';
        if ($oldPublicUrl !== $newPublicUrl) {
            $changes[] = "public_url: $oldPublicUrl -> $newPublicUrl";
        }

        try {
            // Read existing override, then only update our specific keys
            $overrideArray = $configurator->readOverride();
            $sdaConfig = $configurator->config['stic_sinergiada'];
            $sdaPublicConfig = $configurator->config['stic_sinergiada_public'] ?? [];
            $overrideArray['stic_sinergiada'] = $sdaConfig;
            $overrideArray['stic_sinergiada_public'] = $sdaPublicConfig;
            $overrideString = "<?php\n/***CONFIGURATOR***/\n";
            foreach ($overrideArray as $key => $val) {
                $overrideString .= override_value_to_string_recursive2('sugar_config', $key, $val);
            }
            $overrideString .= '/***CONFIGURATOR***/';
            $configurator->saveOverride($overrideString);

            if (!empty($changes)) {
                $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "User $userId changed SinergiaDA config: " . implode('; ', $changes));
            } else {
                $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "User $userId saved SinergiaDA config (no changes detected)");
            }
            SugarApplication::appendErrorMessage('<div class="alert alert-success">' . $mod_strings['LBL_STIC_SINERGIADA_CONFIG_SAVE_SUCCESS'] . '</div>');
        } catch (Exception $e) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "User $userId error saving SinergiaDA config: " . $e->getMessage());
            SugarApplication::appendErrorMessage('<div class="alert alert-danger">' . $mod_strings['LBL_STIC_SINERGIADA_CONFIG_SAVE_ERROR'] . '</div>');
        }

        SugarApplication::redirect("index.php?module=Administration&action=sticmanagesdaintegration");
    }

    public function action_configureMainMenu(){
        // Add specific logic for manage main menu
        require_once('custom/modules/Administration/SticAdvancedMenu/SticAdvancedMenuEdit.php');

    } 

}
