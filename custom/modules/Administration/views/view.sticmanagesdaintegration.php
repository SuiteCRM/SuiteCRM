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
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

class ViewSticManageSdaIntegration extends SugarView
{
    /**
     * @see SugarView::_getModuleTitleParams()
     */
    protected function _getModuleTitleParams($browserTitle = false)
    {
        global $mod_strings;

        return array(
           "<a href='index.php?module=Administration&action=index'>".$mod_strings['LBL_MODULE_NAME']."</a>",
           $mod_strings['LBL_STIC_SINERGIADA_LINK_TITLE']
           );
    }

    /**
     * @see SugarView::preDisplay()
     */
    public function preDisplay()
    {
        global $current_user;

        if (!is_admin($current_user)) {
            sugar_die("Unauthorized access to administration.");
        }
    }

    /**
     * @see SugarView::display()
     */
    public function display()
    {
        
        global $mod_strings, $current_user, $sugar_config, $app_list_strings;
        
        $this->ss->assign('MOD', $GLOBALS['mod_strings']);
        $this->ss->assign('title', $this->getModuleTitle(false));
        $this->ss->assign('CURRENT_USER_ID', $current_user->id);
        $sdaConfig = $sugar_config['stic_sinergiada'] ?? [];
        // publish_as_table may be absent when no module is selected as a table, so it is
        // normalized to an array to avoid "Undefined array key" notices in the template
        if (!is_array($sdaConfig['publish_as_table'] ?? null)) {
            $sdaConfig['publish_as_table'] = [];
        }
        $knownCacheKeys = ['cache_enabled', 'cache_units', 'cache_quantity', 'cache_hours', 'cache_minutes'];
        $defaultCache = [
            'cache_enabled' => false,
            'cache_units' => 'days',
            'cache_quantity' => 2,
            'cache_hours' => '04',
            'cache_minutes' => '30',
        ];
        if (!isset($sdaConfig['config']) || !is_array($sdaConfig['config'])) {
            $sdaConfig['config'] = $defaultCache;
        } else {
            $sdaConfig['config'] = array_merge($defaultCache, $sdaConfig['config']);
        }
        $extraConfig = [];
        foreach ($sdaConfig['config'] as $key => $value) {
            if (!in_array($key, $knownCacheKeys)) {
                $extraConfig[$key] = $value;
            }
        }
        $this->ss->assign('SDA_EXTRA_CONFIG', $extraConfig);
        $this->ss->assign('SDA_CONFIG', $sdaConfig);
        $this->ss->assign('SDA_PUBLIC_URL', $sugar_config['stic_sinergiada_public']['url'] ?? '');

        // Build module list for publish_as_table selector, replicating SinergiaDA module filtering
        include_once 'modules/MySettings/TabController.php';
        $controller = new TabController();
        $visibleModules = $controller->get_system_tabs();
        $evenExcludedModules = [
            'Administration', 'AM_ProjectTemplates', 'AOBH_BusinessHours', 'AOK_Knowledge_Base_Categories',
            'AOK_KnowledgeBase', 'AOR_Reports', 'AOR_Scheduled_Reports', 'AOS_PDF_Templates', 'AOW_WorkFlow',
            'Bugs', 'Calendar', 'DHA_PlantillasDocumentos', 'Documents', 'EmailTemplates', 'FP_events',
            'Home', 'jjwg_Address_Cache', 'jjwg_Areas', 'jjwg_Maps', 'jjwg_Markers', 'KReports',
            'ProspectLists', 'Prospects', 'ResourceCalendar', 'SecurityGroups', 'Spots',
            'stic_Bookings_Calendar', 'stic_Sepe_Files', 'stic_Settings', 'stic_Validation_Actions',
            'stic_Web_Forms', 'stic_Incorpora_Locations', 'stic_Validation_Results', 'stic_Custom_Views',
        ];
        $modulesList = array_diff($visibleModules, $evenExcludedModules);
        $modulesList['Users'] = 'Users';
        natsort($modulesList);
        $moduleOptions = [];
        foreach ($modulesList as $moduleName) {
            $txModuleName = $app_list_strings['moduleList'][$moduleName] ?? $moduleName;
            $moduleOptions[] = ['name' => $moduleName, 'label' => $txModuleName];
        }
        usort($moduleOptions, function ($a, $b) {
            return strcmp($a['label'], $b['label']);
        });
        $this->ss->assign('SDA_MODULES', $moduleOptions);

        echo '<link rel="stylesheet" type="text/css" href="include/javascript/qtip/jquery.qtip.min.css" />';
        echo '<script type="text/javascript" src="include/javascript/qtip/jquery.qtip.min.js"></script>';
        echo '<script type="text/javascript" src="SticInclude/js/SticQtip.js"></script>';

        $output = $this->ss->fetch('custom/modules/Administration/templates/SticManageSdaIntegration.tpl');
        if (!empty($output)) {
            echo $output;
        }
    }

}
