<?php

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
class Jjwg_MapsViewGeocoded_Counts extends SugarView
{
    public function __construct()
    {
        parent::__construct();
    }




    public function display()
    {
        echo '<div class="moduleTitle"><h2>'.$GLOBALS['mod_strings']['LBL_GEOCODED_COUNTS'].'</h2><div class="clear"></div></div>';
        echo '<div class="clear"></div>';
        echo $GLOBALS['mod_strings']['LBL_GEOCODED_COUNTS_DESCRIPTION'];
        echo '<br /><br />';

        // Display last status code, if set.
        if (!empty($_REQUEST['last_status']) && preg_match('/[A-Z\_]/', (string) $_REQUEST['last_status'])) {
            echo '<div><b>'.$GLOBALS['mod_strings']['LBL_MAP_LAST_STATUS'].': '.$_REQUEST['last_status'].'</b></div>';
            echo '<br /><br />';
        }

        echo '<div class="list-view-rounded-corners">';

        echo '<table cellspacing="0" cellpadding="0" border="0" class="list view" style="width: 50% !important;">';
        echo '<thead>';
        echo '<tr><th>'.$GLOBALS['mod_strings']['LBL_MODULE_HEADING'].'</th>';
        foreach ($this->bean->geocoded_headings as $heading) {
            echo '<th>'.$GLOBALS['mod_strings']['LBL_'.$heading].'</th>';
        }
        echo '<th>'.$GLOBALS['mod_strings']['LBL_MODULE_TOTAL_HEADING'].'</th>';
        echo '<th>'.$GLOBALS['mod_strings']['LBL_MODULE_RESET_HEADING'].'</th>';
        echo '</tr>'."\n";
        echo '</thead>';

        echo '<tbody>';
        foreach ($GLOBALS['jjwg_config']['valid_geocode_modules'] as $module) {
            $geocode_url = './index.php?module=jjwg_Maps&action=geocode_addresses&display_module='.$module;
            $reset_url = './index.php?module=jjwg_Maps&action=reset_geocoding&display_module='.$module;

            echo '<tr>';
            echo '<td><strong><a href="'.htmlspecialchars($geocode_url).'">'.$GLOBALS['app_list_strings']['moduleList'][$module].'</a></strong></td>';
            foreach ($this->bean->geocoded_headings as $heading) {
                echo '<td>'.($this->bean->geocoded_counts[$module][$heading] ?? "").'</td>';
            }
            echo '<td><strong>'.($this->bean->geocoded_module_totals[$module] ?? "").'</strong></td>';
            echo '<td><strong><a href="'.htmlspecialchars($reset_url).'">'.$GLOBALS['mod_strings']['LBL_MODULE_RESET_HEADING'].'</a.</strong></td>';
            echo '</tr>'."\n";
        }

        echo '</tbody></table>';

        echo '</div>';

        echo '<br /><br />';

        // Custom Entry Point Registry:
        // $entry_point_registry['jjwg_Maps'] = array('file' => 'modules/jjwg_Maps/jjwg_Maps_Router.php', 'auth' => false);
        // Usage / Cron URL: index.php?module=jjwg_Maps&entryPoint=jjwg_Maps&cron=1

        echo '<strong>'.$GLOBALS['mod_strings']['LBL_CRON_URL'].'</strong>';
        echo '<br /><br />';
        echo $GLOBALS['mod_strings']['LBL_CRON_INSTRUCTIONS'];
        echo '<br /><br />';

        $cron_url = './index.php?module=jjwg_Maps&entryPoint=jjwg_Maps&cron=1';

        echo '<a href="'.$cron_url.'">'.$cron_url.'</a>';
        echo '<br /><br />';
        echo '<br /><br />';

        echo '<strong>'.$GLOBALS['mod_strings']['LBL_EXPORT_ADDRESS_URL'].'</strong>';
        echo '<br /><br />';

        echo $GLOBALS['mod_strings']['LBL_EXPORT_INSTRUCTIONS'];
        echo '<br /><br />';

        $export_url = './index.php?module=jjwg_Maps&action=export_geocoding_addresses&display_module=';

        echo '<a target="_blank" href="'.htmlspecialchars($export_url).$GLOBALS['app_strings']['LBL_ACCOUNTS'].'">'.$GLOBALS['app_strings']['LBL_EXPORT'].' '.$GLOBALS['app_strings']['LBL_ACCOUNTS'].'</a>';
        echo '<br /><br />';
        echo '<a target="_blank" href="'.htmlspecialchars($export_url).$GLOBALS['app_strings']['LBL_CONTACTS'].'">'.$GLOBALS['app_strings']['LBL_EXPORT'].' '.$GLOBALS['app_strings']['LBL_CONTACTS'].'</a>';
        echo '<br /><br />';
        echo '<a target="_blank" href="'.htmlspecialchars($export_url).$GLOBALS['app_strings']['LBL_LEADS'].'">'.$GLOBALS['app_strings']['LBL_EXPORT'].' '.$GLOBALS['app_strings']['LBL_LEADS'].'</a>';
        echo '<br /><br />';
        echo '<a target="_blank" href="'.htmlspecialchars($export_url).$GLOBALS['app_strings']['LBL_PROSPECTS'].'">'.$GLOBALS['app_strings']['LBL_EXPORT'].' '.$GLOBALS['app_strings']['LBL_PROSPECTS'].'</a>';
        echo '<br /><br />';

        echo '<br /><br />';
        echo '<br /><br />';
        echo '<br /><br />';

        $delete_url = './index.php?module=jjwg_Maps&action=delete_all_address_cache';
        echo '<a href="'.htmlspecialchars($delete_url).'">'.$GLOBALS['app_strings']['LBL_DELETE'].' - '.$GLOBALS['mod_strings']['LBL_ADDRESS_CACHE'].'</a>';
    }
}
