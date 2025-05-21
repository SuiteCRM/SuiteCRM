<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('include/utils.php');
require_once('include/export_utils.php');

/*
 * The entry point is registered by
 * custom/include/MVC/Controller/entry_point_registry.php
 */

if (!empty($_REQUEST['cron'])) {
    require_once('modules/jjwg_Maps/jjwg_Maps.php');
    require_once('modules/jjwg_Maps/controller.php');
    /*
     * This script can be used as an entry point for a cron
     * job to run the address geocoding on a regular basis.
     * index.php?module=jjwg_Maps&entryPoint=jjwg_Maps&cron=1&limit=2500
     */
    $controller = new jjwg_MapsController();
    $controller->action_geocode_addresses();
  
    exit;
} else {
  
  /*
   * This script is also used to pass selected records from
   * a module list view to the Maps Module (jjwg_Maps).
   *
   * Multiple records are posted thru 'uid' (comma separated) or
   * 'current_post' (see export_utils.php)
   *
   * A Javascript post method is required here as the
   * parameters are sometimes too long for a get method
   *
   * Search Types
   * 1.) Default - All Records - Map
   *     Uses 'current_post' parameter to define search
   * 2.) Default - Checked Records or This Page - Map
   *     Uses 'uid'(s) parameter to define search
   * 3.) Parameter Search - Select All - Map
   *     Uses 'current_post'
   * 4.) Parameter Search - Checked Records - Map
   *     Uses 'uid'(s) parameter to define search
   */
    //echo 'Test:<pre>'."\n";
    //foreach (array_keys($_REQUEST) as $key) {
    //  echo 'Name: '.htmlspecialchars($key).'  Value: '.htmlspecialchars($_REQUEST[$key])."\n";
    //}
  
    // Redirect parameters to view/action using Javascript form post.
    echo '<html><head></head><body>';
    echo '<form name="redirect" action="index.php" method="POST">'."\n";
    echo '<input type="hidden" name="module" value="jjwg_Maps">'."\n";
    echo '<input type="hidden" name="action" value="map_display">'."\n";
    foreach (array_keys($_REQUEST) as $key) {
        if (!in_array($key, array('action','module','entryPoint','display_module', 'quick_address'))) {
            echo '<input type="hidden" name="'.htmlspecialchars($key).'" value="'.htmlspecialchars((string) $_REQUEST[$key]).'">'."\n";
        }
    }
    echo '<input type="hidden" name="display_module" value="'.htmlspecialchars((string) $_REQUEST['display_module']).'">'."\n";
    echo '</form>'."\n";
    echo '<script language="javascript" type="text/javascript">document.redirect.submit();</script>'."\n";
    echo '</body></html>';
  
    exit;
}
