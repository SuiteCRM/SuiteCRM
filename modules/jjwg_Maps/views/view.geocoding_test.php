<?php

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
class Jjwg_MapsViewGeocoding_Test extends SugarView
{
    public function __construct()
    {
        parent::__construct();
    }




    public function display()
    {
        if (!isset($_REQUEST['geocoding_address'])) {
            $_REQUEST['geocoding_address'] = '';
        } ?>

<div class="moduleTitle"><h2><?php echo $GLOBALS['mod_strings']['LBL_MAP_ADDRESS_TEST']; ?></h2><div class="clear"></div></div>
<div class="clear"></div>

<form name=geocodingtest action="<?php echo $_SERVER['PHP_SELF']; ?>" method="get">
<input type="hidden" name="module" value="<?php echo $GLOBALS['currentModule']; ?>">
<input type="hidden" name="action" value="geocoding_test" />
<strong><?php echo $GLOBALS['mod_strings']['LBL_MAP_ADDRESS'].': '; ?> </strong>
<input autocomplete="off" type="text" name="geocoding_address" id="geocoding_address"
value="<?php echo htmlspecialchars((string) $_REQUEST['geocoding_address']); ?>" title='' tabindex='1' size="40" maxlength="255">
<br /><br />
<input class="button" type="submit" name="submit" value="<?php echo $GLOBALS['mod_strings']['LBL_MAP_PROCESS']; ?>" align="bottom">
<input type="hidden" name="process_trigger" value="yes">
</form>

<?php
    if (!empty($_REQUEST['process_trigger'])) {
        echo '<br /><br /><pre>';
        print_r($this->bean->geocoding_results);
        echo '</pre><br /><br />';
    }
    }
}

?>