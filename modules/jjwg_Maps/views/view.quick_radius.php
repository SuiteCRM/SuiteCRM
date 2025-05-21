<?php

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
class Jjwg_MapsViewQuick_Radius extends SugarView
{
    public function __construct()
    {
        parent::__construct();
    }




    public function display()
    {
        if (!isset($_REQUEST['quick_address'])) {
            $_REQUEST['quick_address'] = '';
        } ?>

<div class="moduleTitle"><h2><?php echo $GLOBALS['mod_strings']['LBL_MAP_QUICK_RADIUS']; ?><div class="clear"></div></h2></div>
<div class="clear"></div>

<form name="quickradius" action="<?php echo $_SERVER['PHP_SELF']; ?>" method="get">
<input type="hidden" name="module" value="<?php echo $GLOBALS['currentModule']; ?>">
<input type="hidden" name="action" value="quick_radius_display" />

<table border="0">
    <tr>
        <td>
            <strong><?php echo $GLOBALS['mod_strings']['LBL_MAP_ADDRESS'].': '; ?> </strong>
        </td>
        <td>
            <input type="text" name="quick_address" id="quick_address"
            value="<?php echo htmlspecialchars((string) $_REQUEST['quick_address']); ?>" title='' tabindex='101' size="40" maxlength="255">
        </td>
    </tr>
    <tr>
        <td>
            <strong><?php echo $GLOBALS['mod_strings']['LBL_MODULE_TYPE'].' '; ?> </strong>
        </td>
        <td>
            <select id="display_module" tabindex="111" title="<?php echo $GLOBALS['mod_strings']['LBL_MODULE_TYPE']; ?>" name="display_module">
                <?php foreach ($GLOBALS['jjwg_config']['valid_geocode_modules'] as $module) {
            ?>
                    <option value="<?php echo htmlspecialchars((string) $module); ?>" <?php
                    if (!empty($_REQUEST['display_module']) && $module == $_REQUEST['display_module']) {
                        echo 'selected="selected"';
                    } ?>><?php echo htmlspecialchars((string) $GLOBALS['app_list_strings']['moduleList'][$module]); ?>
                <?php
        } ?>
            </select>
        </td>
    </tr>
    <tr>
        <td>
            <strong><?php echo $GLOBALS['mod_strings']['LBL_DISTANCE'].' '; ?> </strong>
        </td>
        <td>
            <input type="text" name="distance" id="distance"
            value="<?php echo htmlspecialchars((string) $_REQUEST['distance']); ?>" title='' tabindex='121' size="10" maxlength="10">
            <select id="unit_type" tabindex="131" title="<?php echo $GLOBALS['mod_strings']['LBL_DISTANCE']; ?>" name="unit_type">
                <?php foreach ($GLOBALS['app_list_strings']['map_unit_type_list'] as $key=>$value) {
            ?>
                    <option value="<?php echo htmlspecialchars((string) $key); ?>" <?php
                    if ($key == $_REQUEST['unit_type']) {
                        echo 'selected="selected"';
                    } ?>><?php echo htmlspecialchars((string) $value); ?>
                <?php
        } ?>
            </select>
        </td>
    </tr>
</table>
<br />
<input class="button" tabindex="211" type="submit" name="submit" value="<?php echo $GLOBALS['mod_strings']['LBL_MAP_PROCESS']; ?>" align="bottom">
</form>

<?php
    }
}
