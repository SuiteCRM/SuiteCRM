<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

global $current_user, $sugar_config;
global $mod_strings;
global $app_list_strings;
global $app_strings;
global $theme;

if (!is_admin($current_user)) {
    sugar_die("Unauthorized access to administration.");
}

require_once('modules/Configurator/Configurator.php');

echo getClassicModuleTitle(
    "Administration",
    array(
        "<a href='index.php?module=Administration&action=index'>".translate('LBL_MODULE_NAME', 'Administration')."</a>",
        $mod_strings['LBL_BUSINESS_HOURS_DESC'],
    ),
    false
);

$sugar_smarty	= new Sugar_Smarty();
$errors			= array();
$days = array('Monday' => $mod_strings['LBL_MONDAY'],'Tuesday' => $mod_strings['LBL_TUESDAY'],'Wednesday' => $mod_strings['LBL_WEDNESDAY'],'Thursday' => $mod_strings['LBL_THURSDAY'],'Friday' => $mod_strings['LBL_FRIDAY'],'Saturday' => $mod_strings['LBL_SATURDAY'],'Sunday' => $mod_strings['LBL_SUNDAY']);
$businessHours = BeanFactory::getBean("AOBH_BusinessHours");

if (isset($_REQUEST['do']) && $_REQUEST['do'] == 'save') {
    foreach ($days as $day => $label) {
        $bh = $businessHours->getOrCreate($day);
        $bh->day = $day;
        $bh->open_status = array_key_exists("open_status_".$day, $_REQUEST) ? $_REQUEST["open_status_".$day] : false;
        $bh->opening_hours = $_REQUEST["opening_time_".$day];
        $bh->closing_hours = $_REQUEST["closing_time_".$day];
        $bh->save();
    }
    SugarApplication::redirect('index.php?module=Administration&action=index');
}

$dayDropdowns = array();
foreach ($days as $day => $label) {
    $drops = array();
    $bh = $businessHours->getBusinessHoursForDay($day);
    if ($bh) {
        $bh = $bh[0];
        $drops['open_status'] = $bh->open_status;
    } else {
        $drops['open_status'] = $day != $mod_strings['LBL_SATURDAY'] && $day != $mod_strings['LBL_SUNDAY'];
    }
    $hours = get_select_options_with_id($app_list_strings['business_hours_list'], ($bh ? $bh->opening_hours : 9));
    $drops['opening'] = $hours;
    $hours = get_select_options_with_id($app_list_strings['business_hours_list'], ($bh ? $bh->closing_hours : 17));
    $drops['closing'] = $hours;

    $drops['label'] = $label;
    
    $dayDropdowns[$day] = $drops;
}
$sugar_smarty->assign('DAY_DROPDOWNS', $dayDropdowns);


$sugar_smarty->assign('MOD', $mod_strings);
$sugar_smarty->assign('APP', $app_strings);
$sugar_smarty->assign('APP_LIST', $app_list_strings);
$sugar_smarty->assign('LANGUAGES', get_languages());
$sugar_smarty->assign("JAVASCRIPT", get_set_focus_js());
$sugar_smarty->assign('error', $errors);

$buttons =  <<<EOQ
    <input title="{$app_strings['LBL_SAVE_BUTTON_TITLE']}"
                       accessKey="{$app_strings['LBL_SAVE_BUTTON_KEY']}"
                       class="button primary"
                       type="submit"
                       name="save"
                       onclick="return check_form('ConfigureSettings');"
                       value="  {$app_strings['LBL_SAVE_BUTTON_LABEL']}  " >
                &nbsp;<input title="{$mod_strings['LBL_CANCEL_BUTTON_TITLE']}"  onclick="document.location.href='index.php?module=Administration&action=index'" class="button"  type="button" name="cancel" value="  {$app_strings['LBL_CANCEL_BUTTON_LABEL']}  " >
EOQ;

$sugar_smarty->assign("BUTTONS", $buttons);

$sugar_smarty->display('modules/Administration/BusinessHours.tpl');

$javascript = new javascript();
$javascript->setFormName('ConfigureSettings');
echo $javascript->getScript();

echo <<<EOF
<script type="text/javascript">
$(document).ready(function(){
    $(".open_check").change(function(){
        var thisElem = $(this);

        var day = thisElem.data("day");
        if(thisElem.attr("checked")){
            $("#"+day+"_times").show();
        }else{
            $("#"+day+"_times").hide();
        }
    });
    $(".open_check").change();
});
</script>
EOF;
