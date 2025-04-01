<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


#[\AllowDynamicProperties]
class VersionCheck
{
    public function version_check($event, $arguments)
    {
        global $current_user;

        require_once('include/utils.php');
        if (is_admin($current_user) && empty($_REQUEST['to_pdf']) && empty($_REQUEST['sugar_body_only'])) {
            //require_once('modules/SecurityGroups/SecurityGroup.php');

            //check to see if the securitysuite version
            //matches the sugar version. If not then display an error messag

            global $sugar_config;
            if (empty($sugar_config['securitysuite_version'])
                || $sugar_config['securitysuite_version'] != $sugar_config['sugar_version']
            ) {
                $securitysuite_warning = "Warning! SecuritySuite no longer matches the version of Sugar that you are running. "
                    . "SecuritySuite will not work correctly until updated to ".$sugar_config['sugar_version'].". "
                    . "Upgrade now to  ";

                global $sugar_config;
                $upgrade_url = "http://www.eggsurplus.com/version.php?version=".$sugar_config['sugar_version'];
                $upgrade_text = "SecuritySuite for ".$sugar_config['sugar_version'];
                $GLOBALS['log']->fatal($securitysuite_warning."<a href='$upgrade_url'>$upgrade_text</a>");
                //echo $display_warning;?>
<script language="Javascript">


var oNewP = document.createElement("div");
oNewP.className = 'error';

var oText = document.createTextNode("<?php echo $securitysuite_warning; ?>");
oNewP.appendChild(oText);

var oLink = document.createElement("a");
oLink.href = '<?php echo $upgrade_url; ?>';
oLink.appendChild(document.createTextNode("<?php echo $upgrade_text; ?>"));
oNewP.appendChild(oLink);

var beforeMe = document.getElementsByTagName("div")[0];
document.body.insertBefore(oNewP, beforeMe);
</script>
<?php
            }
        } //end if admin
    }
}
?>