<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
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
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

if (!isset($install_script) || !$install_script) {
    die($mod_strings['ERR_NO_DIRECT_SCRIPT']);
}

#[\AllowDynamicProperties]
class NonDBLocalization extends Localization
{
    public function __construct()
    {
        global $sugar_config;
        $this->localeNameFormatDefault = empty($sugar_config['locale_name_format_default']) ? 's f l' : $sugar_config['default_name_format'];
    }

    /**
     * Creates dropdown items that have localized example names while filtering out invalid formats
     *
     * @override
     * @param array un-prettied dropdown list
     * @return array array of dropdown options
     */
    public function getUsableLocaleNameOptions($options)
    {
        global $mod_strings;
        $examples = array('s' => $mod_strings['LBL_LOCALE_NAME_FIRST'],
            'f' => $mod_strings['LBL_LOCALE_NAME_LAST'],
            'l' => $mod_strings['LBL_LOCALE_NAME_SALUTATION']);
        $newOpts = array();
        foreach ($options as $key => $val) {
            if ($this->isAllowedNameFormat($key) && $this->isAllowedNameFormat($val)) {
                $newVal = '';
                $pieces = str_split($val);
                foreach ($pieces as $piece) {
                    if (isset($examples[$piece])) {
                        $newVal .= $examples[$piece];
                    } else {
                        $newVal .= $piece;
                    }
                }
                $newOpts[$key] = $newVal;
            }
        }
        return $newOpts;
    }
}

#[\AllowDynamicProperties]
class InstallLayout
{
    public static function getSelect($name, $options, $default)
    {
        $out = "<select name=\"$name\">";
        foreach ($options as $key => $value) {
            $selected = '';
            if ($key==$default) {
                $selected = ' selected="selected"';
            }
            $out .= "<option label=\"$value\" value=\"$key\"$selected>$value</option>";
        }
        $out .= "</select>";
        return $out;
    }

    private function getHeaderStyles()
    {
        $out = <<<EOQ
       <link REL="SHORTCUT ICON" HREF="include/images/sugar_icon.ico">
       <link rel="stylesheet" href="install/install2.css" type="text/css">
       <link rel="stylesheet" href="themes/SuiteP/css/fontello.css">
       <link rel="stylesheet" href="themes/SuiteP/css/animation.css"><!--[if IE 7]><link rel="stylesheet" href="css/fontello-ie7.css"><![endif]-->
       <link rel='stylesheet' type='text/css' href='include/javascript/yui/build/container/assets/container.css' />
EOQ;
        return $out;
    }

    private function getHeaderScripts($sugar_version, $js_custom_version)
    {
        $out = <<<EOQ
            <script src="include/javascript/jquery/jquery-min.js"></script>
            <script src="cache/include/javascript/sugar_grp1_yui.js?s={$sugar_version}&c={$js_custom_version}"></script>
EOQ;
        return $out;
    }

    /**
     * @param $title page title
     * @param $styles linked css files (string)
     * @param $scripts linked javascript files (string)
     * @return string Install page layout header
     */
    private function getHeader($mod_strings, $styles, $scripts)
    {
        $out = <<<EOQ
    <head>
       <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
       <meta http-equiv="Content-Style-Type" content="text/css">
       <meta content='width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0' name='viewport' />
       <title>{$mod_strings['LBL_WIZARD_TITLE']} {$mod_strings['LBL_SYSOPTS_DB_TITLE']}</title>
       $styles
       $scripts
    </head>
EOQ;
        return $out;
    }


    /**
     * @param $name form tag name
     * @param $id form tag id
     * @param $errs form errors
     * @param $title form header line
     * @param $items form items (string)
     * @param $controlls form controll buttons (string)
     * @return string
     */
    private function getForm($mod_strings, $name, $id, $errs, $items, $controlls, $scripts, $next_step)
    {
        $out = <<<EOQ
            <iframe id="upload_target" name="upload_target" src="install.php?sugar_body_only=1&uploadLogoFrame=1" style="width:0;height:0;border:0px solid #fff;"></iframe>
            <form action="install.php" method="post" name="$name" id="$id" enctype="multipart/form-data">
                <input type="hidden" name="current_step" value="{$next_step}">
                <div id="install_content">
                    <div id="installoptions">
                        {$errs}
                        $items
                    </div>
                </div>
                <div class="clear"></div>
                <hr>
                <div id="installcontrols">
                    $controlls
                </div>
                <script type="text/javascript">
                    $scripts;
                </script>
            </form>
            <div id="installStatus" style="display: none;">
                <h2>{$mod_strings['LBL_INSTALL_PROCESS']}</h2>
                <p><img alt="Status" src="install/processing.gif"><br><span class="preloader-status"></span></p>
            </div>
EOQ;

        return $out;
    }


    /**
     * @param $header install page head
     * @param $form install page form step
     * @return string install page
     */
    private function getOutput($header, $form, $sugar_md, $mod_strings)
    {
        $langHeader = get_language_header();
        $out = <<<EOQ
    <!DOCTYPE HTML>
    <html {$langHeader}>
    $header
    <body onload="//document.getElementById('button_next2').focus();">
        <!--SuiteCRM installer-->
        <div id="install_container">
            <div id="install_box">
                <div id='licenseDiv'></div>
                <header id="install_header">
                    <div class="install_img">
                        <a href="https://suitecrm.com" target="_blank">
                            <img src="{$sugar_md}" alt="SuiteCRM">
                        </a>
                    </div>
                    <div id="steps">
                        <p>{$mod_strings['LBL_STEP2']}</p>
                        <i class="icon-progress-0" id="complete"></i>
                        <i class="icon-progress-1" id="complete"></i>
                        <i class="icon-progress-2"></i>
                    </div>
                </header>
                $form
            </div>

            <footer id="install_footer">
                <p id="footer_links"><a href="https://suitecrm.com" target="_blank">Visit suitecrm.com</a> | <a href="https://suitecrm.com/suitecrm/forum" target="_blank">Support Forums</a> | <a href="https://docs.suitecrm.com/admin/installation-guide/" target="_blank">Installation Guide</a> | <a href="LICENSE.txt" target="_blank">License</a>
            </footer>
        </div>
    </body>
    </html>
EOQ;
        return $out;
    }

    private function getFormItems(
        $mod_strings,
        $app_list_strings,
        $sugarConfigDefaults,
        $drivers,
        $checked,
        $db,
        $errors,
        $supportedLanguages,
        $current_language,
        $customSession,
        $customLog,
        $customId,
        $customSessionHidden,
        $customLogHidden,
        $customIdHidden
    ) {



        // ------------------------------
        //  DB Type and DB configuration
        // ---------------------------------->


        // database selection
        $out_dbtypesel = "
    <div class=\"floatbox\" id=\"fb5\">
        <h2>{$mod_strings['LBL_DBCONF_TITLE']}</h2>

        <div class=\"form_section\">
          <h3>{$mod_strings['LBL_SYSOPTS_DB']}</h3>";

        foreach ($drivers as $type => $driver) {
            $oci = ($type == "oci8")?"":'none'; // hack for special oracle message
            $out_dbtypesel.=<<<EOQ
                <input type="radio" class="checkbox" name="setup_db_type" id="setup_db_type" value="$type" {$checked[$type]} onclick="onDBTypeClick(this);//document.getElementById('ociMsg').style.display='$oci'"/>{$mod_strings[$driver->label]}<br>
EOQ;
        }
        $out_dbtypesel.=<<<EOQ
        </div>
            <div name="ociMsg" id="ociMsg" style="display:none"></div>
    <!-- </div> -->
EOQ;


        $out2 = $out_dbtypesel;


        $out2.=<<<EOQ2

        <!-- <div class="floatbox"> -->

            <div class="form_section starhook">
            <!-- <div class="required">{$mod_strings['LBL_REQUIRED']}</div> -->
            <h3>{$mod_strings['LBL_DBCONF_TITLE_NAME']}</h3>
EOQ2;

        $config_params = $db->installConfig();
        $form = '';
        foreach ($config_params as $group => $gdata) {
            $form.= "<div class='install_block'>";
            if ($mod_strings[$group . '_LABEL']) {
                $form .= "<label>{$mod_strings[$group . '_LABEL']}" . "<i> i <div class=\"tooltip\">{$mod_strings[$group]}</div></i></label>\n";
            }
            foreach ($gdata as $name => $value) {
                if (!empty($value)) {
                    if (!empty($value['required'])) {
                        $form .= "<span class=\"required\">*</span>";
                    } else {
                    }
                    if (!empty($_SESSION[$name])) {
                        $sessval = $_SESSION[$name];
                    } else {
                        $sessval = '';
                    }
                    if (!empty($value["type"])) {
                        $type = $value["type"];
                    } else {
                        $type = '';
                    }

                    $form .= <<<FORM

FORM;
                    //if the type is password, set a hidden field to capture the value.  This is so that we can properly encode special characters, which is a limitation with password fields
                    if ($type=='password') {
                        $form .= "</div><div class=\"install_block\"><label>{$mod_strings['LBL_DBCONF_TITLE_PSWD_INFO_LABEL']}</label><span>&nbsp;</span><input type='$type' name='{$name}_entry' id='{$name}_entry' value='".urldecode($sessval)."'><input type='hidden' name='$name' id='$name' value='".urldecode($sessval)."'></div><div class=\"install_block\">";
                    } else {
                        $form .= "<input type='$type' name='$name' id='$name' value='$sessval'>";
                    }



                    $form .= <<<FORM
FORM;
                } else {
                    $form .= "<input name=\"$name\" id=\"$name\" value=\"\" type=\"hidden\">\n";
                }
            }
            $form .= "</div>";
        }

        $out2 .= $form;



        // ---------- user data set (dbConfig_a.php)


        //if we are installing in custom mode, include the following html
        if ($db->supports("create_user")) {
            // create / set db user dropdown
            $auto_select = '';
            $provide_select = '';
            $create_select = '';
            $same_select = '';
            if (isset($_SESSION['dbUSRData'])) {
//    if($_SESSION['dbUSRData']=='auto')    {$auto_select ='selected';}
                if ($_SESSION['dbUSRData'] == 'provide') {
                    $provide_select = 'selected';
                }
                if (isset($_SESSION['install_type']) && !empty($_SESSION['install_type']) && strtolower($_SESSION['install_type']) == 'custom') {
                    if ($_SESSION['dbUSRData'] == 'create') {
                        $create_select = 'selected';
                    }
                }
                if ($_SESSION['dbUSRData'] == 'same') {
                    $same_select = 'selected';
                }
            } else {
                $same_select = 'selected';
            }
            $dbUSRDD = "<select name='dbUSRData' id='dbUSRData' onchange='toggleDBUser();'>";
            $dbUSRDD .= "<option value='provide' $provide_select>" . $mod_strings['LBL_DBCONFIG_PROVIDE_DD'] . "</option>";
            $dbUSRDD .= "<option value='create' $create_select>" . $mod_strings['LBL_DBCONFIG_CREATE_DD'] . "</option>";
            $dbUSRDD .= "<option value='same' $same_select>" . $mod_strings['LBL_DBCONFIG_SAME_DD'] . "</option>";
            $dbUSRDD .= "</select><br>&nbsp;";


            $setup_db_sugarsales_password = urldecode($_SESSION['setup_db_sugarsales_password']);
            $setup_db_sugarsales_user = urldecode($_SESSION['setup_db_sugarsales_user']);
            $setup_db_sugarsales_password_retype = urldecode($_SESSION['setup_db_sugarsales_password_retype']);


            $out2 .= <<<EOQ2
<br>
<div class='install_block'>
<!--
    <div class="ibmsg">{$mod_strings['LBL_DBCONFIG_SECURITY']}</div>
    -->
</div>
<div class='install_block'>
    <div class="formrow">
        <label>{$mod_strings['LBL_DBCONF_SUITE_DB_USER']}<i> i <div class="tooltip">{$mod_strings['LBL_DBCONFIG_SECURITY']}</div></i></label>
        $dbUSRDD
    </div>
    <div class="clear"></div>
    <span id='connection_user_div' style="display:none">
        <div class="formrow">
            <label>{$mod_strings['LBL_DBCONF_SUITE_DB_USER']} <span class="required">*</span></label>
            <input type="text" name="setup_db_sugarsales_user" value="{$_SESSION['setup_db_sugarsales_user']}" />
        </div>
        <div class="clear"></div>
        <div class="formrow">
            <label>{$mod_strings['LBL_DBCONF_DB_PASSWORD']}</label>
            <input type="password" name="setup_db_sugarsales_password_entry" value="{$setup_db_sugarsales_password}" />
            <input type="hidden" name="setup_db_sugarsales_password" value="{$setup_db_sugarsales_password}" />
        </div>
        <div class="clear"></div>
        <div class="formrow">
            <label>{$mod_strings['LBL_DBCONF_DB_PASSWORD2']}</label>
            <input type="password" name="setup_db_sugarsales_password_retype_entry" value="{$setup_db_sugarsales_password_retype}"  />
            <input type="hidden" name="setup_db_sugarsales_password_retype" value="{$setup_db_sugarsales_password_retype}" />
        </div>
    </span>
</div>

EOQ2;
        }
        $out =$out2;



        // ------ siteConfig_a.php
        $out .=<<<EOQ
        </div>
    </div>
    <div class="floatbox" id="fb6">
                    <h2>{$mod_strings['LBL_SITECFG_TITLE']}</h2>
                    <div class="form_section">
                    <p>{$errors}</p>
                    <div class="required">{$mod_strings['LBL_REQUIRED']}</div>

                    <h3>{$mod_strings['LBL_SITECFG_TITLE2']}<div class="tooltip-toggle"><em> i </em><div class="tooltip">{$mod_strings['LBL_SITECFG_PASSWORD_MSG']}</div></div></h3>
EOQ;
        //hide this in typical mode
        if (!empty($_SESSION['install_type']) && strtolower($_SESSION['install_type'])=='custom') {
            $out .=<<<EOQ
<div class='install_block'>
    {$mod_strings['LBL_SITECFG_URL_MSG']}
    <span class="required">*</span>
    <label><b>{$mod_strings['LBL_SITECFG_URL']}</b></label>
    <input type="text" name="setup_site_url" value="{$_SESSION['setup_site_url']}" size="40" />
    <br>{$mod_strings['LBL_SITECFG_SYS_NAME_MSG']}
    <span class="required">*</span>
    <label><b>{$mod_strings['LBL_SYSTEM_NAME']}</b></label>
    <input type="text" name="setup_system_name" value="{$_SESSION['setup_system_name']}" size="40" /><br>
</div>
EOQ;
            $db = getDbConnection();
            if ($db->supports("collation")) {
                $collationOptions = $db->getCollationList();
            }
            if (!empty($collationOptions)) {
                if (isset($_SESSION['setup_db_options']['collation'])) {
                    $default = $_SESSION['setup_db_options']['collation'];
                } else {
                    $default = $db->getDefaultCollation();
                }
                $options = get_select_options_with_id(array_combine($collationOptions, $collationOptions), $default);
                $out .=<<<EOQ
     <div class='install_block'>
        <br>{$mod_strings['LBL_SITECFG_COLLATION_MSG']}
        <span class="required">*</span>
        <label><b>{$mod_strings['LBL_COLLATION']}</b></label>
        <select name="setup_db_collation" id="setup_db_collation">$options</select><br>
     </div>
EOQ;
            }
        }

        $help_url = get_help_button_url();
        if (!isset($_SESSION['email1'])) {
            $_SESSION['email1'] = null;
        }

        if (!isset($_SESSION['setup_site_admin_user_name'])) {
            $_SESSION['setup_site_admin_user_name'] = null;
        }

        $out .=<<<EOQ
<div class='install_block'>
    <!--
    <p class="ibmsg">{$mod_strings['LBL_SITECFG_PASSWORD_MSG']}</p>
    -->
    <div class="formrow big">
        <label>{$mod_strings['LBL_SITECFG_ADMIN_Name']} <span class="required">*</span></label>
        <input type="text" name="setup_site_admin_user_name" value="{$_SESSION['setup_site_admin_user_name']}" size="20" maxlength="60" />
    </div>

    <div class="clear"></div>

    <div class="formrow big">
        <label>{$mod_strings['LBL_SITECFG_ADMIN_PASS']} <span class="required">*</span></label>
        <input type="password" name="setup_site_admin_password" value="{$_SESSION['setup_site_admin_password']}" size="20" />
    </div>

    <div class="clear"></div>

    <div class="formrow big">
        <label>{$mod_strings['LBL_SITECFG_ADMIN_PASS_2']} <span class="required">*</span></label>
        <input type="password" name="setup_site_admin_password_retype" value="{$_SESSION['setup_site_admin_password_retype']}" size="20" />
    </div>

    <div class="clear"></div>

    <div class="formrow big">
        <label>{$mod_strings['LBL_SITECFG_URL']} <span class="required">*</span></label>
        <input type="text" name="setup_site_url" value="{$_SESSION['setup_site_url']}" size="40" />
    </div>

    <div class="clear"></div>


    <div class="formrow big">
        <label>{$mod_strings['LBL_EMAIL_ADDRESS']} <span class="required">*</span></label>
        <input type="email" name="email1" value="{$_SESSION['email1']}" size="40" />
    </div>

    <div class="clear"></div>


    <div class="clear"></div>
<!--
    <a href="javascript:;" onclick="$('.security-block').toggle();">More..</a><br/><br/>
-->
EOQ;


        $out.=<<<EOQ
</div>
EOQ;

        // ------------------
        //  Choose Demo Data
        // ------------------------->


        //demo data select
        $demoDD = "<select name='demoData' id='demoData' class='select'><option value='no' >".$mod_strings['LBL_NO']."</option><option value='yes'>".$mod_strings['LBL_YES']."</option>";
        $demoDD .= "</select>";

        $out .=<<<EOQ3
        </div>
        </div>

        <div class="floatbox full" id="fb0">
            <h2>{$mod_strings['LBL_MORE_OPTIONS_TITLE']}</h2>
        </div>

        <div class="floatbox full" id="fb1">
            <div class="install_block">
                <h3 onclick="$(this).next().toggle();" class="toggler">&raquo; {$mod_strings['LBL_DBCONF_DEMO_DATA_TITLE']}</h3>

                <div class="form_section" style="display: none;">
                <div class="clear"></div>
                    <div class="formrow big">
                        <label>{$mod_strings['LBL_DBCONF_DEMO_DATA']}</label>
                        {$demoDD}
                    </div>
                </div>
            </div>
        </div>
EOQ3;

        // ------------------
        //  Choose Scenarios
        // ------------------------->
        $scenarioSelection = "<p class='ibmsg'>{$mod_strings['LBL_WIZARD_SCENARIO_EMPTY']}</p>";
        if (isset($_SESSION['installation_scenarios']) && !empty($_SESSION['installation_scenarios'])) {
            $scenarioSelection = "";
            foreach ($_SESSION['installation_scenarios'] as $scenario) {
                $key = $scenario['key'];
                $description = $scenario['description'];
                $scenarioModuleList = implode(',', $scenario['modulesScenarioDisplayName']);
                $title = $scenario['title'];

                $scenarioSelection.= "<input type='checkbox' name='scenarios[]' value='$key' checked><b>$title</b>.  $description ($scenarioModuleList).<br>";
            }
        }


        $out .= <<<EOQ

        <!-- Scenario Selection -->
        <div class="floatbox full" id="fb3">
            <h3 onclick="$(this).next().toggle();" class="toggler">&raquo; {$mod_strings['LBL_WIZARD_SCENARIO_TITLE']}</h3>
            <div class="form_section" style="display: none;">
                <p class="ibmsg">{$mod_strings['LBL_WIZARD_SCENARIO_DESC']}</p>
                <div class="formrow">$scenarioSelection</div>
                <div class="clear"></div>
            </div>
        </div>

EOQ;

        //--End of scenarios

        // db setup (dbConfig_a.php)
        $out2 =<<<EOQ2
            <input type='hidden' name='setup_db_drop_tables' id='setup_db_drop_tables' value=''>
        </div>
EOQ2;







        // ----------
        //  Branding
        // ------------->

        // company logo
        $currentLogoLink = SugarThemeRegistry::current()->getImageURL('company_logo.png');
        // show logo if we have
        $hiddenLogo = '';
        if (!file_exists($currentLogoLink)) {
            $hiddenLogo = 'display:none;';
        }


        // TODO--low: check the tooltip text at the logo image!

        $out .= <<<EOQ

            <!-- Branding  -->
        <div class="floatbox full" id="fb3">
            <h3 onclick="$(this).next().toggle();" class="toggler">&raquo; {$mod_strings['LBL_WIZARD_SYSTEM_TITLE']}</h3>

            <div class="form_section" style="display: none;">

                <p class="ibmsg">{$mod_strings['LBL_WIZARD_SYSTEM_DESC']}</p>

                <p class="ibmsg">{$mod_strings['LBL_SITECFG_SYS_NAME_MSG']}</p>

                <div class="formrow">
                    <label>{$mod_strings['SYSTEM_NAME_WIZARD']} <i>i<div class="tooltip">{$mod_strings['SYSTEM_NAME_HELP']}</div></i></label>
                    <input type="text" name="setup_system_name" size="25" maxlength="64" value="{$_SESSION['setup_system_name']}">
                </div>

                <div class="clear"></div>

                <div class="formrow">
                    <!-- file upload -->
                    <label>{$mod_strings['NEW_LOGO']} <i>i<div class="tooltip">{$mod_strings['NEW_LOGO_HELP']}</div></i></label>
                    <input type="file" name="company_logo" id="company_logo">
                </div>

                <div class="clear"></div>

                <div class="formrow">
                    <label>&nbsp;</label>
                    <input type="button" name="company_logo_upload_btn" value="{$mod_strings['COMPANY_LOGO_UPLOAD_BTN']}" onclick="onUploadImageClick(this);">
                </div>


                <div class="clear"></div>

                <div class="formrow" id="current_logo_row">
                    <label>{$mod_strings['CURRENT_LOGO']} <i>i<div class="tooltip">{$mod_strings['CURRENT_LOGO_HELP']}</div></i> </label>
                    <img height="100" src="$currentLogoLink" alt="Company Logo" style="max-height: 100px; max-width: 230px; float:left; $hiddenLogo" />
                </div>

                <div class="clear"></div>
            </div>

EOQ;






        // System location defaults

        // TODO--low: 1000s sep, Decimal Symb, Name Format

        $defaultDateFormatSelect = self::getSelect('default_date_format', $sugarConfigDefaults['date_formats'], empty($_SESSION['default_date_format']) ? $sugarConfigDefaults['default_date_format'] : $_SESSION['default_date_format']);
        $defaultTimeFormatSelect = self::getSelect('default_time_format', $sugarConfigDefaults['time_formats'], empty($_SESSION['default_time_format']) ? 'h:ia' : $_SESSION['default_time_format'] /* $sugarConfigDefaults['timef'] */);

        $timezoneSelect = self::getSelect('timezone', array_merge(array(TimeDate::guessTimezone() => TimeDate::guessTimezone()), TimeDate::getTimezoneList()), TimeDate::guessTimezone());

        //$defaultLanguageSelect = get_select_options_with_id($supportedLanguages, $current_language);
        $defaultLanguageSelect = self::getSelect('default_language', $supportedLanguages, $current_language);

        // example name formats (its are in the original language file so may this functionality was there in the original sugarcrm installer also)
        $nonDBLocalization = new NonDBLocalization();
        $sugarConfigDefaults['name_formats'] = $nonDBLocalization->getUsableLocaleNameOptions($sugarConfigDefaults['name_formats']);
        $defaultLocalNameFormatSelect = self::getSelect('default_locale_name_format', $sugarConfigDefaults['name_formats'], empty($_SESSION['default_locale_name_format']) ? $sugarConfigDefaults['default_locale_name_format'] : $_SESSION['default_locale_name_format']);

        $out .= <<<EOQ
        </div>
            <!-- System Local Settings  -->
            <!-- TODO--low: add the time-zone settings here!! -->
        <div class="floatbox full" id="fb4">
            <h3 onclick="$(this).next().toggle();" class="toggler">&raquo; {$mod_strings['LBL_LOCALE_TITLE']}</h3>

            <div class="form_section" style="display: none;">

                <p class="ibmsg">{$mod_strings['LBL_WIZARD_LOCALE_DESC']}</p>

                <div class="formrow">
                    <label>{$mod_strings['LBL_DATE_FORMAT']}</label>
                    $defaultDateFormatSelect
                </div>

                <div class="formrow">
                    <label>{$mod_strings['LBL_TIME_FORMAT']}</label>
                    $defaultTimeFormatSelect
                </div>

                <div class="clear"></div>

                <div class="formrow">
                    <label>{$mod_strings['LBL_TIMEZONE']}</label>
                    $timezoneSelect
                </div>

                <div class="clear"></div>

                <div class="formrow" style="display: none;">
                    <label>{$mod_strings['LBL_LANGUAGE']}</label>
                    $defaultLanguageSelect
                </div>

                <div class="clear"></div>

                <div class="formrow">
                    <label>{$mod_strings['LBL_CURRENCY']}</label>
                    <input type="text" name="default_currency_name" value="{$sugarConfigDefaults['default_currency_name']}">
                </div>

                <div class="formrow">
                    <label>{$mod_strings['LBL_CURRENCY_SYMBOL']}</label>
                    <input type="text" name="default_currency_symbol" size="4" value="{$sugarConfigDefaults['default_currency_symbol']}">
                </div>

                <div class="clear"></div>

                <div class="formrow">
                    <label>{$mod_strings['LBL_CURRENCY_ISO4217']}</label>
                    <input type="text" name="default_currency_iso4217" size="4" value="{$sugarConfigDefaults['default_currency_iso4217']}">
                </div>

                <!--
                <div class="formrow">
                    <label>{$mod_strings['LBL_NUMBER_GROUPING_SEP']}</label>
                    <input type="text" name="default_number_grouping_seperator" size="3" maxlength="1" value="{$sugarConfigDefaults['default_number_grouping_seperator']}">
                </div>


                <div class="formrow">
                    <label>{$mod_strings['LBL_DECIMAL_SEP']}</label>
                    <input type="text" name="default_decimal_seperator" size="3" maxlength="1" value="{$sugarConfigDefaults['default_decimal_seperator']}">
                </div>

                <div class="clear"></div>

                <div class="formrow">
                    <label>{$mod_strings['LBL_NAME_FORMAT']}</label>
                    $defaultLocalNameFormatSelect
                </div>
                -->

                <div class="clear"></div>
            </div>
        </div>

EOQ;


        $out.= "<div class=\"floatbox full\">";
        $out.= "    <h3 onclick=\"$(this).next().toggle();\" class=\"toggler\">&raquo; {$mod_strings['LBL_SITECFG_SECURITY_TITLE']}</h3>";

        $out.=<<<EOQ

<div class="security-block" style="display:none;">
<table cellspacing="0" cellpadding="0" border="0" align="center" class="shell">
      <tr><td colspan="2" id="help"><!-- <a href="{$help_url}" target='_blank'>{$mod_strings['LBL_HELP']} </a> --></td></tr>
    <tr>
      <th width="500">
   </th>
   <th width="200" style="text-align: right;">&nbsp;</th>
   </tr>
<tr>
    <td colspan="2">
    {$errors}
   <div class="required">{$mod_strings['LBL_REQUIRED']}</div>
   <table width="100%" cellpadding="0" cellpadding="0" border="0" class="StyleDottedHr">
   <tr><th colspan="3" align="left">{$mod_strings['LBL_SITECFG_SITE_SECURITY']}</td></tr>

EOQ;
        $checked = '';
        //if(!empty($_SESSION['setup_site_sugarbeet_anonymous_stats'])) $checked = 'checked=""';
        $out .= "
   <tr style='display:none'><td></td>
       <td><input type='checkbox' class='checkbox' name='setup_site_sugarbeet_anonymous_stats' value='yes' $checked /></td>
       <td><b>{$mod_strings['LBL_SITECFG_ANONSTATS']}</b><br><i>{$mod_strings['LBL_SITECFG_ANONSTATS_DIRECTIONS']}</i></td></tr>

";
        $checked = '';
        //if(!empty($_SESSION['setup_site_sugarbeet_automatic_checks'])) $checked = 'checked=""';
        $out .= <<<EOQ
   <tr style='display:none'><td></td>
       <td><input type="checkbox" class="checkbox" name="setup_site_sugarbeet_automatic_checks" value="yes" /></td>
       <td><b>{$mod_strings['LBL_SITECFG_SUITE_UP']}</b><br><i>{$mod_strings['LBL_SITECFG_SUITE_UP_DIRECTIONS']}</i><br>&nbsp;</td></tr>
   <tbody id="setup_site_session_section_pre">
   <tr><td></td>
       <td><input type="checkbox" class="checkbox" name="setup_site_custom_session_path" value="yes" onclick="javascript:$('#setup_site_session_section').toggle();" {$customSession} /></td>
       <td><b>{$mod_strings['LBL_SITECFG_CUSTOM_SESSION']}</b><br>
            <em>{$mod_strings['LBL_SITECFG_CUSTOM_SESSION_DIRECTIONS']}</em><br>&nbsp;</td>
   </tr>
   </tbody>
   <tbody id="setup_site_session_section" {$customSessionHidden}>
   <tr><td></td>
       <td style="text-align : right;"></td>
       <td align="left">
	       <div><div style="width:200px;float:left">{$mod_strings['LBL_SITECFG_SESSION_PATH']} <span class="required">*</span></div>
	               <input type="text" name="setup_site_session_path" size='40' value="{$_SESSION['setup_site_session_path']}" /></td>
	       </div>
       </td>
   </tr>
   </tbody>
   <tbody id="setup_site_log_dir_pre">
   <tr><td></td>
       <td><input type="checkbox" class="checkbox" name="setup_site_custom_log_dir" value="yes" onclick="javascript:$('#setup_site_log_dir').toggle();" {$customLog} /></td>
       <td><b>{$mod_strings['LBL_SITECFG_CUSTOM_LOG']}</b><br>
            <em>{$mod_strings['LBL_SITECFG_CUSTOM_LOG_DIRECTIONS']}</em><br>&nbsp;</td>
   </tr>
   </tbody>
   <tbody id="setup_site_log_dir" {$customLogHidden}>
   <tr><td></td>
       <td style="text-align : right;" ></td>
       <td align="left">
       <div><div style="width:200px;float:left">{$mod_strings['LBL_SITECFG_LOG_DIR']} <span class="required">*</span></div>
            <input type="text" name="setup_site_log_dir" size='30' value="{$_SESSION['setup_site_log_dir']}" />
       </div>
   </tr>
   </tbody>
   <tbody id="setup_site_guid_section_pre">
   <tr><td></td>
       <td><input type="checkbox" class="checkbox" name="setup_site_specify_guid" value="yes" onclick="javascript:$('#setup_site_guid_section').toggle();" {$customId} /></td>
       <td><b>{$mod_strings['LBL_SITECFG_CUSTOM_ID']}</b><br>
            <em>{$mod_strings['LBL_SITECFG_CUSTOM_ID_DIRECTIONS']}</em><br>&nbsp;</td>
   </tr>
   </tbody>
   <tbody id="setup_site_guid_section" {$customIdHidden}>
   <tr><td></td>
       <td style="text-align : right;"></td>
       <td align="left">
	       <div><div style="width:200px;float:left">{$mod_strings['LBL_SITECFG_APP_ID']} <span class="required">*</span></div>
	               <input type="text" name="setup_site_guid" size='30' value="{$_SESSION['setup_site_guid']}" />
	       </div>
       </td>
   </tr>
   </tbody>
</table>
</td>
</tr>
</table>
</div>
EOQ;
        $out .= "</div>";

        // --------------------------
        //  Advanced Database Configuration
        // --------------------------------->

        require_once(__DIR__ . '/suite_install/collations.php');

        $collationCB = "<select name='setup_db_collation' id='setup_db_collation' class='select' onChange='document.getElementById(\"setup_db_charset\").value = document.getElementById(\"setup_db_collation\").value.split(\"_\")[0];'>";
        $charset = "<select name='setup_db_charset' id='setup_db_charset' class='select'>";

        if (isset($collations) && isset($_SESSION['setup_db_type']) && $_SESSION['setup_db_type'] == "mysql") {
                foreach ($collations['mysql'] as $collation) {
                    $collationCB .= "<option value='" . $collation['name'] . "' >" . $collation['name'] . "</option>";
                    $charset .= "<option value='" . $collation['charset'] . "' >" . $collation['charset'] . "</option>";
                }
        }

        $collationCB .= '</select>';
        $charset .= '</select>';

        $out .= <<<EOQ3
        <div class="floatbox full" id="fb5">
          <h3 onclick="$(this).next().toggle();" class="toggler">&raquo; {$mod_strings['LBL_DBCONF_ADV_DB_CFG_TITLE']}</h3>
          <div class="form_section" style="display: none;">
            <!-- smtp settings -->
	    <br>
            <!--
            <p>{$mod_strings['LBL_WIZARD_SMTP_DESC']}</p>
            -->

	    <!-- smtp types toggler buttons -->

	    <p style="display: inline;">

	    <div>
                <div class="formrow">
                    <label>{$mod_strings['LBL_DBCONF_COLLATION']}</label>
                    {$collationCB}
                </div>
                <div class="formrow">
                    <label>{$mod_strings['LBL_DBCONF_CHARSET']}</label>
                    {$charset}
                </div>
            </div>
            <div class="clear"></div>
          </div>
        </div>

EOQ3;

        return $out;
    }

    private function getFormControlls($mod_strings, $formId)
    {
        $out =<<<EOQ
        <div id="checkingDiv" style="display:none">
                    <p><img alt="{$mod_strings['LBL_LICENSE_CHKDB_HEADER']}" src='install/processing.gif'> <br>{$mod_strings['LBL_LICENSE_CHKDB_HEADER']}</p>
           </div>
          <div id='sysCheckMsg' style="display:none">
                    <p>{$mod_strings['LBL_DROP_DB_CONFIRM']}</p>
                    <input id='accept_btn' type='button' class='button' onclick='confirm_drop_tables(true)' value="{$mod_strings['LBL_ACCEPT']}">
                    <input type='button' class='button' onclick='confirm_drop_tables(false)' id="button_cancel_dbConfig" value="{$mod_strings['LBL_CANCEL']}">
          </div>

            <div id="preloaderDiv" style="display:none">
                <p><img alt="Please wait.." src='install/processing.gif'> <br><span id="process_msg">Please wait..</span></p>
            </div>
            <div class="clear"></div>

            <div id="errorMsgs" style="display:none"></div>

            <div class="text-right">
                <input type='hidden' name='setup_db_drop_tables' id='setup_db_drop_tables' value=''>

                <input type="hidden" name="goto" id="goto">
                <input type="hidden" id="hidden_goto" name="goto" value="{$mod_strings['LBL_BACK']}" />

                <input class="button" type="button" value="{$mod_strings['LBL_BACK']}" id="button_back_settings" onclick="onBackClick(this); //document.getElementById('goto').value='{$mod_strings['LBL_BACK']}';document.getElementById('$formId').submit();" />
                <!--
                <input class="button" type="button" value="{$mod_strings['LBL_LANG_BUTTON_COMMIT']}" onclick="document.getElementById('goto').value='{$mod_strings['LBL_NEXT']}';document.getElementById('$formId').submit();" id="button_next2"/>
                -->
                <input class="button" type="button" name="goto" id="button_next2" value="{$mod_strings['LBL_NEXT']}" onClick="onNextClick(this); //callDBCheck();"/>
                <script>

                    /**
                     * Back button clicked
                     */
                    var onBackClick = function(e) {
                        removeSMTPSettings();
                        storeConfig(e, function(){
                            removeSMTPSettings();
                            // original back-submit
                            document.getElementById('goto').value='{$mod_strings['LBL_BACK']}';
                            document.getElementById('$formId').submit();
                        });
                    };


                    /**
                     * Preloader popup panel.
                     */
                    var preloaderMsgPanel;

                    /**
                     * Show a preloader popup panel.
                     */
                    var preloaderOn = function(msg, status) {
                        //$('#process_msg').html(msg);
                        //$('#preloaderDiv').show();
                        getPanel = function() {
                            var args = {    width:"300px",
                                            modal:true,
                                            fixedcenter: true,
                                            constraintoviewport: false,
                                            underlay:"shadow",
                                            close:false,
                                            draggable:true,

                                            effect:{effect:YAHOO.widget.ContainerEffect.FADE, duration:.5}
                                           } ;
                                    msg_panel = new YAHOO.widget.Panel('p_msg', args);

                                    msg_panel.setHeader(msg);
                                    status = status?status:'Please wait...';
                                    msg_panel.setBody('<p><img alt="'+status+'" src="install/processing.gif"><br><span class="preloader-status">'+status+'</span></p>');
                                    msg_panel.render(document.body);
                                    preloaderMsgPanel = msg_panel;
                            }
                            getPanel();
                            preloaderMsgPanel.show;
                    };

                    /**
                     * Popup panel hide.
                     */
                    var preloaderOff = function() {
                        //$('#process_msg').html('');
                        //$('#preloaderDiv').hide();
                        preloaderMsgPanel.hide();
                    };

                    /**
                     * Change status message in popup panel.
                     */
                    var preloaderSetStatus = function(msg) {
                        $('.preloader-status').html(msg);
                    };

                    /**
                     * Otiginal callback function for compant logo uploader, override this function to callback the upload finist event.
                     */
                    var uploadLogoCallback = function(status) {
                        // Upload finished, more details in status. (override this function)
                    };

                    /**
                     * Upload company logo in uploader-iframe.
                     */
                    var uploadLogo = function(e, cb) {
                        if(cb) {
                            uploadLogoCallback = cb;
                        }
                        $(e.form).attr('action', 'install.php?sugar_body_only=1&uploadLogo=1&callback=uploadLogoCallback')
                        $(e.form).attr('target', 'upload_target');
                        $(e.form).submit();
                    };

                    /**
                     * Store config into the server session.
                     */
                    var storeConfig = function(e, cb) {
                        var _cb = false;
                        if(cb) {
                            _cb = cb;
                        }
                        $.post('install.php?sugar_body_only=1&storeConfig=1', $(e.form).serialize(), function(resp, err){
                            if(err!=='success') {
                                console.error(err);
                            }
                            else if(resp) {
                                //console.error('configuration store failed');

                                document.getElementById("errorMsgs").innerHTML = resp;
                                document.getElementById("errorMsgs").style.display = '';
                                return false;

                            }
                            else {
                                $('#errorMsgs').html('');
                                $('#errorMsgs').hide();
                                if(_cb) {
                                    _cb();
                                }
                            }
                        });
                    };

                    /**
                     * Other SMTP settings form parts to Other tab. we want to add these to the POST request.
                     */
                    var removeSMTPSettings = function() {
//                        // on form submit prevent set the other tab div with inputs and post that!
//                        $('.smtp_tab').each(function(i,e){
//                            if($(this).attr('id') == $('input[name="smtp_tab_selected"]').val() && $(this).attr('id')!='smtp_tab_other') {
//                                var selid = '#' + $(this).attr('id') + ' ';
//                                $('input[name="mail_smtpserver"]').val( $(selid+'input[name="_mail_smtpserver"]').val() );
//                                $('#mail_smtpauth_req_chk').prop( $(selid+'input[name="_mail_smtpauth_req"]').prop() );
//                                $('input[name="mail_smtpuser"]').val( $(selid+'input[name="_mail_smtpuser"]').val() );
//                                $('input[name="mail_smtppass"]').val( $(selid+'input[name="_mail_smtppass"]').val() );
//                                $('#notify_allow_default_outbound_chk').prop( $(selid+'input[name="_notify_allow_default_outbound"]').prop() );
//                                $('input[name="mail_smtpport"]').val( $(selid+'input[name="_mail_smtpport"]').val() );
//                                $('input[name="mail_smtpssl"]').val( $(selid+'input[name="_mail_smtpssl"]').val() );
//                            }
//                        });
                    };

                    /**
                     * Show and refresh status message to user.
                     */
                    statReaderStop = false;
                    var startStatusReader = function() {
                        setInterval(function(){
                            if(!statReaderStop) {
                                $.getJSON('install/status.json?' + Math.random(), function(resp){
                                    preloaderSetStatus(resp.message);
                                    if(resp.command && resp.command.function == 'redirect') {
                                        document.location.href = resp.command.arguments;
                                        statReaderStop = true;
                                    }
                                });
                            }
                        }, 1200);
                    };

                    var dbCheckPassed = function(url, next_step, msgpanel) {
                                msgpanel.hide();
                                document.installForm.goto.value="{$mod_strings['LBL_NEXT']}";
                                document.getElementById('hidden_goto').value="{$mod_strings['LBL_NEXT']}";
                                document.installForm.current_step.value=next_step;
                                removeSMTPSettings();
                                // TODO--low: add correct form validation for all fields (number is number, server name a valid server name etc)
                                $('#installForm').attr('action', 'install.php');

                                //preloaderOn('{$mod_strings['LBL_INSTALL_PROCESS']}', '...');
                                startStatusReader();

                                $('#installForm').hide();
                                $('#installStatus').show();
                                $("html, body").animate({
                                     scrollTop:0
                                });
                                document.installForm.submit();
                    };

                    /**
                     * Starting company logo upload.
                     */
                    var onUploadImageClick = function(e) {
                        var _e = e;
                        preloaderOn('{$mod_strings['UPLOAD_LOGO']}');
                        uploadLogo(e, function(status){
                            preloaderOff();
                            // upload finish
                            $('#errorMsgs').html('');
                            $('#errorMsgs').hide();
                            if(status.errors) {
                                // show errors..
                                var errList = '';
                                $.each(status.errors, function(i,e){
                                    errList+= '<li class="error"><span class="error">' + e + '</span></li>';
                                });
                                $('#errorMsgs').html('<p><b>{$mod_strings['LBL_SYSOPTS_ERRS_TITLE']}</b></p><ul>' + errList + '</ul>');
                                $('#errorMsgs').show();

//                                storeConfig(_e);
                            }
                            else {
                                // show logo ..
                                $('#current_logo_row img').attr('src', status.filepath + '?' + Math.random());
                                var imghtml = $('<div>').append($('#current_logo_row img')).html();
                                $('#current_logo_row img').remove();
                                $('#current_logo_row').append(imghtml);
                                $('#current_logo_row img').show();
//                                storeConfig(_e, function(){
//                                    callDBCheck(function(url, next_step, msgpanel){
//                                        dbCheckPassed(url, next_step, msgpanel);
//                                    });
//                                });
                            }
                        });
                    };

                    /**
                     * Client side pre-validation.
                     */
                    var getFormErrors = function() {
                        var errors = [];

                        $('.field-error').removeClass('field-error');

                        if(!$('input[name="email1"]').val()) {
                            errors.push('{$mod_strings['ERR_ADMIN_EMAIL']}');
                            $('input[name="email1"]').addClass('field-error');
                        }

                        if(!$('input[name="setup_site_url"]').val()) {
                            errors.push('{$mod_strings['ERR_SITE_URL']}');
                            $('input[name="email1"]').addClass('field-error');
                        }

                        return errors;
                    };

                    /**
                     * Click to next button and start the installation.
                     */
                    var onNextClick = function(e) {

                        if($('#dbUSRData').val() == 'provide') {
                            $('#setup_db_admin_user_name').val($('input[name="setup_db_sugarsales_user"]').val());
                            $('#setup_db_admin_password_entry').val($('input[name="setup_db_sugarsales_password_entry"]').val());
                            $('#setup_db_admin_password').val($('input[name="setup_db_sugarsales_password_entry"]').val());
                        }

                        var errors = getFormErrors();
                        if(!errors.length) {
                            var _e = e;
                            storeConfig(_e, function(){
                                callDBCheck(function(url, next_step, msgpanel){
                                    dbCheckPassed(url, next_step, msgpanel);
                                });
                            });
                        }
                        else {
                            $('#errorMsgs').html("<p><b>Please fix the following errors before proceeding:</b></p><ul><li class=\"error\">" + errors.join('</li><li>') + "</li></ul>");
                            $('#errorMsgs').show();
                            //alert(errors.join('</li><li>'));
                        }
                        return false;
                    };
                </script>
            </div>
EOQ;
        return $out;
    }

    private function getFormScripts($mod_strings, $next_step)
    {
        $out =<<<EOQ
            /**
             * Submit form without step.
             */
            var formRefreshSubmit = function(e) {
                document.getElementById('goto').value='resend';
                document.getElementById('hidden_goto').value='resend';
                e.form.submit();
            }

            $('#fts_type').change(function(){
                if($(this).val() == '')
                    hideFTSSettings();
                else
                    showFTSSettings();
            });

            function showFTSSettings()
            {
                $('#fts_port_row').show();
                $('#fts_host_row').show();
            }

            function hideFTSSettings()
            {
                $('#fts_port_row').hide();
                $('#fts_host_row').hide();
            }


            function toggleDBUser(){
                 if(typeof(document.getElementById('dbUSRData')) !='undefined'
                 && document.getElementById('dbUSRData') != null){

                    ouv = document.getElementById('dbUSRData').value;
                    if(ouv == 'provide' || ouv == 'create'){
                        document.getElementById('connection_user_div').style.display = '';
                        //document.getElementById('sugarDBUs<br>er').style.display = 'none';

                        if(ouv == 'provide') {
                            $('#setup_db_admin_user_name').parent().prev().hide();
                            $('#setup_db_admin_user_name').parent().hide();
                            $('#setup_db_admin_password_entry').parent().hide();
                        }
                        else {
                            $('#setup_db_admin_user_name').parent().prev().show();
                            $('#setup_db_admin_user_name').parent().show();
                            $('#setup_db_admin_password_entry').parent().show();
                        }

                    }else{
                        document.getElementById('connection_user_div').style.display = 'none';
                        //document.getElementById('sugarDBUser').style.display = '';
                    }
                }
            }
            toggleDBUser();

            var msgPanel;

            // Modified: Callback function added.
            function callDBCheck(cb){
                var _cb = cb;
                        //begin main function that will be called
                        ajaxCall = function(msg_panel){
                            //create success function for callback

                            getPanel = function() {
                            var args = {    width:"300px",
                                            modal:true,
                                            fixedcenter: true,
                                            constraintoviewport: false,
                                            underlay:"shadow",
                                            close:false,
                                            draggable:true,

                                            effect:{effect:YAHOO.widget.ContainerEffect.FADE, duration:.5}
                                           } ;
                                    msg_panel = new YAHOO.widget.Panel('p_msg', args);

                                    msg_panel.setHeader("{$mod_strings['LBL_LICENSE_CHKDB_HEADER']}");
                                    msg_panel.setBody(document.getElementById("checkingDiv").innerHTML);
                                    msg_panel.render(document.body);
                                    msgPanel = msg_panel;
                            }


                            passed = function(url){
                                _cb(url, "{$next_step}", msgPanel);
                            }
                            success = function(o) {
                                //condition for just the preexisting database
                                if (o.responseText.indexOf('preexeest')>=0){

                                    //  throw confirmation message
                                    msg_panel.setBody(document.getElementById("sysCheckMsg").innerHTML);
                                    msg_panel.render(document.body);
                                    msgPanel = msg_panel;
                                    document.getElementById('accept_btn').focus();
                                //condition for no errors
                                }else if (o.responseText.indexOf('dbCheckPassed')>=0){
                                    //make navigation
                                    passed("install.php?goto={$mod_strings['LBL_NEXT']}");

                                //condition for other errors
                                }else{
                                    //turn off loading message
                                    msgPanel.hide();
                                    document.getElementById("errorMsgs").innerHTML = o.responseText;
                                    document.getElementById("errorMsgs").style.display = '';
                                    return false;
                                }


                            }//end success


                            //copy the db values over to the hidden field counterparts
                            document.installForm.setup_db_admin_password.value = document.installForm.setup_db_admin_password_entry.value;



                            //set loading message and create url
                            postData = "checkDBSettings=true&to_pdf=1&sugar_body_only=1";
                            postData += "&setup_db_database_name="+document.installForm.setup_db_database_name.value;
                            if(typeof(document.installForm.setup_db_host_instance) != 'undefined'){
                                postData += "&setup_db_host_instance="+document.installForm.setup_db_host_instance.value;
                            }
                            if(typeof(document.installForm.setup_db_port_num) != 'undefined'){
                                postData += "&setup_db_port_num="+document.installForm.setup_db_port_num.value;
                            }
                            postData += "&setup_db_host_name="+document.installForm.setup_db_host_name.value;
                            postData += "&setup_db_admin_user_name="+document.installForm.setup_db_admin_user_name.value;
			    postData += "&setup_db_admin_password="+encodeURIComponent(document.installForm.setup_db_admin_password.value);
                            postData += "&setup_db_collation="+document.installForm.setup_db_collation.value;
			    postData += "&setup_db_charset="+document.installForm.setup_db_charset.value;

                            if(typeof(document.installForm.setup_db_sugarsales_user) != 'undefined'){
                                postData += "&setup_db_sugarsales_user="+document.installForm.setup_db_sugarsales_user.value;
                            }
                            if(typeof(document.installForm.setup_db_sugarsales_password) != 'undefined'){
                            document.installForm.setup_db_sugarsales_password.value = document.installForm.setup_db_sugarsales_password_entry.value;
                                postData += "&setup_db_sugarsales_password="+encodeURIComponent(document.installForm.setup_db_sugarsales_password.value);
                            }
                            if(typeof(document.installForm.setup_db_sugarsales_password_retype) != 'undefined'){
                                document.installForm.setup_db_sugarsales_password_retype.value = document.installForm.setup_db_sugarsales_password_retype_entry.value;
                                postData += "&setup_db_sugarsales_password_retype="+encodeURIComponent(document.installForm.setup_db_sugarsales_password_retype.value);
                            }
                            if(typeof(document.installForm.dbUSRData) != 'undefined'){
                                postData += "&dbUSRData="+document.getElementById('dbUSRData').value;
                            }

                            postData += "&demoData="+document.installForm.demoData.value;

                            postData += "&to_pdf=1&sugar_body_only=1";

                            //if this is a call already in progress, then just return
                                if(typeof ajxProgress != 'undefined'){
                                    return;
                                }

                            getPanel();
                            msgPanel.show;
                            var ajxProgress = YAHOO.util.Connect.asyncRequest('POST','install.php', {success: success, failure: success}, postData);


                        };//end ajaxCall method
                          ajaxCall();
                        return;
            }

            // Modified: Show a message for user when install started.. "Installation process, pease wait..."
            function confirm_drop_tables(yes_no){

                    if(yes_no == true){
                        document.getElementById('setup_db_drop_tables').value = true;
                       //make navigation
                                document.installForm.goto.value="{$mod_strings['LBL_NEXT']}";
                                document.getElementById('hidden_goto').value="{$mod_strings['LBL_NEXT']}";
                                document.installForm.current_step.value="{$next_step}";

                                msgPanel.hide();

                                //preloaderOn('{$mod_strings['LBL_INSTALL_PROCESS']}', '...');
                                startStatusReader();

                                removeSMTPSettings();

                                $('#installForm').hide();
                                $('#installStatus').show();
                                $("html, body").animate({
                                     scrollTop:0
                                });
                                document.installForm.submit();

                    }else{
                        //set drop tables to false
                        document.getElementById('setup_db_drop_tables').value = false;
                        msgPanel.hide();
                    }
            }


                        var onDBTypeClick = function(e) {
                            formRefreshSubmit(e);
                        };
EOQ;
        return $out;
    }

    /**
     * @param $data   form data
     * @return string   output
     */
    public function show($data = null)
    {
        global $mod_strings;
        global $sugar_version;
        global $js_custom_version;
        global $app_list_strings;
        global $db;
        global $current_language;
        global $errs;
        global $sugarConfigDefaults ;
        global $drivers;
        global $checked;
        global $errors;
        global $supportedLanguages;
        global $customSession;
        global $customLog;
        global $customId;
        global $customSessionHidden;
        global $customLogHidden;
        global $customIdHidden;
        global $next_step;
        global $sugar_md;
        foreach ($data as $__key => $__val) {
            ${$__key} = $__val;
        }
        $formId = 'installForm';
        $out = $this->getOutput(
            $this->getHeader(
                $mod_strings,
                $this->getHeaderStyles(),
                $this->getHeaderScripts($sugar_version, $js_custom_version)
            ),
            $this->getForm(
                $mod_strings,
                $formId,
                $formId,
                $errs,
                $this->getFormItems(
                    $mod_strings,
                    $app_list_strings,
                    $sugarConfigDefaults,
                    $drivers,
                    $checked,
                    $db,
                    $errors,
                    $supportedLanguages,
                    $current_language,
                    $customSession,
                    $customLog,
                    $customId,
                    $customSessionHidden,
                    $customLogHidden,
                    $customIdHidden
                ),
                $this->getFormControlls($mod_strings, $formId),
                $this->getFormScripts($mod_strings, $next_step),
                $next_step
            ),
            $sugar_md,
            $mod_strings
        );
        echo $out;
        return $out;
    }
}

#[\AllowDynamicProperties]
class DisplayErrors
{
    private static $settingsStack = array();

    public static function show()
    {
        array_push(self::$settingsStack, array(
            'level' => error_reporting(),
            'display_errors' => ini_get('display_errors'),
        ));

        error_reporting(E_ALL);
        ini_set('display_errors', 1);
    }

    public static function restore()
    {
        $settings = array_pop(self::$settingsStack);
        error_reporting($settings['level']);
        ini_set('display_errors', $settings['display_errors']);
    }
}


//-------------------------------------- InstallLayout

global $sugar_version, $js_custom_version;



if (!isset($install_script) || !$install_script) {
    die($mod_strings['ERR_NO_DIRECT_SCRIPT']);
}



//---------------- systemOption: db driver select

if (!isset($_SESSION['setup_db_type']) || $_SESSION['setup_db_type'] =='') {
    $_SESSION['setup_db_type'] = 'mysql';
}
$setup_db_type = $_SESSION['setup_db_type'];

$errs = '';
if (isset($validation_errors)) {
    if ((is_countable($validation_errors) ? count($validation_errors) : 0) > 0) {
        $errs  = '<div id="errorMsgs">';
        $errs .= "<p>{$mod_strings['LBL_SYSOPTS_ERRS_TITLE']}</p>";
        $errs .= '<ul>';

        foreach ($validation_errors as $error) {
            $errs .= '<li>' . $error . '</li>';
        }

        $errs .= '</ul>';
        $errs .= '</div>';
    }
}

$drivers = DBManagerFactory::getDbDrivers();
foreach (array_keys($drivers) as $dname) {
    $checked[$dname] = '';
}
$checked[$setup_db_type] = 'checked="checked"';



//----------------- dbConfig_a: db name user pass...

if (empty($_SESSION['setup_db_host_name'])) {
    $_SESSION['setup_db_host_name'] = (isset($sugar_config['db_host_name']))  ? $sugar_config['db_host_name'] :  $_SERVER['SERVER_NAME'];
}


// DB split
$createDbCheckbox = '';
$createDb = (!empty($_SESSION['setup_db_create_database'])) ? 'checked="checked"' : '';
$dropCreate = (!empty($_SESSION['setup_db_drop_tables'])) ? 'checked="checked"' : '';
$instanceName = '';
if (isset($_SESSION['setup_db_host_instance']) && !empty($_SESSION['setup_db_host_instance'])) {
    $instanceName = $_SESSION['setup_db_host_instance'];
}

$setupDbPortNum ='';
if (isset($_SESSION['setup_db_port_num']) && !empty($_SESSION['setup_db_port_num'])) {
    $setupDbPortNum = $_SESSION['setup_db_port_num'];
}

if (!isset($_SESSION['setup_db_manager'])) {
    $_SESSION['setup_db_manager'] = null;
}

$db = getInstallDbInstance();

if(!isset($_SESSION['setup_db_collation']) || $_SESSION['setup_db_collation'] ==''){
    $_SESSION['setup_db_collation'] = 'utf8mb4_general_ci';
}

if(!isset($_SESSION['setup_db_charset']) || $_SESSION['setup_db_charset'] ==''){
    $_SESSION['setup_db_charset'] = 'utf8mb4';
}


//----------------- siteConfig_a.php Site Config & admin user


if (is_file("config.php")) {
    if (!empty($sugar_config['default_theme'])) {
        $_SESSION['site_default_theme'] = $sugar_config['default_theme'];
    }

    if (!empty($sugar_config['disable_persistent_connections'])) {
        $_SESSION['disable_persistent_connections'] =
            $sugar_config['disable_persistent_connections'];
    }
    if (!empty($sugar_config['default_language'])) {
        $_SESSION['default_language'] = $sugar_config['default_language'];
    }
    if (!empty($sugar_config['translation_string_prefix'])) {
        $_SESSION['translation_string_prefix'] = $sugar_config['translation_string_prefix'];
    }
    if (!empty($sugar_config['default_charset'])) {
        $_SESSION['default_charset'] = $sugar_config['default_charset'];
    }

    if (!empty($sugar_config['default_currency_name'])) {
        $_SESSION['default_currency_name'] = $sugar_config['default_currency_name'];
    }
    if (!empty($sugar_config['default_currency_symbol'])) {
        $_SESSION['default_currency_symbol'] = $sugar_config['default_currency_symbol'];
    }
    if (!empty($sugar_config['default_currency_iso4217'])) {
        $_SESSION['default_currency_iso4217'] = $sugar_config['default_currency_iso4217'];
    }

    if (!empty($sugar_config['rss_cache_time'])) {
        $_SESSION['rss_cache_time'] = $sugar_config['rss_cache_time'];
    }
    if (!empty($sugar_config['languages'])) {
        // We need to encode the languages in a way that can be retrieved later.
        $language_keys = array();
        $language_values = array();

        foreach ($sugar_config['languages'] as $key=>$value) {
            $language_keys[] = $key;
            $language_values[] = $value;
        }

        $_SESSION['language_keys'] = urlencode(implode(",", $language_keys));
        $_SESSION['language_values'] = urlencode(implode(",", $language_values));
    }
}

//Load in the array for the site scenarios
require_once('install/suite_install/scenarios.php');
if (isset($installation_scenarios)) {
    $_SESSION['installation_scenarios'] = $installation_scenarios;
}

////	errors
$errors = '';
if (isset($validation_errors) && is_array($validation_errors)) {
    if (count($validation_errors) > 0) {
        $errors  = '<div id="errorMsgs">';
        $errors .= '<p>'.$mod_strings['LBL_SITECFG_FIX_ERRORS'].'</p><ul>';
        foreach ($validation_errors as $error) {
            $errors .= '<li>' . $error . '</li>';
        }
        $errors .= '</ul></div>';
    }
}


////	ternaries
$sugarUpdates = (isset($_SESSION['setup_site_sugarbeet']) && !empty($_SESSION['setup_site_sugarbeet'])) ? 'checked="checked"' : '';
$siteSecurity = (isset($_SESSION['setup_site_defaults']) && !empty($_SESSION['setup_site_defaults'])) ? 'checked="checked"' : '';

$customSession = (isset($_SESSION['setup_site_custom_session_path']) && !empty($_SESSION['setup_site_custom_session_path'])) ? 'checked="checked"' : '';
$customLog = (isset($_SESSION['setup_site_custom_log_dir']) && !empty($_SESSION['setup_site_custom_log_dir'])) ? 'checked="checked"' : '';
$customId = (isset($_SESSION['setup_site_specify_guid']) && !empty($_SESSION['setup_site_specify_guid'])) ? 'checked="checked"' : '';

$customSessionHidden = (isset($_SESSION['setup_site_custom_session_path']) && !empty($_SESSION['setup_site_custom_session_path'])) ? '' : ' style="display:none;" ';
$customLogHidden = (isset($_SESSION['setup_site_custom_log_dir']) && !empty($_SESSION['setup_site_custom_log_dir'])) ? '' : ' style="display:none;" ';
$customIdHidden = (isset($_SESSION['setup_site_specify_guid']) && !empty($_SESSION['setup_site_specify_guid'])) ? '' : ' style="display:none;" ';


// defaults or user sets
// warn: may the system bring it up
$_SESSION = array_merge($_SESSION, $_POST);
$sugarConfigDefaults = array_merge(get_sugar_config_defaults(), $_SESSION);

//----- show layout

// show display errors (for testing only - do not forget restore!)
// DisplayErrors::show();

$installConfigLayout = new InstallLayout();
$installConfigLayout->show(get_defined_vars());

// restore display errors
// DisplayErrors::restore();

// TODO--low: add Name Format 	[default_locale_name_format]	[Dr. David Livingstone]	-- ???
