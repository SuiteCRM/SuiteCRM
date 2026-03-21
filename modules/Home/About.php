<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SuiteCRM Ltd.
 * Copyright (C) 2011 - 2025 SuiteCRM Ltd.
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
include 'suitecrm_version.php';
global $sugar_config, $mod_strings;

?>
<div class="about" id="about_header">
    <h1><img src="include/images/suite_logo.png" alt="SuiteCRM"></h1>
    <br>
    <b>
        <?php echo $mod_strings['LBL_VERSION'] . ' ' . $suitecrm_version;
        if (is_file('custom_version.php')) {
            include 'custom_version.php';
            echo '&nbsp;&nbsp;&nbsp;' . $custom_version;
        } ?>
    </b>
    <br>
    <h1><?php echo $mod_strings['LBL_CONTRIBUTOR_SUITECRM']; ?></h1>
    <table id="about_table" class="contentBox">
        <tr>
            <td valign="top" style="padding: 15px 10px 15px 10px;">
                <div class="about_suite">
                    <br>
                    <h3><?php echo $mod_strings['LBL_ABOUT_SUITE']; ?></h3>
                    <ul id="about_menu">
                        <li><?php echo $mod_strings['LBL_ABOUT_SUITE_2']; ?></li>
                        <li><?php echo $mod_strings['LBL_ABOUT_SUITE_4']; ?></li>
                        <li><?php echo $mod_strings['LBL_ABOUT_SUITE_5']; ?></li>
                    </ul>
                </div>
                <div class="about_suite">
                    <br>
                    <h3><?php echo $mod_strings['LBL_CONTRIBUTORS']; ?></h3>
                    <ul id="about_menu">
          
                        <li><?php echo $mod_strings['LBL_MAINTAINER_SUITECRM']; ?> (<a
                                    href="https://suitecrm.com/about/about-us/suitecrm-ltd"
                                    target="_blank">https://suitecrm.com</a>)
                        </li>
                        <li><?php echo $mod_strings['LBL_CONTRIBUTOR_SECURITY_SUITE']; ?> (<a
                                    href="https://store.suitecrm.com/addons/securitysuite?tag=suitecrm"
                                    target="_blank" rel="noreferrer">https://store.suitecrm.com/</a>)
                        </li>
                        <li><?php echo $mod_strings['LBL_CONTRIBUTOR_JJW_GMAPS']; ?>
                        </li>
                        <li><?php echo $mod_strings['LBL_CONTRIBUTOR_CONSCIOUS']; ?> (<a
                                    href="http://www.conscious.co.uk" target="_blank" rel="noreferrer">https://www.conscious.co.uk</a>)
                        </li>
                        <li><?php echo $mod_strings['LBL_CONTRIBUTOR_RESPONSETAP']; ?> (<a
                                    href="https://www.responsetap.com" target="_blank" rel="noreferrer">https://www.responsetap.com</a>)
                        </li>
                        <li><?php echo $mod_strings['LBL_SOURCE_SUGAR']; ?> (<a href="http://www.sugarcrm.com"
                                                                                target="_blank" rel="noreferrer">https://www.sugarcrm.com</a>)
                        </li>
                        <li><?php echo $mod_strings['LBL_CONTRIBUTOR_GMBH']; ?>
                        </li>
                    </ul>
                </div>

               <div class="about_suite">
                    <br>
                    <h3><?php echo $mod_strings['LBL_LANGUAGE_ABOUT']; ?></h3>
                    <ul id="about_menu">
                        <li><?php echo $mod_strings['LBL_LANGUAGE_COMMUNITY_ABOUT']; ?>
                        </li>
                        <li><?php echo $mod_strings['LBL_LANGUAGE_COMMUNITY_PACKS']; ?> (<a
                                href="https://crowdin.com/project/suitecrmtranslations" target="_blank" rel="noreferrer">https://crowdin.com/project/suitecrmtranslations</a>)
                        </li>
                    </ul>
                </div>

                <div class="about_suite">
                    <br>
                    <h3><?php echo $mod_strings['LBL_PARTNERS']; ?></h3>
                    <ul id="about_menu">
                        <li><?php echo $mod_strings['LBL_SUITE_PARTNERS']; ?> (<a
                                    href="https://suitecrm.com/about/about-us/partners">https://suitecrm.com</a>)
                        </li>
                    </ul>
                </div>
        </tr>
    </table>
</div>