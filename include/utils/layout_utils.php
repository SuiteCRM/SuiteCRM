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


/**
 * Contains a variety of utility functions used to display UI components such as form headers and footers.
 *
 * @todo refactor out these functions into the base UI objects as indicated
 */

/**
 * Create HTML to display formatted form title.
 *
 * @param  $form_title string to display as the title in the header
 * @param  $other_text string to next to the title.  Typically used for form buttons.
 * @param  $show_help  boolean which determines if the print and help links are shown.
 * @param  $print_out  boolean which determines if the print/echo out code
 * @return string HTML
 */
function get_form_header(
    $form_title,
    $other_text,
    $show_help,
    $print_out = false
    ) {
    global $sugar_version, $sugar_flavor, $server_unique_key, $current_language, $current_module, $current_action, $app_strings;

    $blankImageURL = SugarThemeRegistry::current()->getImageURL('blank.gif');
    $printImageURL = SugarThemeRegistry::current()->getImageURL("print.gif");
    $helpImageURL  = SugarThemeRegistry::current()->getImageURL("help.gif");

    $keywords = array("/class=\"button\"/","/class='button'/","/class=button/","/<\/form>/");
    $match = false;
    foreach ($keywords as $left) {
        if (preg_match($left, (string) $other_text)) {
            $match = true;
        }
    }

    $other_text_and_match = false;
    if ($other_text && $match) {
        $other_text_and_match = true;
    }

    $template = new Sugar_Smarty();

    $template->assign('sugar_version', $sugar_version);
    $template->assign('sugar_flavor', $sugar_flavor);
    $template->assign('server_unique_key', $server_unique_key);
    $template->assign('current_language', $current_language);
    $template->assign('current_module', $current_module);
    $template->assign('current_action', $current_action);
    $template->assign('app_strings', $app_strings);

    $template->assign('match', $match);
    $template->assign('other_text_and_match', $other_text_and_match);
    $template->assign('blankImageURL', $blankImageURL);
    $template->assign('printImageURL', $printImageURL);
    $template->assign('helpImageURL', $helpImageURL);
    $template->assign('show_help', $show_help);
    $template->assign('other_text', $other_text);
    $template->assign('form_title', $form_title);

    $template_output = $template->fetch('include/get_form_header.tpl');
    if ($print_out) {
        echo $template_output;
    }

    return $template_output;
}

/**
 * Wrapper function for the get_module_title function, which is mostly used for pre-MVC modules.
 *
 * @deprecated use SugarView::getModuleTitle() for MVC modules, or getClassicModuleTitle() for non-MVC modules
 *
 * @param  $module       string  to next to the title.  Typically used for form buttons.
 * @param  $module_title string  to display as the module title
 * @param  $show_help    boolean which determines if the print and help links are shown.
 * @return string HTML
 */
function get_module_title(
    $module,
    $module_title,
    $show_create,
    $count=0
    ) {
    global $sugar_version, $sugar_flavor, $server_unique_key, $current_language, $action;
    global $app_strings;

    $the_title = "<div class='moduleTitle'>\n";
    $module = preg_replace("/ /", "", (string) $module);
    $iconPath = "";
    if (is_file(SugarThemeRegistry::current()->getImageURL('icon_'.$module.'_32.png', false))) {
        $iconPath = SugarThemeRegistry::current()->getImageURL('icon_'.$module.'_32.png');
    } else {
        if (is_file(SugarThemeRegistry::current()->getImageURL('icon_'.ucfirst($module).'_32.png', false))) {
            $iconPath = SugarThemeRegistry::current()->getImageURL('icon_'.ucfirst($module).'_32.png');
        }
    }
    if (!empty($iconPath)) {
        $the_title .= '<h2>';
        $sw = new SugarView();
        if (SugarThemeRegistry::current()->directionality == "ltr") {
            $the_title .= "<a href='index.php?module={$module}&action=index'><img src='{$iconPath}' " . "alt='".$module."' title='".$module."' align='absmiddle'></a>";
            $the_title .= ($count >= 1) ? $sw->getBreadCrumbSymbol() : "";
            $the_title .=  $module_title.'';
        } else {
            $the_title .= $module_title;
            $the_title .= ($count > 1) ? $sw->getBreadCrumbSymbol() : "";
            $the_title .= "<a href='index.php?module={$module}&action=index'><img src='{$iconPath}' "  . "alt='".$module."' title='".$module."' align='absmiddle'></a>";
        }
        $the_title .= '</h2>';
    } else {
        $the_title .="<h2> $module_title </h2>";
    }
    $the_title .= "\n";
    
    if ($show_create) {
        $the_title .= "<span class='utils'>";
        $createRecordURL = SugarThemeRegistry::current()->getImageURL('create-record.gif');
        $the_title .= <<<EOHTML
&nbsp;
<a href="index.php?module={$module}&action=EditView&return_module={$module}&return_action=DetailView" class="utilsLink">
<img src='{$createRecordURL}' alt='{$GLOBALS['app_strings']['LNK_CREATE']}'></a>
<a href="index.php?module={$module}&action=EditView&return_module={$module}&return_action=DetailView" class="utilsLink">
{$GLOBALS['app_strings']['LNK_CREATE']}
</a>
EOHTML;

        $the_title .= '</span>';
    }

    $the_title .= "</div>\n";
    return $the_title;
}

/**
 * Handles displaying the header for classic view modules
 *
 * @param $module String value of the module to create the title section for
 * @param $params Array of arguments used to create the title label.  Typically this is just the current language string label for the section
 * These should be in the form of array('label' => '<THE LABEL>', 'link' => '<HREF TO LINK TO>');
 * the first breadcrumb should be index at 0, and built from there e.g.
 * <code>
 * array(
 *    '<a href="index.php?module=Contacts&amp;action=index">Contacts</a>',
 *    '<a href="index.php?module=Contacts&amp;action=DetailView&amp;record=123">John Smith</a>',
 *    'Edit',
 *    );
 * </code>
 * would display as:
 * <a href='index.php?module=Contacts&amp;action=index'>Contacts</a> >> <a href='index.php?module=Contacts&amp;action=DetailView&amp;record=123'>John Smith</a> >> Edit
 * @param $show_create boolean flag indicating whether or not to display the create link (defaults to false)
 * @param $index_url_override String value of url to override for module index link (defaults to module's index action if none supplied)
 * @param $create_url_override String value of url to override for module create link (defaults to EditView action if none supplied)
 *
 * @return String HTML content for a classic module title section
 */
function getClassicModuleTitle($module, $params, $show_create=false, $index_url_override='', $create_url_override='')
{
    global $sugar_version, $sugar_flavor, $server_unique_key, $current_language, $action;
    global $app_strings;

    $module_title = '';
    $index = 0;

    $module = preg_replace("/ /", "", (string) $module);
    $iconPath = "";
    $the_title = "<div class='moduleTitle'>\n";

    if (!empty($module)) {
        if (is_file(SugarThemeRegistry::current()->getImageURL('icon_' . $module . '_32.png', false))) {
            $iconPath = SugarThemeRegistry::current()->getImageURL('icon_' . $module . '_32.png');
        } elseif (is_file(SugarThemeRegistry::current()->getImageURL('icon_' . ucfirst($module) . '_32.png', false))) {
            $iconPath = SugarThemeRegistry::current()->getImageURL('icon_' . ucfirst($module) . '_32.png');
        }
    }

    if (!empty($iconPath)) {
        $url = (!empty($index_url_override)) ? $index_url_override : "index.php?module={$module}&action=index";
        array_unshift($params, "<a href='{$url}'><img src='{$iconPath}' ". "alt='".$module."' title='".$module."' align='absmiddle'></a>");
    }

    $new_params = array_pop($params);
    if (!is_null($new_params) && ($new_params !== "")) {
        $module_title = $new_params;
    }
    if (!empty($module_title)) {
        $the_title .= "<h2>".$module_title."</h2>\n";//removing empty H2 tag for 508 compliance
    }


    if ($show_create) {
        $the_title .= "<span class='utils'>";
        $createRecordImage = SugarThemeRegistry::current()->getImageURL('create-record.gif');
        if (empty($create_url_override)) {
            $create_url_override = "index.php?module={$module}&action=EditView&return_module={$module}&return_action=DetailView";
        }

        $the_title .= <<<EOHTML
&nbsp;
<a href="{$create_url_override}" class="utilsLink" id="create_link">
{$GLOBALS['app_strings']['LNK_CREATE']}
</a>
EOHTML;

        $the_title .= '</span>';
    }

    $the_title .= "<div class='clear'></div></div>\n";
    return $the_title;
}

/**
 * Create a header for a popup.
 *
 * @todo refactor this into the base Popup_Picker class
 *
 * @param  $theme string the name of the current theme, ignorred to use SugarThemeRegistry::current() instead.
 * @return string HTML
 */
function insert_popup_header($theme = null, $includeJS = true)
{
    global $app_strings, $sugar_config;

    $themeCSS = SugarThemeRegistry::current()->getCSS();

    $langHeader = get_language_header();

    //The SugarView will insert the header now, this function should no longer do the actual head element.
    if ($includeJS) {
        echo <<<EOHTML
<!DOCTYPE HTML>
<html {$langHeader}>
<head>
EOHTML;
    }

    if (isset($sugar_config['meta_tags']) && isset($sugar_config['meta_tags']['IE_COMPAT_MODE'])) {
        echo $sugar_config['meta_tags']['IE_COMPAT_MODE'];
    }

    echo "<title>{$app_strings['LBL_BROWSER_TITLE']}</title>";
    echo '<link href="themes/'.SugarThemeRegistry::current().'/css/bootstrap.min.css" rel="stylesheet">';
    echo $themeCSS;
    if ($includeJS) {
        $charset = isset($app_strings['LBL_CHARSET']) ? $app_strings['LBL_CHARSET'] : $sugar_config['default_charset'];
        echo '<meta http-equiv="Content-Type" content="text/html; charset="{$charset}">';
        echo '<script type="text/javascript" src="' . getJSPath('cache/include/javascript/sugar_grp1_jquery.js') . '"></script>';
        echo '<script type="text/javascript" src="' . getJSPath('cache/include/javascript/sugar_grp1_yui.js') . '"></script>';
        echo '<script type="text/javascript" src="' . getJSPath('cache/include/javascript/sugar_grp1.js') . '"></script>';
    }
    /* Fix to include files required to make pop-ups responsive */
    echo '<meta http-equiv="X-UA-Compatible" content="IE=edge">';
    echo '<meta name="viewport" content="initial-scale=1.0, user-scalable=no" />';

    echo '</head>';
    echo  '<body class="popupBody">';
}

/**
 * Create a footer for a popup.
 *
 * @todo refactor this into the base Popup_Picker class
 *
 * @return string HTML
 */
function insert_popup_footer()
{
    echo <<<EOQ
</body>
</html>
EOQ;
}
