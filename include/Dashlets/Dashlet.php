<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
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

require_once('include/Sugar_Smarty.php');
require_once('include/utils/layout_utils.php');

/**
 * Basic Dashlet
 * @api
 */
#[\AllowDynamicProperties]
class Dashlet
{
    /**
     * Id of the Dashlet
     * @var guid
     */
    public $id;
    /**
     * Title of the Dashlet
     * @var string
     */
    public $title = 'Generic Dashlet';
    /**
     * true if the Dashlet has configuration options.
     * @var bool
     */
    public $isConfigurable = false;
    /**
     * true if the Dashlet is refreshable (ie charts that provide their own refresh)
     * @var bool
     */
    public $isRefreshable = true;
    /**
     * true if the Dashlet configuration options panel has the clear button
     * @var bool
     */
    public $isConfigPanelClearShown = true;
    /**
     * true if the Dashlet contains javascript
     * @var bool
     */
    public $hasScript = false;
    /**
     * Language strings, must be loaded at the Dashlet level w/ loadLanguage
     * @var array
     */
    public $dashletStrings;
    /**
     * Time period in minutes to refresh the dashlet (0 for never)
     * Do not refresh if $isRefreshable is set to false
     *
     * To support auto refresh all refreshable dashlets that override process() must call processAutoRefresh()
     * @var int
     */
    public $autoRefresh = "0";

    /**
     * Constructor
     *
     * @param $id
     */
    public function __construct($id)
    {
        $this->id = $id;
    }

    /**
     * Returns the HTML for the configure icon
     *
     * @return string HTML
     */
    public function setConfigureIcon()
    {
        if ($this->isConfigurable) {
            $additionalTitle = '<td nowrap width="1%" style="padding-right: 0px;"><div class="dashletToolSet"><a href="javascript:void(0)"  aria-label="'.translate('LBL_DASHLET_EDIT', 'Home').'" onclick="SUGAR.mySugar.configureDashlet(\''
                . $this->id . '\'); return false;">'
                . SugarThemeRegistry::current()->getImage('dashlet-header-edit', 'title="' . translate('LBL_DASHLET_EDIT', 'Home') . '" border="0"  align="absmiddle"', null, null, '.gif', translate('LBL_DASHLET_EDIT', 'Home')).'</a>'
                . '';
        } else {
            $additionalTitle = '<td nowrap width="1%" style="padding-right: 0px;"><div class="dashletToolSet">';
        }

        return $additionalTitle;
    }

    /**
     * Returns the HTML for the refresh icon
     *
     * @return string HTML
     */
    public function setRefreshIcon()
    {
        $additionalTitle = '';
        if ($this->isRefreshable) {
            $additionalTitle .= '<a href="javascript:void(0)" aria-label="'.translate('LBL_DASHLET_REFRESH', 'Home').'" onclick="SUGAR.mySugar.retrieveDashlet(\''
                . $this->id . '\'); return false;">'
                . SugarThemeRegistry::current()->getImage('dashlet-header-refresh', 'border="0" align="absmiddle" title="' . translate('LBL_DASHLET_REFRESH', 'Home') . '"', null, null, '.gif', translate('LBL_DASHLET_REFRESH', 'Home'))
                . '</a>';
        }

        return $additionalTitle;
    }

    /**
     * Returns the HTML for the delete icon
     *
     * @return string HTML
     */
    public function setDeleteIcon()
    {
        global $sugar_config;

        if (!empty($sugar_config['lock_homepage']) && $sugar_config['lock_homepage'] == true) {
            return '</div></td></tr></table>';
        }
        $additionalTitle = '<a href="javascript:void(0)" aria-label="'.translate('LBL_DASHLET_DELETE', 'Home').'" onclick="SUGAR.mySugar.deleteDashlet(\''
            . $this->id . '\'); return false;">'
            . SugarThemeRegistry::current()->getImage('dashlet-header-close', 'border="0" align="absmiddle" title="' . translate('LBL_DASHLET_DELETE', 'Home') . '"', null, null, '.gif', translate('LBL_DASHLET_DELETE', 'Home'))
            . '</a></div></td></tr></table>';
        return $additionalTitle;
    }

    /**
     * @deprecated No longer needed, replaced with Dashlet::getHeader() and Dashlet::getFooter()
     *
     * @param string $text
     * @return string HTML
     */
    public function getTitle($text = '')
    {
        return '';
    }

    /**
     * Called when Dashlet is displayed
     *
     * @param string $text text after the title
     * @return string Header html
     */
    public function getHeader($text = '')
    {
        global $sugar_config, $sugar_version, $sugar_flavor, $server_unique_key, $current_language, $current_module, $current_action, $app_strings;

        $title = '<table width="100%" cellspacing="0" cellpadding="0" border="0"><tr><td width="99%">' . $text . '</td>';
        $title .= $this->setConfigureIcon();
        $title .= $this->setRefreshIcon();
        $title .= $this->setDeleteIcon();

        $str = '<div ';
        if (empty($sugar_config['lock_homepage']) || $sugar_config['lock_homepage'] == false) {
            $str .= 'onmouseover="this.style.cursor = \'move\';" ';
        }
        $str .= 'id="dashlet_header_' . $this->id . '" class="hd"><div class="tl"></div><div class="hd-center">' . get_form_header($this->title, $title, false) . '</div><div class="tr"></div></div><div class="bd"><div class="ml"></div><div class="bd-center">';


        $blankImageURL = SugarThemeRegistry::current()->getImageURL('blank.gif');
        $printImageURL = SugarThemeRegistry::current()->getImageURL("print.gif");
        $helpImageURL  = SugarThemeRegistry::current()->getImageURL("help.gif");

        $keywords = array("/class=\"button\"/","/class='button'/","/class=button/","/<\/form>/");
        $match = false;
        foreach ($keywords as $left) {
            if (preg_match($left, $title)) {
                $match = true;
            }
        }

        $other_text_and_match = false;
        if ($title && $match) {
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
//        $template->assign('show_help', $show_help);
        $template->assign('other_text', $title);
        $template->assign('form_title', $this->title);
        $template->assign('SUGAR_CONFIG', $sugar_config);
        $template->assign('DASHLET_TITLE', $this->title);
        $template->assign('DASHLET_ID', $this->id);
        $template->assign('CONFIGURE_ICON', $this->setConfigureIcon());
        $template->assign('REFRESH_ICON', $this->setRefreshIcon());
        $template->assign('DELETE_ICON', $this->setDeleteIcon());
        $moduleName = '';
        if (!isset($this->seedBean) || !is_object($this->seedBean)) {
            $GLOBALS['log']->info('seedBean not set, or not an object, for Dashlet: ' . get_class($this));
        } else {
            $moduleName = $this->seedBean->module_name;
        }
        $template->assign('DASHLET_MODULE', $moduleName);
        $template->assign('DASHLET_BUTTON_ARIA_EDIT', translate('LBL_DASHLET_EDIT', 'Home'));
        $template->assign('DASHLET_BUTTON_ARIA_REFRESH', translate('LBL_DASHLET_REFRESH', 'Home'));
        $template->assign('DASHLET_BUTTON_ARIA_DELETE', translate('LBL_DASHLET_DELETE', 'Home'));


        $template->assign('GET_FORM_HEADER', get_form_header($this->title, $title, false));
        $template->assign('HEADER', $str);

        return $template->fetch('include/Dashlets/DashletHeader.tpl');
    }

    /**
     * Called when Dashlet is displayed
     *
     * @return string footer HTML
     */
    public function getFooter()
    {
        global $sugar_config, $sugar_version, $sugar_flavor, $server_unique_key, $current_language, $current_module, $current_action, $app_strings;

        $blankImageURL = SugarThemeRegistry::current()->getImageURL('blank.gif');
        $printImageURL = SugarThemeRegistry::current()->getImageURL("print.gif");
        $helpImageURL  = SugarThemeRegistry::current()->getImageURL("help.gif");

        $template = new Sugar_Smarty();

        $template->assign('sugar_version', $sugar_version);
        $template->assign('sugar_flavor', $sugar_flavor);
        $template->assign('server_unique_key', $server_unique_key);
        $template->assign('current_language', $current_language);
        $template->assign('current_module', $current_module);
        $template->assign('current_action', $current_action);
        $template->assign('app_strings', $app_strings);
        $template->assign('blankImageURL', $blankImageURL);
        $template->assign('printImageURL', $printImageURL);
        $template->assign('helpImageURL', $helpImageURL);
        //        $template->assign('show_help', $show_help);
        $template->assign('form_title', $this->title);
        $template->assign('SUGAR_CONFIG', $sugar_config);
        $template->assign('DASHLET_TITLE', $this->title);
        $template->assign('DASHLET_ID', $this->id);
        $template->assign('CONFIGURE_ICON', $this->setConfigureIcon());
        $template->assign('REFRESH_ICON', $this->setRefreshIcon());
        $template->assign('DELETE_ICON', $this->setDeleteIcon());

        return $template->fetch('include/Dashlets/DashletFooter.tpl');
    }

    /**
     * Called when Dashlet is displayed, override this
     *
     * @return string title HTML
     */
    public function display()
    {
        return '';
    }

    /**
     * Called when Dashlets configuration options are called
     */
    public function displayOptions()
    {
    }

    /**
     * Override if you need to do pre-processing before display is called
     */
    public function process()
    {
    }

    /**
     * Processes and displays the auto refresh code for the dashlet
     *
     * @param int $dashletOffset
     * @return string HTML code
     */
    protected function processAutoRefresh($dashletOffset = 0)
    {
        global $sugar_config;

        if (empty($dashletOffset)) {
            $dashletOffset = 0;
            $module = $_REQUEST['module'];
            if (isset($_REQUEST[$module.'2_'.strtoupper($this->seedBean->object_name).'_offset'])) {
                $dashletOffset = $_REQUEST[$module.'2_'.strtoupper($this->seedBean->object_name).'_offset'];
            }
        }

        if (!$this->isRefreshable) {
            return '';
        }
        if (!empty($sugar_config['dashlet_auto_refresh_min']) && $sugar_config['dashlet_auto_refresh_min'] == -1) {
            return '';
        }
        $autoRefreshSS = new Sugar_Smarty();
        $autoRefreshSS->assign('dashletOffset', $dashletOffset);
        $autoRefreshSS->assign('dashletId', $this->id);
        $autoRefreshSS->assign('strippedDashletId', str_replace("-", "", $this->id)); //javascript doesn't like "-" in function names
        $autoRefreshSS->assign('dashletRefreshInterval', $this->getAutoRefresh());
        $tpl = 'include/Dashlets/DashletGenericAutoRefresh.tpl';
        if ($_REQUEST['action'] == "DynamicAction") {
            $tpl = 'include/Dashlets/DashletGenericAutoRefreshDynamic.tpl';
        }

        return $autoRefreshSS->fetch($tpl);
    }

    protected function getAutoRefresh()
    {
        global $sugar_config;

        if (empty($this->autoRefresh) || $this->autoRefresh == -1) {
            $autoRefresh = 0;
        } elseif (!empty($sugar_config['dashlet_auto_refresh_min'])
            && $this->autoRefresh > 0
            && $sugar_config['dashlet_auto_refresh_min'] > $this->autoRefresh) {
            $autoRefresh = $sugar_config['dashlet_auto_refresh_min'];
        } else {
            $autoRefresh = $this->autoRefresh;
        }

        $ret = $autoRefresh * 1000;

        /**
           This number is used by setInterval() function in JS
           We should consider a limit of 2**31 -1
           https://stackoverflow.com/questions/12633405/what-is-the-maximum-delay-for-setinterval/12633556#comment78208539_12633488
         */
        if ($ret > (pow(2, 31) - 1)) {
            $ret = pow(2, 31) - 1;
            LoggerManager::getLogger()->warn(
                "The value of autoRefresh key in Dashlet: {$this->title} must be less than 2.147.483 seconds."
                ."{$autoRefresh} was configured. Using 2.147.483 seconds instead."
            );
        }

        return $ret;
    }

    /**
     * Override this if your dashlet is configurable (this is called when the configureDashlet form is shown)
     * Filters the array for only the parameters it needs to save
     *
     * @param array $req the array to pull options from
     * @return array options array
     */
    public function saveOptions($req)
    {
    }

    /**
     * Sets the language strings
     *
     * @param string $dashletClassname classname of the dashlet
     * @param string $dashletDirectory directory path of the dashlet
     */
    public function loadLanguage($dashletClassname, $dashletDirectory = 'modules/Home/Dashlets/')
    {
        global $current_language, $dashletStrings;

        if (!isset($dashletStrings[$dashletClassname])) {
            // load current language strings for current language, else default to english
            if (is_file($dashletDirectory . $dashletClassname . '/' . $dashletClassname . '.' . $current_language . '.lang.php')
                || is_file('custom/' . $dashletDirectory . $dashletClassname . '/' . $dashletClassname . '.' . $current_language . '.lang.php')) {
                if (is_file($dashletDirectory . $dashletClassname . '/' . $dashletClassname . '.' . $current_language . '.lang.php')) {
                    require($dashletDirectory . $dashletClassname . '/' . $dashletClassname . '.' . $current_language . '.lang.php');
                }
                if (is_file('custom/' . $dashletDirectory . $dashletClassname . '/' . $dashletClassname . '.' . $current_language . '.lang.php')) {
                    require('custom/' . $dashletDirectory . $dashletClassname . '/' . $dashletClassname . '.' . $current_language . '.lang.php');
                }
            } else {
                if (is_file($dashletDirectory . $dashletClassname . '/' . $dashletClassname . '.en_us.lang.php')) {
                    require($dashletDirectory . $dashletClassname . '/' . $dashletClassname . '.en_us.lang.php');
                }
                if (is_file('custom/' . $dashletDirectory . $dashletClassname . '/' . $dashletClassname . '.en_us.lang.php')) {
                    require('custom/' . $dashletDirectory . $dashletClassname . '/' . $dashletClassname . '.en_us.lang.php');
                }
            }
        }

        $this->dashletStrings = $dashletStrings[$dashletClassname];
    }

    /**
     * Generic way to store an options array into UserPreferences
     *
     * @param array $optionsArray the array to save
     */
    public function storeOptions($optionsArray)
    {
        global $current_user;

        $dashletDefs = $current_user->getPreference('dashlets', 'Home'); // load user's dashlets config
        $dashletDefs[$this->id]['options'] = $optionsArray;
        $current_user->setPreference('dashlets', $dashletDefs, 0, 'Home');
    }

    /**
     * Generic way to retrieve options array from UserPreferences
     *
     * @return array options array stored in UserPreferences
     */
    public function loadOptions()
    {
        global $current_user;

        $dashletDefs = $current_user->getPreference('dashlets', 'Home'); // load user's dashlets config
        if (isset($dashletDefs[$this->id]['options'])) {
            return $dashletDefs[$this->id]['options'];
        } else {
            return array();
        }
    }

    /**
     * Override this in the subclass. It is used to determine whether the dashlet can be displayed.
     *
     * @return bool indicating whether or not the current user has access to display this Dashlet.
     */
    public function hasAccess()
    {
        return true;
    }

    /**
     * Returns the available auto refresh settings you can set a dashlet to
     *
     * @return array options available
     */
    protected function getAutoRefreshOptions()
    {
        $options = $GLOBALS['app_list_strings']['dashlet_auto_refresh_options'];

        if (isset($GLOBALS['sugar_config']['dashlet_auto_refresh_min'])) {
            foreach ($options as $time => $desc) {
                if ($time != -1 && $time < $GLOBALS['sugar_config']['dashlet_auto_refresh_min']) {
                    unset($options[$time]);
                }
            }
        }

        return $options;
    }

    /**
     * Returns true if the dashlet is auto refreshable
     *
     * @return bool
     */
    protected function isAutoRefreshable()
    {
        return $this->isRefreshable &&
        (isset($GLOBALS['sugar_config']['dashlet_auto_refresh_min']) ?
            $GLOBALS['sugar_config']['dashlet_auto_refresh_min'] != -1 : true);
    }
}
