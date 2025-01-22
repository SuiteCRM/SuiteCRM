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


/**
 * Connector's HTML helper
 * @api
 */
#[\AllowDynamicProperties]
class ConnectorHtmlHelper
{
    /**
     * Method return the HTML code for the hover link field
     *
     * @param array $shown_sources
     * @param mixed $module
     * @param mixed $smarty
     * @return string
     */
    public function getConnectorButtonCode(array $shown_sources, $module, $smarty)
    {
        require_once('include/connectors/formatters/FormatterFactory.php');

        return $this->getButton($shown_sources, $module, $smarty);
    }

    /**
     * Get button for source
     *
     * @param string $shown_source
     * @param mixed $module
     * @param mixed $smarty
     * @return string
     */
    private function getButton(array $shown_sources, $module, $smarty)
    {
        $code = '';

        foreach ($shown_sources as $id) {
            $formatter = FormatterFactory::getInstance($id);
            $formatter->setModule($module);
            $formatter->setSmarty($smarty);
            $formatter_code = $formatter->getDetailViewFormat();
            if (!empty($formatter_code)) {
                $iconFilePath = $formatter->getIconFilePath();
                $iconFilePath = empty($iconFilePath) ? 'themes/default/images/MoreDetail.png' : $iconFilePath;


                $code .= '<!--not_in_theme!--><img id="dswidget_img" border="0" src="' . $iconFilePath .'" alt="'
                         . $id .'" onclick="show_' . $id . '(event);">';

                $code .= "<script type='text/javascript' src='{sugar_getjspath file='include/connectors/formatters/default/company_detail.js'}'></script>";
                //$code .= $formatter->getDetailViewFormat();
                $code .= $formatter_code;
            }
        }

        return $code;
    }

    /**
     * Get popup for sources
     *
     * @param array $shown_sources
     * @param mixed $module
     * @param mixed $smarty
     * @return string
     */
    private function getPopup(array $shown_sources, $module, $smarty)
    {
        global $app_strings;

        $code = '';
        $menuParams = 'var menuParams = "';
        $formatterCode = '';
        $sourcesDisplayed = 0;
        $singleIcon = '';
        foreach ($shown_sources as $id) {
            $formatter = FormatterFactory::getInstance($id);
            $formatter->setModule($module);
            $formatter->setSmarty($smarty);
            $buttonCode = $formatter->getDetailViewFormat();
            if (!empty($buttonCode)) {
                $sourcesDisplayed++;
                $singleIcon = $formatter->getIconFilePath();
                $source = SourceFactory::getSource($id);
                $config = $source->getConfig();
                $name = !empty($config['name']) ? $config['name'] : $id;
                //Create the menu item to call show_[source id] method in javascript
                $menuParams .= '<a href=\'#\' style=\'width:150px\' class=\'menuItem\' onmouseover=\'hiliteItem(this,\"yes\");\''
                            . ' onmouseout=\'unhiliteItem(this);\' onclick=\'show_' . $id . '(event);\'>' . $name . '</a>';
                $formatterCode .= $buttonCode;
            }
        } //for

        if (!empty($formatterCode)) {
            if ($sourcesDisplayed > 1) {
                $dswidget_img = SugarThemeRegistry::current()->getImageURL('MoreDetail.png');
                $code = '<!--not_in_theme!--><img id="dswidget_img" src="' . $dswidget_img . '" width="11" height="7" border="0" alt="'
                        . $app_strings['LBL_CONNECTORS_POPUPS'] . '" onclick="return showConnectorMenu2(this);">';
            } else {
                $dswidget_img = SugarThemeRegistry::current()->getImageURL('MoreDetail.png');
                $singleIcon = empty($singleIcon) ? $dswidget_img : $singleIcon;
                $code = '<!--not_in_theme!--><img id="dswidget_img" border="0" src="' . $singleIcon . '" alt="'.$app_strings['LBL_CONNECTORS_POPUPS']
                        . '" onclick="return showConnectorMenu2(this);">';
            }
            $code .= "<script type='text/javascript' src='{sugar_getjspath file='include/connectors/formatters/default/company_detail.js'}'></script>\n";
            $code .= "<script type='text/javascript'>\n";
            $code .= "function showConnectorMenu2(el) {literal} { {/literal}\n";

            $menuParams .= '";';
            $code .= $menuParams . "\n";
            $code .= "return SUGAR.util.showHelpTips(el,menuParams);\n";
            $code .= "{literal} } {/literal}\n";
            $code .= "</script>\n";
            $code .= $formatterCode;
        }
        return $code;
    }
}
