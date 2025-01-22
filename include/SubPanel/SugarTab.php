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
 * Tab representation
 * @api
 */
class SugarTab
{
    public function __construct($type='singletabmenu')
    {
        $this->type = $type;
        $this->ss = new Sugar_Smarty();
    }

    public function setup($mainTabs, $otherTabs=array(), $subTabs=array(), $selected_group='All')
    {
        global $sugar_version, $sugar_config, $current_user;

        $max_tabs = $current_user->getPreference('max_tabs');
        if (!isset($max_tabs) || $max_tabs <= 0) {
            $max_tabs = $GLOBALS['sugar_config']['default_max_tabs'];
        }

        $key_all = translate('LBL_TABGROUP_ALL');
        if ($selected_group == 'All') {
            $selected_group = $key_all;
        }

        $moreTabs = array_slice($mainTabs, $max_tabs);
        /* If the current tab is in the 'More' menu, move it into the visible menu. */
        if (!empty($moreTabs[$selected_group])) {
            $temp = array($selected_group => $mainTabs[$selected_group]);
            unset($mainTabs[$selected_group]);
            array_splice($mainTabs, $max_tabs-1, 0, $temp);
        }

        $subpanelTitles = array();

        if (isset($otherTabs[$key_all]) && isset($otherTabs[$key_all]['tabs'])) {
            foreach ($otherTabs[$key_all]['tabs'] as $subtab) {
                $subpanelTitles[$subtab['key']] = $subtab['label'];
            }
        }

        $this->ss->assign('showLinks', 'false');
        $this->ss->assign('sugartabs', array_slice($mainTabs, 0, $max_tabs));
        $this->ss->assign('moreMenu', array_slice($mainTabs, $max_tabs));
        $this->ss->assign('othertabs', $otherTabs);
        $this->ss->assign('subpanelTitlesJSON', json_encode($subpanelTitles));
        $this->ss->assign('startSubPanel', $selected_group);
        $this->ss->assign('sugarVersionJsStr', "?s=$sugar_version&c={$sugar_config['js_custom_version']}");
        if (!empty($mainTabs)) {
            $mtak = array_keys($mainTabs);
            $this->ss->assign('moreTab', $mainTabs[$mtak[min(count($mtak)-1, $max_tabs-1)]]['label']);
        }
    }

    public function fetch()
    {
        return $this->ss->fetch('include/SubPanel/tpls/' . $this->type . '.tpl');
    }

    public function display()
    {
        $this->ss->display('include/SubPanel/tpls/' . $this->type . '.tpl');
    }
}
