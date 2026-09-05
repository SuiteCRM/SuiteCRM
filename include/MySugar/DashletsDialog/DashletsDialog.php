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


require_once 'modules/ModuleBuilder/Module/IconRepository.php';

class DashletsDialog
{
    public $dashlets = array();

    public function getDashlets($category='')
    {
        global $app_strings, $current_language, $mod_strings;

        require_once($GLOBALS['sugar_config']['cache_dir'].'dashlets/dashlets.php');

        $categories = array( 'module' 	=> 'Module Views',
            'portal' 	=> 'Portal',
            'charts'	=> 'Charts',
            'tools'	=> 'Tools',
            'misc'		=> 'Miscellaneous',
            'web'      => 'Web');

        $dashletStrings = array();
        $dashletsList = array();

        if (!empty($category)) {
            $dashletsList[$categories[$category]] = array();
        } else {
            $dashletsList['Module Views'] = array();
            $dashletsList['Charts'] = array();
            $dashletsList['Tools'] = array();
            $dashletsList['Web'] = array();
        }

        asort($dashletsFiles);

        foreach ($dashletsFiles as $className => $files) {
            if (!empty($files['meta']) && is_file($files['meta'])) {
                require_once($files['meta']); // get meta file

                $directory = substr($files['meta'], 0, strrpos($files['meta'], '/') + 1);
                if (is_file($directory . $files['class'] . '.' . $current_language . '.lang.php')) {
                    require_once($directory . $files['class'] . '.' . $current_language . '.lang.php');
                } elseif (is_file($directory . $files['class'] . '.en_us.lang.php')) {
                    require_once($directory . $files['class'] . '.en_us.lang.php');
                }

                // try to translate the string
                if (empty($dashletStrings[$files['class']][$dashletMeta[$files['class']]['title']])) {
                    $title = $dashletMeta[$files['class']]['title'];
                } else {
                    $title = $dashletStrings[$files['class']][$dashletMeta[$files['class']]['title']];
                }

                // try to translate the string
                if (empty($dashletStrings[$files['class']][$dashletMeta[$files['class']]['description']])) {
                    $description = $dashletMeta[$files['class']]['description'];
                } else {
                    $description = $dashletStrings[$files['class']][$dashletMeta[$files['class']]['description']];
                }

                // generate icon
                if (!empty($dashletMeta[$files['class']]['icon'])) {
                    // here we'll support image inheritance if the supplied image has a path in it
                    // i.e. $dashletMeta[$files['class']]['icon'] = 'themes/default/images/dog.gif'
                    // in this case, we'll strip off the path information to check for the image existing
                    // in the current theme.

                    $imageName = $files['class'];
                    if (!empty($imageName)) {
                        $icon = $imageName;
                    }
                } else {
                    if (empty($dashletMeta[$files['class']]['module'])) {
                        $icon = get_dashlets_dialog_icon('default');
                    } else {
                        if ((!in_array($dashletMeta[$files['class']]['module'], $GLOBALS['moduleList']) && !in_array($dashletMeta[$files['class']]['module'], $GLOBALS['modInvisList'])) && (!in_array('Activities', $GLOBALS['moduleList']))) {
                            unset($dashletMeta[$files['class']]);
                            continue;
                        } else {
                            $icon = get_dashlets_dialog_icon($dashletMeta[$files['class']]['module']);
                        }
                    }
                }

                // determine whether to display
                if (!empty($dashletMeta[$files['class']]['hidden']) && $dashletMeta[$files['class']]['hidden'] === true) {
                    $displayDashlet = false;
                }
                //co: fixes 20398 to respect ACL permissions
                elseif (!empty($dashletMeta[$files['class']]['module']) && (!in_array($dashletMeta[$files['class']]['module'], $GLOBALS['moduleList']) && !in_array($dashletMeta[$files['class']]['module'], $GLOBALS['modInvisList'])) && (!in_array('Activities', $GLOBALS['moduleList']))) {
                    $displayDashlet = false;
                } else {
                    $displayDashlet = true;
                    //check ACL ACCESS
                    if (!empty($dashletMeta[$files['class']]['module']) && ACLController::moduleSupportsACL($dashletMeta[$files['class']]['module'])) {
                        $type = 'module';
                        if ($dashletMeta[$files['class']]['module'] == 'Trackers') {
                            $type = 'Tracker';
                        }
                        if (!ACLController::checkAccess($dashletMeta[$files['class']]['module'], 'view', true, $type)) {
                            $displayDashlet = false;
                        }
                        if (!ACLController::checkAccess($dashletMeta[$files['class']]['module'], 'list', true, $type)) {
                            $displayDashlet = false;
                        }
                    }
                }

                if ($dashletMeta[$files['class']]['category'] == 'Charts') {
                    $type = 'predefined_chart';
                } else {
                    $type = 'module';
                }

                if ($displayDashlet && isset($dashletMeta[$files['class']]['dynamic_hide']) && $dashletMeta[$files['class']]['dynamic_hide']) {
                    if (file_exists($files['file'])) {
                        require_once($files['file']);
                        if (class_exists($files['class'])) {
                            $dashletClassName = $files['class'];
                            $displayDashlet = call_user_func(array($files['class'],'shouldDisplay'));
                        }
                    }
                }

                if ($displayDashlet) {
                    $cell = array( 'title' => $title,
                        'description' => $description,
                        'onclick' => 'return SUGAR.mySugar.addDashlet(\'' . $className . '\', \'' . $type . '\', \''.(!empty($dashletMeta[$files['class']]['module']) ? $dashletMeta[$files['class']]['module'] : '') .'\');',
                        'icon' => IconRepository::getIconName($dashletMeta[$files['class']]['module']),
                        'id' => $files['class'] . '_select',
                        'module_name'=> array_key_exists('module', $dashletsFiles[$className]) ? $dashletsFiles[$className]['module']:""
                    );

                    if (!empty($category) && $dashletMeta[$files['class']]['category'] == $categories[$category]) {
                        array_push($dashletsList[$categories[$category]], $cell);
                    } else {
                        if (empty($category)) {
                            array_push($dashletsList[$dashletMeta[$files['class']]['category']], $cell);
                        }
                    }
                }
            }
        }
        if (!empty($category)) {
            asort($dashletsList[$categories[$category]]);
        } else {
            foreach ($dashletsList as $key=>$value) {
                asort($dashletsList[$key]);
            }
        }
        $this->dashlets = $dashletsList;
    }
}
