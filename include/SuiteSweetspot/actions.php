<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2026 SalesAgility Ltd.
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * Build action entries available to current user.
 *
 * @return array
 */
function suite_sweetspot_build_dynamic_actions()
{
    global $current_user, $app_list_strings, $mod_strings;

    require_once 'include/utils.php';
    require_once 'include/modules.php';
    require_once 'modules/ACL/ACLController.php';

    $actions = array();

    $accessibleModules = query_module_access_list($current_user);
    ACLController::filterModuleList($accessibleModules);

    if (empty($app_list_strings['moduleList'])) {
        require_once 'include/language/en_us.lang.php';
    }

    $moduleList = $app_list_strings['moduleList'] ?? array();

    foreach ($accessibleModules as $moduleKey => $moduleValue) {
        $moduleName = is_string($moduleValue) && !empty($moduleValue) ? $moduleValue : $moduleKey;

        global $modInvisList;
        if (isset($modInvisList) && in_array($moduleName, $modInvisList, true)) {
            continue;
        }

        $label = $moduleList[$moduleName] ?? $moduleName;

        suite_sweetspot_add_module_menu_actions($actions, $moduleName, $label);
    }

    global $beanList;
    if (!empty($beanList)) {
        foreach ($beanList as $moduleName => $beanClass) {
            if (isset($actions[$moduleName . '_list'])) {
                continue;
            }

            global $modInvisList;
            if (isset($modInvisList) && in_array($moduleName, $modInvisList, true)) {
                continue;
            }

            $label = $moduleList[$moduleName] ?? $moduleName;
            suite_sweetspot_add_module_menu_actions($actions, $moduleName, $label);
        }
    }

    suite_sweetspot_add_global_navigation_actions($actions);

    if (is_admin($current_user)) {
        suite_sweetspot_add_admin_actions($actions);
    }

    return $actions;
}

/**
 * Add admin actions from admin panel definitions.
 *
 * @param array $actions
 */
function suite_sweetspot_add_admin_actions(&$actions)
{
    global $mod_strings, $admin_group_header;

    require_once 'modules/Administration/language/en_us.lang.php';
    if (empty($mod_strings)) {
        $mod_strings = return_module_language('en_us', 'Administration');
    }

    $adminPanelFile = 'modules/Administration/metadata/adminpaneldefs.php';
    if (!file_exists($adminPanelFile)) {
        return;
    }

    $admin_option_defs = array();
    $admin_group_header = array();

    ob_start();
    require $adminPanelFile;
    ob_end_clean();

    if (!empty($admin_group_header) && is_array($admin_group_header)) {
        foreach ($admin_group_header as $group) {
            if (empty($group[3]) || !is_array($group[3])) {
                continue;
            }

            $allOptions = array();
            foreach ($group[3] as $moduleKey => $moduleOptions) {
                if (!is_array($moduleOptions)) {
                    continue;
                }

                if (isset($moduleOptions[0]) && is_array($moduleOptions[0])) {
                    foreach ($moduleOptions as $optionKey => $option) {
                        if (is_array($option) && isset($option[3])) {
                            $allOptions[] = array(
                                'module' => $moduleKey,
                                'key' => $optionKey,
                                'option' => $option,
                            );
                        }
                    }
                } else {
                    if (isset($moduleOptions[3])) {
                        $allOptions[] = array(
                            'module' => $moduleKey,
                            'key' => $moduleKey,
                            'option' => $moduleOptions,
                        );
                    } else {
                        foreach ($moduleOptions as $optionKey => $option) {
                            if (is_array($option) && isset($option[3])) {
                                $allOptions[] = array(
                                    'module' => $moduleKey,
                                    'key' => $optionKey,
                                    'option' => $option,
                                );
                            }
                        }
                    }
                }
            }

            foreach ($allOptions as $item) {
                $option = $item['option'];
                if (!is_array($option) || empty($option[3])) {
                    continue;
                }

                $url = ltrim($option[3], './');
                if (empty($url) || strpos($url, 'javascript:') === 0) {
                    continue;
                }

                $labelKey = $option[1] ?? '';
                $label = $labelKey;
                if (!empty($mod_strings[$labelKey])) {
                    $label = $mod_strings[$labelKey];
                } elseif (!empty($option[0]) && is_string($option[0])) {
                    $label = $option[0];
                }

                $id = 'admin_' . $item['module'] . '_' . $item['key'];
                if (isset($actions[$id])) {
                    continue;
                }

                $keywords = array(
                    strtolower($label),
                    strtolower($labelKey),
                    strtolower($item['key']),
                    'admin',
                );

                $actions[$id] = array(
                    'id' => $id,
                    'label' => $label,
                    'module' => $item['module'],
                    'url' => $url,
                    'weight' => 30,
                    'acl_action' => 'admin',
                    'keywords' => $keywords,
                );
            }
        }
    }

    $customAdminFile = 'custom/modules/Administration/Ext/Administration/administration.ext.php';
    if (file_exists($customAdminFile)) {
        $admin_option_defs = array();
        ob_start();
        include $customAdminFile;
        ob_end_clean();

        if (!empty($admin_option_defs) && is_array($admin_option_defs)) {
            foreach ($admin_option_defs as $moduleKey => $moduleOptions) {
                if (!is_array($moduleOptions)) {
                    continue;
                }

                foreach ($moduleOptions as $optionKey => $option) {
                    if (!is_array($option) || empty($option[3])) {
                        continue;
                    }

                    $url = ltrim($option[3], './');
                    if (empty($url) || strpos($url, 'javascript:') === 0) {
                        continue;
                    }

                    $labelKey = $option[1] ?? '';
                    $label = !empty($mod_strings[$labelKey]) ? $mod_strings[$labelKey] : ($option[0] ?? $labelKey);

                    $id = 'admin_custom_' . $moduleKey . '_' . $optionKey;
                    if (isset($actions[$id])) {
                        continue;
                    }

                    $actions[$id] = array(
                        'id' => $id,
                        'label' => $label,
                        'module' => $moduleKey,
                        'url' => $url,
                        'weight' => 30,
                        'acl_action' => 'admin',
                        'keywords' => array(strtolower($label), strtolower($labelKey), strtolower($optionKey), 'admin'),
                    );
                }
            }
        }
    }
}

/**
 * Add module routes from module menu definitions, including custom menu extensions.
 *
 * @param array  $actions
 * @param string $moduleName
 * @param string $moduleLabel
 */
function suite_sweetspot_add_module_menu_actions(&$actions, $moduleName, $moduleLabel)
{
    $module_menu = array();
    $menuFile = get_custom_file_if_exists('modules/' . $moduleName . '/Menu.php');
    if (file_exists($menuFile)) {
        require $menuFile;
    }

    $menuExtFile = 'custom/modules/' . $moduleName . '/Ext/Menus/menu.ext.php';
    if (file_exists($menuExtFile)) {
        require $menuExtFile;
    }

    if (empty($module_menu) || !is_array($module_menu)) {
        return;
    }

    foreach ($module_menu as $idx => $menuItem) {
        if (!is_array($menuItem) || empty($menuItem[0])) {
            continue;
        }

        $url = (string)$menuItem[0];
        if (strpos($url, 'javascript:') === 0) {
            continue;
        }

        $label = isset($menuItem[1]) ? (string)$menuItem[1] : ('Open ' . $moduleLabel);
        $icon = isset($menuItem[2]) ? (string)$menuItem[2] : '';
        $id = 'menu_' . $moduleName . '_' . md5($url . '|' . $label . '|' . $idx);
        if (isset($actions[$id])) {
            continue;
        }

        $keywords = array(
            strtolower($moduleName),
            strtolower($moduleLabel),
            strtolower($label),
            'menu',
        );

        $actions[$id] = array(
            'id' => $id,
            'label' => $label,
            'module' => $moduleName,
            'url' => $url,
            'icon' => $icon,
            'weight' => 25,
            'acl_action' => null,
            'keywords' => $keywords,
        );

        if (!isset($actions[$moduleName . '_list']) && suite_sweetspot_is_list_url($url)) {
            $actions[$moduleName . '_list'] = array(
                'id' => $moduleName . '_list',
                'label' => $moduleLabel,
                'module' => $moduleName,
                'url' => $url,
                'icon' => $icon,
                'weight' => 10,
                'acl_action' => 'list',
                'keywords' => array(strtolower($moduleLabel), strtolower($moduleName), 'list'),
            );
        }

        if (!isset($actions[$moduleName . '_create']) && suite_sweetspot_is_create_url($url)) {
            $actions[$moduleName . '_create'] = array(
                'id' => $moduleName . '_create',
                'label' => 'Create ' . $moduleLabel,
                'module' => $moduleName,
                'url' => $url,
                'icon' => $icon,
                'weight' => 20,
                'acl_action' => 'edit',
                'keywords' => array('create', strtolower($moduleLabel), 'new ' . strtolower($moduleLabel)),
            );
        }
    }
}

/**
 * Add dynamic global navigation links (home/profile/admin/user actions).
 *
 * @param array $actions
 */
function suite_sweetspot_add_global_navigation_actions(&$actions)
{
    $global_control_links = array();
    require 'include/globalControlLinks.php';

    foreach ($global_control_links as $section => $group) {
        if (!is_array($group) || empty($group['linkinfo']) || !is_array($group['linkinfo'])) {
            continue;
        }

        foreach ($group['linkinfo'] as $label => $url) {
            if (empty($url) || strpos((string)$url, 'javascript:') === 0) {
                continue;
            }

            $id = 'global_' . $section . '_' . md5($label . '|' . $url);
            if (isset($actions[$id])) {
                continue;
            }

            $labelString = (string)$label;
            $actions[$id] = array(
                'id' => $id,
                'label' => $labelString,
                'module' => ucfirst((string)$section),
                'url' => (string)$url,
                'weight' => 12,
                'acl_action' => null,
                'keywords' => array(strtolower($labelString), strtolower((string)$section), 'global'),
            );
        }
    }
}

/**
 * Determine whether a menu URL represents a list route.
 *
 * @param string $url
 * @return bool
 */
function suite_sweetspot_is_list_url($url)
{
    $query = parse_url((string)$url, PHP_URL_QUERY);
    if ($query === null) {
        return false;
    }

    parse_str($query, $params);
    if (empty($params['action'])) {
        return true;
    }

    return strcasecmp((string)$params['action'], 'index') === 0 ||
        strcasecmp((string)$params['action'], 'ListView') === 0;
}

/**
 * Determine whether a menu URL represents a create route.
 *
 * @param string $url
 * @return bool
 */
function suite_sweetspot_is_create_url($url)
{
    $query = parse_url((string)$url, PHP_URL_QUERY);
    if ($query === null) {
        return false;
    }

    parse_str($query, $params);
    if (empty($params['action'])) {
        return false;
    }

    return strcasecmp((string)$params['action'], 'EditView') === 0 ||
        strcasecmp((string)$params['action'], 'QuickCreate') === 0;
}

/**
 * Filter actions by ACL and user context.
 *
 * @param User $current_user
 * @return array
 */
function suite_sweetspot_get_actions_for_user($current_user)
{
    $all = suite_sweetspot_build_dynamic_actions();

    require_once 'include/utils.php';
    require_once 'modules/ACL/ACLController.php';

    $result = array();
    foreach ($all as $id => $action) {
        $module = !empty($action['module']) ? $action['module'] : null;
        $aclAction = !empty($action['acl_action']) ? $action['acl_action'] : null;

        if ($aclAction === 'admin') {
            if (!is_admin($current_user)) {
                continue;
            }
        } elseif ($module && $aclAction) {
            if (!ACLController::checkAccess($module, $aclAction, true)) {
                continue;
            }
        }

        $result[$id] = $action;
    }

    uasort($result, function ($a, $b) {
        $wa = isset($a['weight']) ? (int)$a['weight'] : 0;
        $wb = isset($b['weight']) ? (int)$b['weight'] : 0;
        if ($wa === $wb) {
            $la = isset($a['label']) ? (string)$a['label'] : '';
            $lb = isset($b['label']) ? (string)$b['label'] : '';
            return strcasecmp($la, $lb);
        }

        return ($wa < $wb) ? -1 : 1;
    });

    return $result;
}
