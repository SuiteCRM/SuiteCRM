<?php

/**
 * 
 * This script is in charge of adding the SticNews Dashlet in the dashboard of all Users in the instance. If the dashboard already contains
 * the dashlet, this function don't do anything.
 */


global $current_user;
$current_user = new User();
$current_user->getSystemUser();

$userEmptyBean = BeanFactory::getBean('Users');
$userBeanArray = $userEmptyBean->get_full_list();
$sticDashlet = array(
    'className' => 'SticNewsDashlet',
    'module' => 'Home',
    'options' => array(),
    'fileLocation' => 'custom/modules/Home/Dashlets/SticNewsDashlet/SticNewsDashlet.php',
);


if (empty($userBeanArray)) {
    $GLOBALS['log']->debug('No users found. Skipping SticNewsDashlet installation.');
} else {
    foreach ($userBeanArray as $userBean) {
        $dashletId = false;
        require_once 'modules/UserPreferences/UserPreference.php';
        $userBean = new UserPreference($userBean);
        $foundInDashlet = false;
        $dashletArray = $userBean->getPreference('dashlets', 'Home');

        if (is_array($dashletArray)) {
            foreach ($dashletArray as $key => $dashlet) {
                if ($dashlet['className'] === 'SticNewsDashlet') {
                    $dashletId = $key;
                    $GLOBALS['log']->debug('Dashlet found in User dashboard');
                    break;
                }
            }
        }
        if (!$dashletId) {
            $dashletId = create_guid();
            $GLOBALS['log']->debug('Dashlet not found in Dashlet array. We will add it...');
            $dashletArray[$dashletId] = $sticDashlet;
            $userBean->setPreference('dashlets', $dashletArray, 'Home');
            $GLOBALS['log']->debug('Dashlet added in Dashlet array');
        }

        $pages = $userBean->getPreference('pages', 'Home');
        if (is_array($pages) && isset($pages[0]['columns'][1]['dashlets']) && is_array($pages[0]['columns'][1]['dashlets'])) {
            if (in_array($dashletId, $pages[0]['columns'][1]['dashlets'])) {
                $GLOBALS['log']->debug('Dashlet found in Page array');
            } else {
                $GLOBALS['log']->debug('Dashlet not found in Page. We will add it...');
                array_unshift($pages[0]['columns'][1]['dashlets'], $dashletId);
                $userBean->setPreference('pages', $pages, 'Home');
                $GLOBALS['log']->debug('Dashlet added in Page array');
            }
        }
        $userBean->savePreferencesToDB();
    }
}