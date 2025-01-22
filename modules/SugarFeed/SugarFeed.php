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

#[\AllowDynamicProperties]
class SugarFeed extends Basic
{
    public $new_schema = true;
    public $module_dir = 'SugarFeed';
    public $object_name = 'SugarFeed';
    public $table_name = 'sugarfeed';
    public $importable = false;

    public $id;
    public $name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $modified_by_name;
    public $created_by;
    public $created_by_name;
    public $description;
    public $deleted;
    public $created_by_link;
    public $modified_user_link;
    public $assigned_user_id;
    public $assigned_user_name;
    public $assigned_user_link;

    public function __construct()
    {
        parent::__construct();
    }




    public static function activateModuleFeed($module, $updateDB = true)
    {
        if ($module != 'UserFeed') {
            // UserFeed is a fake module, used for the user postings to the feed
            // Don't try to load up any classes for it
            $fileList = SugarFeed::getModuleFeedFiles($module);

            foreach ($fileList as $fileName) {
                $feedClass = substr(basename((string) $fileName), 0, -4);

                require_once($fileName);
                $tmpClass = new $feedClass();
                $tmpClass->installHook($fileName, $feedClass);
            }
        }
        if ($updateDB == true) {
            $admin = BeanFactory::newBean('Administration');
            $admin->saveSetting('sugarfeed', 'module_'.$admin->db->quote($module), '1');
        }
    }

    public static function disableModuleFeed($module, $updateDB = true)
    {
        if ($module != 'UserFeed') {
            // UserFeed is a fake module, used for the user postings to the feed
            // Don't try to load up any classes for it
            $fileList = SugarFeed::getModuleFeedFiles($module);

            foreach ($fileList as $fileName) {
                $feedClass = substr(basename((string) $fileName), 0, -4);

                require_once($fileName);
                $tmpClass = new $feedClass();
                $tmpClass->removeHook($fileName, $feedClass);
            }
        }

        if ($updateDB == true) {
            $admin = BeanFactory::newBean('Administration');
            $admin->saveSetting('sugarfeed', 'module_'.$admin->db->quote($module), '0');
        }
    }

    public static function flushBackendCache()
    {
        // This function will flush the cache files used for the module list and the link type lists
        sugar_cache_clear('SugarFeedModules');
        if (file_exists($cachefile = sugar_cached('modules/SugarFeed/moduleCache.php'))) {
            unlink($cachefile);
        }

        sugar_cache_clear('SugarFeedLinkType');
        if (file_exists($cachefile = sugar_cached('modules/SugarFeed/linkTypeCache.php'))) {
            unlink($cachefile);
        }
    }


    public static function getModuleFeedFiles($module)
    {
        $baseDirList = array('modules/'.$module.'/SugarFeeds/', 'custom/modules/'.$module.'/SugarFeeds/');

        // We store the files in a list sorted by the filename so you can override a default feed by
        // putting your replacement feed in the custom directory with the same filename
        $fileList = array();

        foreach ($baseDirList as $baseDir) {
            if (! file_exists($baseDir)) {
                continue;
            }
            $d = dir($baseDir);
            while ($file = $d->read()) {
                if ($file[0] == '.') {
                    continue;
                }
                if (substr($file, -4) == '.php') {
                    // We found one
                    $fileList[$file] = $baseDir.$file;
                }
            }
        }

        return($fileList);
    }

    public static function getActiveFeedModules()
    {
        // Stored in a cache somewhere
        $feedModules = sugar_cache_retrieve('SugarFeedModules');
        if ($feedModules != null) {
            return($feedModules);
        }

        // Already stored in a file
        if (file_exists($cachefile = sugar_cached('modules/SugarFeed/moduleCache.php'))) {
            require_once($cachefile);
            sugar_cache_put('SugarFeedModules', $feedModules);
            return $feedModules;
        }

        // Gotta go looking for it

        $admin = BeanFactory::newBean('Administration');
        $admin->retrieveSettings();

        $feedModules = array();
        if (isset($admin->settings['sugarfeed_enabled']) && $admin->settings['sugarfeed_enabled'] == '1') {
            // Only enable modules if the feed system is enabled
            foreach ($admin->settings as $key => $value) {
                if (strncmp($key, 'sugarfeed_module_', 17) === 0) {
                    // It's a module setting
                    if ($value == '1') {
                        $moduleName = substr((string) $key, 17);
                        $feedModules[$moduleName] = $moduleName;
                    }
                }
            }
        }


        sugar_cache_put('SugarFeedModules', $feedModules);
        if (! file_exists($cachedir = sugar_cached('modules/SugarFeed'))) {
            mkdir_recursive($cachedir);
        }
        $fd = fopen("$cachedir/moduleCache.php", 'wb');
        fwrite($fd, '<'."?php\n\n".'$feedModules = '.var_export($feedModules, true).';');
        fclose($fd);

        return $feedModules;
    }

    public static function getAllFeedModules()
    {
        // Uncached, only used from the admin panel and during installation currently
        $feedModules = array('UserFeed'=>'UserFeed');

        $baseDirList = array('modules/', 'custom/modules/');
        foreach ($baseDirList as $baseDir) {
            if (! file_exists($baseDir)) {
                continue;
            }
            $d = dir($baseDir);
            while ($module = $d->read()) {
                if (file_exists($baseDir.$module.'/SugarFeeds/')) {
                    $dFeed = dir($baseDir.$module.'/SugarFeeds/');
                    while ($file = $dFeed->read()) {
                        if ($file[0] == '.') {
                            continue;
                        }
                        if (substr($file, -4) == '.php') {
                            // We found one
                            $feedModules[$module] = $module;
                        }
                    }
                }
            }
        }

        return($feedModules);
    }

    /**
     * pushFeed2
     * This method is a wrapper to pushFeed
     *
     * @param $text String value of the feed's description
     * @param $bean The SugarBean that is triggering the feed
     * @param $link_type boolean value indicating whether or not feed is a link type
     * @param $link_url String value of the URL (for link types only)
     */
    public static function pushFeed2($text, $bean, $link_type=false, $link_url=false)
    {
        self::pushFeed(
            $text,
            $bean->module_dir,
            $bean->id,
            $bean->assigned_user_id,
            $link_type,
            $link_url
        );
    }

    public static function pushFeed(
        $text,
        $module,
        $id,
        $record_assigned_user_id=false,
        $link_type=false,
        $link_url=false
        ) {
        $feed = BeanFactory::newBean('SugarFeed');
        if ((empty($text) && empty($link_url)) || !$feed->ACLAccess('save', true)) {
            $GLOBALS['log']->error('Unable to save SugarFeed record (missing data or no ACL access)');
            return;
        }

        if (!empty($link_url)) {
            $linkClass = SugarFeed::getLinkClass($link_type);
            if ($linkClass !== false) {
                $linkClass->handleInput($feed, $link_type, $link_url);
            }
        }
        $text = strip_tags(from_html($text));
        $text = '<b>{this.CREATED_BY}</b> ' . $text;
        $feed->name = mb_substr($text, 0, 255, 'UTF-8');
        if (mb_strlen($text, 'UTF-8') > 255) {
            $feed->description = mb_substr($text, 255, 510, 'UTF-8');
        }

        if ($record_assigned_user_id === false) {
            $feed->assigned_user_id = $GLOBALS['current_user']->id;
        } else {
            $feed->assigned_user_id = $record_assigned_user_id;
        }
        $feed->related_id = $id;
        $feed->related_module = $module;
        $feed->save();
    }

    public static function getLinkTypes()
    {
        static $linkTypeList = null;

        // Fastest, already stored in the static variable
        if ($linkTypeList != null) {
            return $linkTypeList;
        }

        // Second fastest, stored in a cache somewhere
        $linkTypeList = sugar_cache_retrieve('SugarFeedLinkType');
        if ($linkTypeList != null) {
            return($linkTypeList);
        }

        // Third fastest, already stored in a file
        if (file_exists($cachedfile = sugar_cached('modules/SugarFeed/linkTypeCache.php'))) {
            require_once($cachedfile);
            sugar_cache_put('SugarFeedLinkType', $linkTypeList);
            return $linkTypeList;
        }

        // Slow, have to actually collect the data
        $baseDirs = array('custom/modules/SugarFeed/linkHandlers/','modules/SugarFeed/linkHandlers');

        $linkTypeList = array();

        foreach ($baseDirs as $dirName) {
            if (!file_exists($dirName)) {
                continue;
            }
            $d = dir($dirName);
            while ($file = $d->read()) {
                if ($file[0] == '.') {
                    continue;
                }
                if (substr($file, -4) == '.php') {
                    // We found one
                    $typeName = substr($file, 0, -4);
                    $linkTypeList[$typeName] = $typeName;
                }
            }
        }

        sugar_cache_put('SugarFeedLinkType', $linkTypeList);
        if (! file_exists($cachedir = sugar_cached('modules/SugarFeed'))) {
            mkdir_recursive($cachedir);
        }
        $fd = fopen("$cachedir/linkTypeCache.php", 'wb');
        fwrite($fd, '<'."?php\n\n".'$linkTypeList = '.var_export($linkTypeList, true).';');
        fclose($fd);

        return $linkTypeList;
    }

    public static function getLinkClass($linkName)
    {
        $linkTypeList = SugarFeed::getLinkTypes();

        // Have to make sure the linkName is on the list, so they can't pass in linkName's like ../../config.php ... not that they could get anywhere if they did
        if (! isset($linkTypeList[$linkName])) {
            // No class by this name...
            return false;
        }

        if (file_exists('custom/modules/SugarFeed/linkHandlers/'.$linkName.'.php')) {
            require_once('custom/modules/SugarFeed/linkHandlers/'.$linkName.'.php');
        } elseif(!file_exists('modules/SugarFeed/linkHandlers/'.$linkName.'.php')) {
            return false;
        } else {
            require_once('modules/SugarFeed/linkHandlers/'.$linkName.'.php');
        }

        $linkClassName = 'FeedLinkHandler'.$linkName;

        $linkClass = new $linkClassName();

        return($linkClass);
    }

    public function get_list_view_data()
    {
        $data = parent::get_list_view_data();
        $delete = '';

        if (!isset($data['CREATED_BY'])) {
            LoggerManager::getLogger()->warn('SugarFeed fetchReplies: Undefined index: $data[CREATED_BY]');
            $dataCreateBy = null;
        } else {
            $dataCreateBy = $data['CREATED_BY'];
        }

        if (!isset($data['DESCRIPTION'])) {
            LoggerManager::getLogger()->warn('SugarFeed get_list_view_data: Undefined index: DESCRIPTION ');
            $dataDescription = null;
        } else {
            $dataDescription = $data['DESCRIPTION'];
        }

        if (!isset($data['NAME'])) {
            $data['NAME'] = '';
        }

        $data['NAME'] .= $dataDescription;
        $data['NAME'] =  '<div style="padding:3px">' . html_entity_decode($data['NAME']);
        if (!empty($data['LINK_URL'])) {
            $linkClass = SugarFeed::getLinkClass($data['LINK_TYPE']);
            if ($linkClass !== false) {
                $data['NAME'] .= $linkClass->getDisplay($data);
            }
        }
        $data['NAME'] .= '<div class="byLineBox"><span class="byLineLeft">';

        if (!isset($data['DATE_ENTERED'])) {
            LoggerManager::getLogger()->warn('SugarFeed get_list_view_data: Undefined index: DATE_ENTERED ');
            $dataDateEntered = null;
        } else {
            $dataDateEntered = $data['DATE_ENTERED'];
        }

        if (!isset($data['ID'])) {
            LoggerManager::getLogger()->warn('SugarFeed get_list_view_data: Undefined index: ID ');
            $dataId = null;
        } else {
            $dataId = $data['ID'];
        }

        if (is_admin($GLOBALS['current_user']) || $dataCreateBy == $GLOBALS['current_user']->id) {
            $delete = ' | <a id="sugarFieldDeleteLink'.$dataId.'" href="#" onclick=\'SugarFeed.deleteFeed("'. $dataId . '", "{this.id}"); return false;\'>'. $GLOBALS['app_strings']['LBL_DELETE_BUTTON_LABEL'].'</a>';
        }

        $data['NAME'] .= static::getTimeLapse($dataDateEntered) . '&nbsp;</span><div class="byLineRight"><a id="sugarFeedReplyLink'.$dataId.'" href="#" onclick=\'SugarFeed.buildReplyForm("'.$dataId.'", "{this.id}", this); return false;\'>'.$GLOBALS['app_strings']['LBL_EMAIL_REPLY'].'</a>' .$delete. '</div></div>';

        $data['NAME'] .= $this->fetchReplies($data);
        return  $data ;
    }

    public function fetchReplies($data)
    {
        $seedBean = new SugarFeed;

        if (!isset($data['ID'])) {
            LoggerManager::getLogger()->warn('SugarFeed fetchReplies: Undefined index: ID ');
            $dataId = null;
        } else {
            $dataId = $data['ID'];
        }

        $replies = $seedBean->get_list('date_entered', "related_module = 'SugarFeed' AND related_id = '".$dataId."'");

        if ((is_countable($replies['list']) ? count($replies['list']) : 0) < 1) {
            return '';
        }


        $replyHTML = '<div class="clear"></div><blockquote>';

        foreach ($replies['list'] as $reply) {
            // Setup the delete link
            $delete = '';
            if (is_admin($GLOBALS['current_user']) || $reply->created_by == $GLOBALS['current_user']->id) {
                $delete = '<a id="sugarFieldDeleteLink'.$reply->id.'" href="#" onclick=\'SugarFeed.deleteFeed("'. $reply->id . '", "{this.id}"); return false;\'>'. $GLOBALS['app_strings']['LBL_DELETE_BUTTON_LABEL'].'</a>';
            }

            $user = BeanFactory::getBean('Users', $reply->created_by);
            $image_url = 'include/images/default_user_feed_picture.png';
            if (!empty($user) && !empty($user->picture)) {
                $image_url = 'index.php?entryPoint=download&id=' . $user->picture . '&type=SugarFieldImage&isTempFile=1&isProfile=1';
            }

            $replyHTML .= '<div style="float: left; margin-right: 3px; width: 50px; height: 50px;"><!--not_in_theme!--><img src="'.$image_url.'" style="max-width: 50px; max-height: 50px;"></div> ';
            $replyHTML .= str_replace("{this.CREATED_BY}", get_assigned_user_name($reply->created_by), html_entity_decode((string) $reply->name)).'<br>';
            $replyHTML .= '<div class="byLineBox"><span class="byLineLeft">'. static::getTimeLapse($reply->date_entered) . '&nbsp;</span><div class="byLineRight">  &nbsp;' .$delete. '</div></div><div class="clear"></div>';
        }

        $replyHTML .= '</blockquote>';
        return $replyHTML;
    }

    public static function getTimeLapse($startDate)
    {
        global $timedate;

        $timedate->getInstance()->userTimezone();
        $currentTime = $timedate->now();
        //Fix #9875 SugarFeed shows 0 seconds ago and negative interval for certain datetime formats
        //Use proper user datetime format to convert datetime string to timestamp
        $user_format=$timedate->get_date_time_format();
		$first=date_create_from_format($user_format,$currentTime);
        if(empty($first)){
            LoggerManager::getLogger()->warn('SugarFeed getTimeLapse: Could not fetch currentTime ');
            $first=0;
        }
        else{
            $first=$first->getTimestamp();
        }

        $second=date_create_from_format($user_format,$startDate);
        if(empty($second)){
            LoggerManager::getLogger()->warn('SugarFeed getTimeLapse: Could not fetch startDate ');
            $second=0;
        }
        else{
            $second=$second->getTimestamp();
        }

        $seconds = $first - $second;
        $minutes = $seconds / 60;
        $seconds = $seconds % 60;
        $hours = floor($minutes / 60);
        $minutes = $minutes % 60;
        $days = floor($hours / 24);
        $hours = $hours % 24;
        $weeks = floor($days / 7);
        $days = $days % 7;
        $result = '';
        if ($weeks == 1) {
            return translate('LBL_TIME_LAST_WEEK', 'SugarFeed') . ' ';
        } elseif ($weeks > 1) {
            $result .= $weeks . ' ' . translate('LBL_TIME_WEEKS', 'SugarFeed') . ' ';
            if ($days > 0) {
                $result .= ' ' . translate('LBL_TIME_AND', 'SugarFeed') . ' ';
                $result .= $days . ' ' . translate('LBL_TIME_DAYS', 'SugarFeed') . ' ';
            }
        } else {
            if ($days == 1) {
                return translate('LBL_TIME_YESTERDAY', 'SugarFeed') . ' ';
            } elseif ($days > 1) {
                $result .= $days . ' ' . translate('LBL_TIME_DAYS', 'SugarFeed') . ' ';
            } else {
                if ($hours == 1) {
                    $result .= $hours . ' ' . translate('LBL_TIME_HOUR', 'SugarFeed') . ' ';
                } else {
                    $result .= $hours . ' ' . translate('LBL_TIME_HOURS', 'SugarFeed') . ' ';
                }
                if ($hours < 6) {
                    if ($minutes == 1) {
                        $result .= $minutes . ' ' . translate('LBL_TIME_MINUTE', 'SugarFeed') . ' ';
                    } else {
                        $result .= $minutes . ' ' . translate('LBL_TIME_MINUTES', 'SugarFeed') . ' ';
                    }
                }
                if ($hours == 0 && $minutes == 0) {
                    if ($seconds == 1) {
                        $result = $seconds . ' ' . translate('LBL_TIME_SECOND', 'SugarFeed');
                    } else {
                        $result = $seconds . ' ' . translate('LBL_TIME_SECONDS', 'SugarFeed');
                    }
                }
            }
        }

        return $result . ' ' . translate('LBL_TIME_AGO', 'SugarFeed');
    }

    /**
     * Parse a piece of text and replace with proper display tags.
     * @static
     * @param  $input
     * @return string
     */
    public static function parseMessage($input)
    {
        $urls = getUrls($input);
        foreach ($urls as $url) {
            $output = "<a href='$url' target='_blank'>".$url."</a>";
            $input = str_replace($url, $output, (string) $input);
        }
        return $input;
    }
}
