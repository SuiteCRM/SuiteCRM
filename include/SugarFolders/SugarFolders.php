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

require_once __DIR__ . '/../ytree/Tree.php';
require_once __DIR__ . '/../ytree/ExtNode.php';
require_once __DIR__ . '/SugarFolderEmptyException.php';
require_once __DIR__ . '/../TimeDate.php';

/**
 * Polymorphic buckets - place any item in a folder
 */
class SugarFolder
{
    /**
     *
     * @var User
     */
    protected $currentUser     = null;

    /**
     *
     * @var array
     */
    protected $appStrings      = null;

    /**
     *
     * @var array
     */
    protected $modStrings      = null;

    /**
     *
     * @var array
     */
    protected $sugarConfig     = null;

    /**
     *
     * @var timedate
     */
    protected $timeDate        = null;

    /**
     *
     * @var array
     */
    protected $beanList        = null;

    /**
     *
     * @var array
     */
    protected $currentLanguage = null;


    // Public attributes
    public $id;
    public $name;
    public $parent_folder;
    public $has_child = 0; // flag node has child
    public $is_group = 0;
    public $is_dynamic = 0;
    public $dynamic_query = '';
    public $assign_to_id;
    public $created_by;
    public $modified_by;
    public $date_created;
    public $date_modified;
    public $deleted;
    public $folder_type;

    public $db;
    public $new_with_id = true;

    // Core queries
    public $core;
    public $coreSubscribed;
    public $coreWhere;
    public $coreWhereSubscribed;
    public $coreOrderBy;

    public $hrSortLocal;
    public $defaultSort = 'date';
    public $defaultDirection = "DESC";

    // Private attributes
    public $_depth;

    /**
     * Sole constructor
     */
    public function __construct(
        User $current_user = null,
        $app_strings       = null,
        $mod_strings       = null,
        $timedate          = null,
        $bean_list         = null,
        $sugar_config      = null,
        $current_language  = null
    ) {
        $this->currentUser     = $current_user;
        $this->appStrings      = $app_strings;
        $this->modStrings      = $mod_strings;
        $this->timeDate        = $timedate;
        $this->beanList        = $bean_list;
        $this->sugarConfig     = $sugar_config;
        $this->currentLanguage = $current_language;

        if (empty($this->currentUser)) {
            global $current_user;
            $this->currentUser = $current_user;
        }

        if (empty($this->appStrings)) {
            global $app_strings;
            $this->appStrings = $app_strings;
        }

        if (empty($this->modStrings)) {
            global $mod_strings;
            $this->modStrings = $mod_strings;
        }

        if (empty($this->timeDate)) {
            global $timedate;
            $this->timeDate = $timedate;
        }

        if (empty($this->beanList)) {
            global $beanList;
            $this->beanList = $beanList;
        }

        if (empty($this->sugarConfig)) {
            global $sugar_config;
            $this->sugarConfig = $sugar_config;
        }

        if (empty($this->currentLanguage)) {
            global $current_language;
            $this->currentLanguage = $current_language;
        }

        $this->db = DBManagerFactory::getInstance();

        $this->core = "SELECT f.id, f.name, f.has_child, f.is_group, f.is_dynamic, f.dynamic_query," .
        " f.folder_type, f.created_by, f.deleted FROM folders f ";
        $this->coreSubscribed = "SELECT f.id, f.name, f.has_child, f.is_group, f.is_dynamic,".
        " f.dynamic_query, f.folder_type, f.created_by, f.deleted FROM folders f LEFT JOIN folders_subscriptions".
        " fs ON f.id = fs.folder_id ";
        $this->coreWhere = "WHERE f.deleted != 1 ";
        $this->coreWhereSubscribed = "WHERE f.deleted != 1 AND fs.assigned_user_id = ";
        $this->coreOrderBy = " ORDER BY f.is_dynamic, f.is_group, f.name ASC ";
        $this->hrSortLocal = array(
            'flagged'    => 'type',
            'status'     => 'reply_to_status',
            'from'       => 'emails_text.from_addr',
            'subject'    => 'name',
            'date'       => 'date_sent_received',
            'AssignedTo' => 'assigned_user_id',
            'flagged'    => 'flagged'
        );
    }

    /**
     * Delete the email from the all folders
     *
     * @param  int $id
     * @return boolean
     */
    public function deleteEmailFromAllFolder($id)
    {
        $query = "DELETE FROM folders_rel WHERE polymorphic_module = 'Emails' AND polymorphic_id = " . $this->db->quoted($id);

        return $this->db->query($query);
    }

    /**
     * Delete Email From Folder
     *
     * @param  string $id
     * @return boolean
     */
    public function deleteEmailFromFolder($id)
    {
        $query = "DELETE FROM folders_rel " .
             "WHERE polymorphic_module = 'Emails' " .
             "AND polymorphic_id = " . $this->db->quoted($id) . " " .
             "AND folder_id = " . $this->db->quoted($this->id);

        return $this->db->query($query);
    }

    /**
     * Check an email exists for folder
     *
     * @param  string $id
     * @return boolean
     */
    public function checkEmailExistForFolder($id)
    {
        $query = "SELECT COUNT(*) c FROM folders_rel WHERE polymorphic_module = 'Emails' AND polymorphic_id = " . $this->db->quoted($id) .
            " AND folder_id = " . $this->db->quoted($this->id);

        $res = $this->db->query($query);
        $a = $this->db->fetchByAssoc($res);

        return $a['c'] > 0;
    }

    /**
     * Moves beans from one folder to another folder
     *
     * @param string fromFolder GUID of source folder
     * @param string toFolder GUID of destination folder
     * @param string beanId GUID of SugarBean being moved
     * @return boolean
     */
    public function move($fromFolder, $toFolder, $beanId)
    {
        $query = "UPDATE folders_rel SET folder_id = " . $this->db->quoted($toFolder) . " " .
             "WHERE folder_id = " . $this->db->quoted($fromFolder) . " " .
             "AND polymorphic_id = " . $this->db->quoted($beanId) . " AND deleted = 0";

        return $this->db->query($query);
    }

    /**
     * Copies one bean from one folder to another
     *
     * @param  string $fromFolder
     * @param  string $toFolder
     * @param  string $beanId
     * @param  string $module
     * @return boolean
     */
    public function copyBean($fromFolder, $toFolder, $beanId, $module)
    {
        $guid = create_guid();

        $query = "INSERT INTO folders_rel (id, folder_id, polymorphic_module, polymorphic_id, deleted) " .
              "VALUES(" . $this->db->quoted($guid) . ", " . $this->db->quoted($toFolder) .
              ", " . $this->db->quoted($module) . ", " . $this->db->quoted($beanId) . ", 0)";

        return $this->db->query($query);
    }

    /**
     * Creates a new group Folder from the passed fields
     *
     * @param array fields
     * @return boolean
     */
    public function setFolder($fields)
    {
        if (empty($fields['groupFoldersUser'])) {
            $fields['groupFoldersUser'] = $this->currentUser->id;
        }

        $this->name = $fields['name'];
        $this->parent_folder = $fields['parent_folder'];
        $this->has_child = 0;
        $this->is_group = 1;
        $this->assign_to_id = $fields['groupFoldersUser'];

        return $this->save();
    }

    /**
     * Returns GUIDs of folders that the user in focus is subscribed to
     *
     * @param User User object in focus
     * @return array
     */
    public function getSubscriptions($user)
    {
        if (empty($user)) {
            $user = $this->currentUser;
        }

        $query = "SELECT folder_id FROM folders_subscriptions WHERE assigned_user_id = " . $this->db->quoted($user->id);
        $r = $this->db->query($query);

        $ret = array();

        while ($a = $this->db->fetchByAssoc($r)) {
            $ret[] = $a['folder_id'];
        }

        return $ret;
    }

    /**
     * Sets a user's preferences for subscribe folders (Sugar only)
     *
     * @param array   Sub Array of IDs for subscribed folders
     * @param User    Specify which user to set the subscriptions
     * @return void
     */
    public function setSubscriptions($subs, $user = null)
    {
        if (null === $user) {
            $user = $this->currentUser;
        }

        if (empty($user->id)) {
            $GLOBALS['log']->fatal("*** FOLDERS: tried to update folder subscriptions for a user with no ID");
            return false;
        }

        $cleanSubscriptions = array();

        // Remove the duplications
        $subs = array_unique($subs);

        // Ensure parent folders are selected, regardless.
        foreach ($subs as $id) {
            $id = trim($id);
            if (!empty($id)) {
                $cleanSubscriptions[] = $id;
                $queryChk = "SELECT parent_folder FROM folders WHERE id = " . $this->db->quoted($id);
                $rChk = $this->db->query($queryChk);
                $aChk = $this->db->fetchByAssoc($rChk);

                if (!empty($aChk['parent_folder'])) {
                    $cleanSubscriptions = $this->getParentIDRecursive($aChk['parent_folder'], $cleanSubscriptions);
                }
            }
        }

        $this->clearSubscriptions($user);

        foreach ($cleanSubscriptions as $id) {
            $this->insertFolderSubscription($id, $user->id);
        }
    }

    /**
     * Given a folder id and user id, create a folder subscription entry.
     *
     * @param string $folderId
     * @param string $userID
     * @return string The id of the newly created folder subscription.
     */
    public function insertFolderSubscription($folderId, $userID)
    {
        $guid = create_guid();
        $query = "INSERT INTO folders_subscriptions" .
            " (id, folder_id, assigned_user_id) VALUES (" .
            $this->db->quoted($guid) . ", " . $this->db->quoted($folderId) . ", " . $this->db->quoted($userID) . ')';

        $r = $this->db->query($query);

        return $guid;
    }

    /**
     * Recursively finds parent node until it hits root
     *
     * @param string id Starting id to follow up
     * @param array ret collected ids
     * @return array of IDs
     */
    public function getParentIDRecursive($id, $ret = array())
    {
        $query = "SELECT * FROM folders WHERE id = " . $this->db->quoted($id) . " AND deleted = 0";
        $r = $this->db->query($query);
        $a = $this->db->fetchByAssoc($r);

        if (!in_array($id, $ret)) {
            $ret[] = $id;
        }

        if ($a['parent_folder'] != '') {
            $queryChk = "SELECT parent_folder FROM folders WHERE id = " . $this->db->quoted($id);
            $rChk = $this->db->query($queryChk);
            $aChk = $this->db->fetchByAssoc($rChk);

            if (!empty($aChk['parent_folder'])) {
                $ret = $this->getParentIDRecursive($aChk['parent_folder'], $ret);
            }
        }

        return $ret;
    }

    /**
     * Deletes subscriptions to folders in preparation for reset
     *
     * @param User|null $user User
     * @return boolean
     */
    public function clearSubscriptions($user = null)
    {
        if (!$user) {
            $user = $this->currentUser;
        }

        if (!empty($user->id)) {
            $query = "DELETE FROM folders_subscriptions WHERE assigned_user_id = " . $this->db->quoted($user->id);
            $r = $this->db->query($query);
        }
    }

    /**
     * Deletes all subscriptions for a particular folder id
     * @param  string $folder_id
     * @return boolean
     */
    public function clearSubscriptionsForFolder($folder_id)
    {
        $query = "DELETE FROM folders_subscriptions WHERE folder_id = "  . $this->db->quoted($folder_id);

        return $this->db->query($query);
    }

    /**
     * Get the Generate Archive Folder Query
     *
     * @return string
     */
    public function generateArchiveFolderQuery()
    {
        return "SELECT emails.id , emails.name, emails.date_sent_received, emails.status, emails.type, emails.flagged, " .
            "emails.reply_to_status, emails_text.from_addr, emails_text.to_addrs, 'Emails' polymorphic_module " .
            "FROM emails " .
            "JOIN emails_text on emails.id = emails_text.email_id " .
            "JOIN emails_email_addr_rel eear ON eear.email_id = emails.id " .
            "JOIN email_addr_bean_rel eabr ON eabr.email_address_id=eear.email_address_id " .
            "WHERE emails.deleted=0 AND emails.type NOT IN ('out', 'draft') AND emails.status NOT IN ('sent', 'draft') " .
            "AND eabr.bean_id = " . $this->db->quoted($this->currentUser->id) . " AND eabr.bean_module = 'Users' " .
            "AND eear.deleted=0 " .
            "GROUP BY id";
    }

    public function generateSugarsDynamicFolderQuery()
    {
        $type = $this->folder_type;

        if ($type == 'archived') {
            return $this->generateArchiveFolderQuery();
        }

        $status = $type;

        if ($type == "sent") {
            $type = "out";
        }

        if ($type == 'inbound') {
            $ret = " AND emails.status NOT IN ('sent', 'archived', 'draft') AND emails.type NOT IN ('out', 'archived', 'draft')";
        } else {
            $ret = " AND emails.status NOT IN ('archived') AND emails.type NOT IN ('archived')";
        }

        $query = "SELECT emails.id, emails.name, emails.date_sent_received, emails.status, emails.type, emails.flagged,".
            " emails.reply_to_status, emails_text.from_addr, emails_text.to_addrs, ".
            "'Emails' polymorphic_module FROM emails" .
            " JOIN emails_text on emails.id = emails_text.email_id WHERE (type = " . $this->db->quoted($type) . " OR status = " . $this->db->quoted($status) . ")" .
            " AND assigned_user_id = " . $this->db->quoted($this->currentUser->id) . " AND emails.deleted = 0";

        return $query . $ret;
    }


    /**
     * returns array of items for listView display in yui-ext Grid
     */
    public function getListItemsForEmailXML($folderId, $page = 1, $pageSize = 10, $sort = '', $direction = '')
    {
        $this->retrieve($folderId);

        $start = ($page - 1) * $pageSize;

        $sort = (empty($sort)) ? $this->defaultSort : $sort;
        if (!in_array(strtolower($direction), array('asc', 'desc'))) {
            $direction = $this->defaultDirection;
        }

        if (!empty($this->hrSortLocal[$sort])) {
            $order = " ORDER BY " . $this->db->quoted($this->hrSortLocal[$sort]) . " {$direction}";
        } else {
            $order = "";
        }

        if ($this->is_dynamic) {
            $r = $this->db->limitQuery(
                from_html($this->generateSugarsDynamicFolderQuery() . $order),
                $start,
                $pageSize
            );
        } else {
            // get items and iterate through them
            $query = "SELECT emails.id , emails.name, emails.date_sent_received, emails.status, emails.type, emails.flagged,".
                " emails.reply_to_status, emails_text.from_addr, emails_text.to_addrs,".
                " 'Emails' polymorphic_module FROM emails JOIN folders_rel ON emails.id = folders_rel.polymorphic_id" .
                " JOIN emails_text on emails.id = emails_text.email_id
                  WHERE folders_rel.folder_id = " . $this->db->quoted($folderId) . " AND folders_rel.deleted = 0 AND emails.deleted = 0";
            if ($this->is_group) {
                $query = $query . " AND (emails.assigned_user_id is null or emails.assigned_user_id = '')";
            }
            $r = $this->db->limitQuery($query . $order, $start, $pageSize);
        }

        $return = array();

        $email = BeanFactory::newBean('Emails'); //Needed for email specific functions.

        while ($a = $this->db->fetchByAssoc($r)) {
            $temp = array();
            $temp['flagged']   = (is_null($a['flagged']) || $a['flagged'] == '0') ? '' : 1;
            $temp['status']    = (is_null($a['reply_to_status']) || $a['reply_to_status'] == '0') ? '' : 1;
            $temp['from']      = preg_replace('/[\x00-\x08\x0B-\x1F]/', '', $a['from_addr']);
            $temp['subject']   = $a['name'];
            $temp['date']      = $this->timeDate->to_display_date_time($this->db->fromConvert($a['date_sent_received'], 'datetime'));
            $temp['uid']       = $a['id'];
            $temp['mbox']      = 'sugar::' . $a['polymorphic_module'];
            $temp['ieId']      = $folderId;
            $temp['site_url']  = $this->sugarConfig['site_url'];
            $temp['seen']      = ($a['status'] == 'unread') ? 0 : 1;
            $temp['type']      = $a['type'];
            $temp['hasAttach'] = $email->doesImportedEmailHaveAttachment($a['id']);
            $temp['to_addrs']  = preg_replace('/[\x00-\x08\x0B-\x1F]/', '', $a['to_addrs']);
            $return[] = $temp;
        }


        $metadata = array();
        $metadata['mbox'] = $this->appStrings['LBL_EMAIL_SUITE_FOLDER'] . ': ' . $this->name;
        $metadata['ieId'] = $folderId;
        $metadata['name'] = $this->name;
        $metadata['unreadChecked'] = ($this->currentUser->getPreference('showUnreadOnly', 'Emails') == 1) ? 'CHECKED' : '';
        $metadata['out'] = $return;

        return $metadata;
    }

    /**
     * Get the count of items for dynamic folder
     *
     * @param bool $unread
     * @return int
     */
    public function getDynamicFolderCount($unread = false)
    {
        $selectQuery = $this->generateSugarsDynamicFolderQuery();
        $pattern = '/SELECT(.*?)(\s){1}FROM(\s){1}/is';  // ignores the case

        if ($this->folder_type === 'archived') {
            $replacement = 'SELECT count(DISTINCT emails.id) c FROM ';
            $modifiedSelectQuery = preg_replace($pattern, $replacement, $selectQuery, 1);

            // remove GROUP BY statement
            $pattern = '/GROUP BY id(\s)?/s';
            $modifiedSelectQuery = preg_replace($pattern, '', $modifiedSelectQuery, 1);
        } else {
            $replacement = 'SELECT count(*) c FROM ';
            $modifiedSelectQuery = preg_replace($pattern, $replacement, $selectQuery, 1);
        }

        $query = from_html($modifiedSelectQuery);

        if ($unread) {
            $query .= " AND emails.status = 'unread'";
        }

        $res = $this->db->query($query);

        $result = $this->db->fetchByAssoc($res);

        return $result['c'];
    }

    /**
     * Get the count of items
     *
     * @param string $folderId
     * @return int
     */
    public function getCountItems($folderId)
    {
        $this->retrieve($folderId);

        if ($this->is_dynamic) {
            return $this->getDynamicFolderCount();
        }

        // Get items and iterate through them
        $query = "SELECT count(*) c FROM folders_rel JOIN emails ON emails.id = folders_rel.polymorphic_id" .
            " WHERE folder_id = " . $this->db->quoted($folderId) . " AND folders_rel.deleted = 0 AND emails.deleted = 0";

        if ($this->is_group) {
            $query .= " AND (emails.assigned_user_id is null or emails.assigned_user_id = '')";
        }

        $res = $this->db->query($query);

        $result = $this->db->fetchByAssoc($res);

        return $result['c'];
    }

    /**
     * Get a count of the Unread Items
     *
     * @param string $folderId
     * @return integer
     */
    public function getCountUnread($folderId)
    {
        $this->retrieve($folderId);

        if ($this->is_dynamic) {
            return $this->getDynamicFolderCount(true);
        }

        // Get items and iterate through them
        $query = "SELECT count(*) c FROM folders_rel fr JOIN emails on fr.folder_id = " . $this->db->quoted($folderId) .
            " AND fr.deleted = 0 " .
            "AND fr.polymorphic_id = emails.id AND emails.status = 'unread' AND emails.deleted = 0";

        if ($this->is_group) {
            $query .= " AND (emails.assigned_user_id is null or emails.assigned_user_id = '')";
        }

        $res = $this->db->query($query);

        $result = $this->db->fetchByAssoc($res);

        return $result['c'];
    }


    /**
     * Convenience method, pass a SugarBean
     *
     * @param SugarBean $bean
     *
     * @return boolean
     */
    public function addBean(SugarBean $bean)
    {
        if (empty($bean->id) || empty($bean->module_dir)) {
            $GLOBALS['log']->fatal("*** FOLDERS: addBean() got empty bean - not saving");
            return false;
        }

        if (empty($this->id)) {
            $GLOBALS['log']->fatal("*** FOLDERS: addBean() is trying to save to a non-saved or non-existent folder");
            return false;
        }

        /*
        Fix issue #9192 - Duplicating rows for folders_rel
        First check if a row with the same data already exists
        If so, return false
        */

        $q = "SELECT id FROM folders_rel WHERE".
            " folder_id = ".$this->db->quoted($this->id).
            " AND polymorphic_module = ".$this->db->quoted($bean->module_dir).
            " AND polymorphic_id = ".$this->db->quoted($bean->id).
            " AND deleted = 0";

        $result = $this->db->fetchByAssoc($this->db->query($q));

        if($result) {
            $GLOBALS['log']->debug("*** FOLDERS: addBean() is trying to create an already existing relationship");
            return false;
        }

        $guid = create_guid();

        $query = "INSERT INTO folders_rel " .
            "(id, folder_id, polymorphic_module, polymorphic_id, deleted) VALUES (" .
            $this->db->quoted($guid) . ", " .
            $this->db->quoted($this->id) . ", " .
            $this->db->quoted($bean->module_dir) . ", " .
            $this->db->quoted($bean->id) . ", 0)";

        return $this->db->query($query);
    }

    /**
     * Builds up a metacollection of user/group folders to be passed to processor methods
     *
     * @param User $user  Defaults to $current_user
     * @return array      Array of abstract folder objects
     * @throws \SugarFolderEmptyException
     */
    public function retrieveFoldersForProcessing($user, $subscribed = true)
    {
        $myEmailTypeString     = 'inbound';
        $myDraftsTypeString    = 'draft';
        $mySentEmailTypeString = 'sent';
        $myArchiveTypeString   = 'archived';

        if (empty($user)) {
            $user = $this->currentUser;
        }

        $rootWhere = '';
        $teamSecurityClause = '';
        $rootWhere .= " AND (f.parent_folder IS NULL OR f.parent_folder = '')";

        if ($subscribed) {
            $query = $this->coreSubscribed . $teamSecurityClause .
                $this->coreWhereSubscribed . $this->db->quoted($user->id) . $rootWhere . $this->coreOrderBy;
        } else {
            $query = $this->core . $teamSecurityClause . $this->coreWhere . $rootWhere . $this->coreOrderBy;
        }

        $res = $this->db->query($query);

        $return = array();

        $found = array();

        while ($a = $this->db->fetchByAssoc($res)) {
            if (!empty($a['folder_type']) &&
                $a['folder_type'] !== $myArchiveTypeString
            ) {
                if (!isset($found[$a['id']])) {
                    $found[$a['id']] = true;

                    $children = $this->db->query("SELECT * FROM folders WHERE parent_folder = '" . $a['id'] . "'");
                    while ($b = $this->db->fetchByAssoc($children)) {
                        $a['children'][] = $b;
                    }

                    $return[] = $a;
                } elseif ($found[$a['id']] === true) {
                    LoggerManager::getLogger()->error('Duplicated folder detected: ' . $a['id']);
                }
            }
        }

        if (empty($found)) {
            LoggerManager::getLogger()->error(
                ' SugarFolder::retrieveFoldersForProcessing() Cannot Retrieve Folders - '.
                'Please check the users inbound email settings.'
            );
        }

        $secureReturn = [];

        $userAccessibleInboundIds = $this->getUserAccessibleInboundIds($user);

        foreach ($return as $item) {
            if (empty($item) || empty($item['id'])) {
                continue;
            }

            $isGroup = $item['isgroup'] ?? '';
            if ($isGroup === 1) {
                $secureReturn[] = $item;
            }

            if (!empty($userAccessibleInboundIds[$item['id']])) {
                $secureReturn[] = $item;
            }
        }

        return $secureReturn;
    }

    /**
     * Preps object array for async call from user's Settings->Folders
     *
     * @param  User|null $focusUser
     * @return array
     */
    public function getGroupFoldersForSettings($focusUser = null)
    {
        $grp = array();

        $folders = $this->retrieveFoldersForProcessing($focusUser, false);
        $subscriptions = $this->getSubscriptions($focusUser);

        foreach ($folders as $a) {
            $a['selected'] = (in_array($a['id'], $subscriptions)) ? true : false;
            $a['origName'] = $a['name'];

            if ($a['is_group'] == 1) {
                if ($a['deleted'] != 1) {
                    $grp[] = $a;
                }
            }
        }

        return $grp;
    }

    /**
     * Preps object array for async call from user's Settings->Folders
     *
     * @param  User|null $focusUser
     * @return array
     */
    public function getFoldersForSettings($focusUser = null)
    {
        $user = array();
        $grp  = array();

        $user[] = array(
            'id'        => '',
            'name'      => $this->appStrings['LBL_NONE'],
            'has_child' => 0,
            'is_group'  => 0,
            'selected'  => false
        );

        $grp[] = array(
            'id'        => '',
            'name'      => $this->appStrings['LBL_NONE'],
            'has_child' => 0,
            'is_group'  => 1,
            'selected'  => false,
            'origName'  => ""
        );

        try {
            $folders = $this->retrieveFoldersForProcessing($focusUser, false);
            $subscriptions = $this->getSubscriptions($focusUser);

            foreach ($folders as $a) {
                $a['selected'] = (in_array($a['id'], $subscriptions)) ? true : false;
                $a['origName'] = $a['name'];

                if (isTrue($a['deleted'] ?? false)) {
                    continue;
                }

                if (isset($a['dynamic_query'])) {
                    unset($a['dynamic_query']);
                }

                if ($a['is_group'] == 1) {
                    $grp[] = $a;
                } else {
                    $user[] = $a;
                }

                if ($a['has_child'] == 1) {
                    $qGetChildren = $this->core . $this->coreWhere . "AND parent_folder = " . $this->db->quoted($a['id']);
                    $rGetChildren = $this->db->query($qGetChildren);

                    while ($aGetChildren = $this->db->fetchByAssoc($rGetChildren)) {
                        if ($a['is_group']) {
                            $this->_depth = 1;
                            $grp = $this->getFoldersChildForSettings($aGetChildren, $grp, $subscriptions);
                        } else {
                            $this->_depth = 1;
                            $user = $this->getFoldersChildForSettings($aGetChildren, $user, $subscriptions);
                        }
                    }
                }
            }
        } catch (SugarFolderEmptyException $e) {
            // And empty sugar folder exception is ok in this case.
        }

        $user = $this->removeDeletedFolders($user);

        $ret = array(
            'userFolders'  => $user,
            'groupFolders' => $grp,
        );

        return $ret;
    }

    /**
     * Remove folders of deleted inbounds
     *
     * @param array $folders - array of folders table rows
     * @return array
     */
    private function removeDeletedFolders($folders)
    {
        $ret = array();

        foreach ($folders as $folder) {
            $correct = false;

            if (!$folder['id']) {
                $correct = true;
            }

            $ie = BeanFactory::getBean('InboundEmail', $folder['id']);

            if ($ie) {
                $correct = true;
            }

            if ($correct) {
                $ret[] = $folder;
            }
        }

        return $ret;
    }

    /**
     * Gets all child folders for settings
     *
     * @param  array $a
     * @param  array $collection
     * @param  array $subscriptions
     * @return array
     */
    public function getFoldersChildForSettings($a, $collection, $subscriptions)
    {
        $a['selected'] = (in_array($a['id'], $subscriptions)) ? true : false;
        $a['origName'] = $a['name'];

        if (isset($a['dynamic_query'])) {
            unset($a['dynamic_query']);
        }

        for ($i = 0; $i < $this->_depth; $i++) {
            $a['name'] = "." . $a['name'];
        }

        $collection[] = $a;

        if ($a['has_child'] == 1) {
            $this->_depth++;
            $qGetChildren = $this->core . $this->coreWhere . " AND parent_folder = " . $this->db->quoted($a['id']);
            $rGetChildren = $this->db->query($qGetChildren);

            while ($aGetChildren = $this->db->fetchByAssoc($rGetChildren)) {
                $collection = $this->getFoldersChildForSettings($aGetChildren, $collection, $subscriptions);
            }
        }

        return $collection;
    }


    /**
     * Returns the number of "new" items (based on passed criteria)
     *
     * @param string id ID of folder
     * @param array criteria
     *        expected:
     *        array('field' => 'status',
     *                'value' => 'unread');
     * @param array
     * @return int
     */
    public function getCountNewItems($id, $criteria, $folder)
    {
        $sugarFolder = new SugarFolder();
        return $sugarFolder->getCountUnread($id);
    }


    /**
     * Collects, sorts, and builds tree of user's folders
     *
     * @param object  $rootNode     Reference to tree root node
     * @param array   $folderStates User pref folder open/closed states
     * @param User    $user         Optional User in focus, default current_user
     *
     * @return array
     */
    public function getUserFolders(&$rootNode, $folderStates, $user = null, $forRefresh = false)
    {
        if (empty($user)) {
            $user = $this->currentUser;
        }

        $folders = $this->retrieveFoldersForProcessing($user, true);

        $subscriptions = $this->getSubscriptions($user);

        $refresh = ($forRefresh) ? array() : null;

        if (!is_array($folderStates)) {
            $folderStates = array();
        }

        $settingsFolders = $this->getFoldersForSettings($user);

        $selectedFolders = [];

        foreach ($folders as $folder) {
            if ($this->isToDisplay($folder['id'] ?? '', $settingsFolders)){
                $selectedFolders[] = $folder;
            }
        }

        foreach ($selectedFolders as $a) {
            if ($a['deleted'] == 1) {
                continue;
            }
            $label = ($a['name'] == 'My Email' ? $this->modStrings['LNK_MY_INBOX'] : $a['name']);

            $folderNode = new ExtNode($a['id'], $label);
            $folderNode->dynamicloadfunction = '';
            $folderNode->expanded = false;

            if (array_key_exists('Home::' . $a['id'], $folderStates)) {
                if ($folderStates['Home::' . $a['id']] == 'open') {
                    $folderNode->expanded = true;
                }
            }
            $nodePath = "Home::" . $folderNode->_properties['id'];

            $folderNode->dynamic_load = true;
            $folderNode->set_property('ieId', 'folder');
            $folderNode->set_property('is_group', ($a['is_group'] == 1) ? 'true' : 'false');
            $folderNode->set_property('is_dynamic', ($a['is_dynamic'] == 1) ? 'true' : 'false');
            $folderNode->set_property('mbox', $folderNode->_properties['id']);
            $folderNode->set_property('id', $a['id']);
            $folderNode->set_property('folder_type', $a['folder_type']);
            $folderNode->set_property('children', array());

            if (in_array($a['id'], $subscriptions) && $a['has_child'] == 1) {
                $qGetChildren = $this->core . $this->coreWhere . "AND parent_folder = " . $this->db->quoted($a['id']);
                $rGetChildren = $this->db->query($qGetChildren);

                while ($aGetChildren = $this->db->fetchByAssoc($rGetChildren)) {
                    if (in_array($aGetChildren['id'], $subscriptions)) {
                        $folderNode->add_node(
                            $this->buildTreeNodeFolders(
                                $aGetChildren,
                                $nodePath,
                                $folderStates,
                                $subscriptions
                            )
                        );
                    }
                }
            }

            if (is_null($rootNode)) {
                $guid = create_guid();
                $label = 'Parent';
                $rootNode = new ExtNode($guid, $label);
            }

            $rootNode->add_node($folderNode);
        }

        /* the code below is called only by Settings->Folders when selecting folders to subscribe to */
        if ($forRefresh) {
            $metaNode = array();

            if (!empty($rootNode->nodes)) {
                foreach ($rootNode->nodes as $node) {
                    $metaNode[] = $this->buildTreeNodeRefresh($node, $subscriptions);
                }
            }

            return $metaNode;
        }
    }

    /**
     * Builds up a metanode for folder refresh (Sugar folders only)
     *
     * @param  string $folderNode
     * @param  array  $subscriptions
     * @return array
     */
    public function buildTreeNodeRefresh($folderNode, $subscriptions)
    {
        $metaNode = $folderNode->_properties;
        $metaNode['expanded'] = $folderNode->expanded;
        $metaNode['text'] = $folderNode->_label;

        if ($metaNode['is_group'] == 'true') {
            $metaNode['cls'] = 'groupFolder';
        } else {
            $metaNode['cls'] = 'sugarFolder';
        }

        $metaNode['id'] = $folderNode->_properties['id'];
        $metaNode['children'] = array();
        $metaNode['type'] = 1;
        $metaNode['leaf'] = false;
        $metaNode['isTarget'] = true;
        $metaNode['allowChildren'] = true;

        if (!empty($folderNode->nodes)) {
            foreach ($folderNode->nodes as $node) {
                if (in_array($node->_properties['id'], $subscriptions)) {
                    $metaNode['children'][] = $this->buildTreeNodeRefresh($node, $subscriptions);
                }
            }
        }

        return $metaNode;
    }

    /**
     * Builds children nodes for folders for TreeView
     * @return $folderNode TreeView node
     */
    public function buildTreeNodeFolders($a, $nodePath, $folderStates, $subscriptions)
    {
        $label = $a['name'];

        if ($a['name'] == 'My Drafts') {
            $label = $this->modStrings['LBL_LIST_TITLE_MY_DRAFTS'];
        }

        if ($a['name'] == 'Sent Emails') {
            $label = $this->modStrings['LBL_LIST_TITLE_MY_SENT'];
        }

        $folderNode = new ExtNode($a['id'], $label);
        $folderNode->dynamicloadfunction = '';
        $folderNode->expanded = false;

        $nodePath .= "::{$a['id']}";

        if (array_key_exists($nodePath, $folderStates)) {
            if ($folderStates[$nodePath] == 'open') {
                $folderNode->expanded = true;
            }
        }

        $folderNode->dynamic_load = true;

        $folderNode->set_property(
            'click',
            "SUGAR.email2.listView.populateListFrameSugarFolder(".
            "YAHOO.namespace('frameFolders').selectednode, '{$a['id']}', 'false');"
        );

        $folderNode->set_property('ieId', 'folder');
        $folderNode->set_property('mbox', $a['id']);
        $folderNode->set_property('is_group', ($a['is_group'] == 1) ? 'true' : 'false');
        $folderNode->set_property('is_dynamic', ($a['is_dynamic'] == 1) ? 'true' : 'false');
        $folderNode->set_property('folder_type', $a['folder_type']);

        if (in_array($a['id'], $subscriptions) && $a['has_child'] == 1) {
            $qGetChildren = $this->core . $this->coreWhere . "AND parent_folder = " . $this->db->quoted($a['id']) . $this->coreOrderBy;
            $rGetChildren = $this->db->query($qGetChildren);

            while ($aGetChildren = $this->db->fetchByAssoc($rGetChildren)) {
                $folderNode->add_node(
                    $this->buildTreeNodeFolders(
                        $aGetChildren,
                        $nodePath,
                        $folderStates,
                        $subscriptions
                    )
                );
            }
        }

        return $folderNode;
    }

    /**
     * Flags a folder as deleted
     *
     * @return bool True on success
     */
    public function delete()
    {
        if (!empty($this->id)) {
            if ($this->has_child) {
                $this->deleteChildrenCascade($this->id);
            }

            $ownerCheck = ($this->currentUser->is_admin == 0) ? " AND created_by = " . $this->db->quoted($this->currentUser->id) : "";

            $query = "UPDATE folders SET deleted = 1 WHERE id = " . $this->db->quoted($this->id) . $ownerCheck;
            $r = $this->db->query($query);

            return true;
        }

        return false;
    }

    /**
     * Deletes all children in a cascade
     *
     * @param string $id ID of parent
     * @return bool True on success
     */
    public function deleteChildrenCascade($id)
    {
        $canContinue = true;
        $checkInboundQuery = "SELECT count(*) c FROM inbound_email WHERE groupfolder_id = " . $this->db->quoted($id) . " AND deleted = 0";

        $resultSet = $this->db->query($checkInboundQuery);
        $a = $this->db->fetchByAssoc($resultSet);

        if ($a['c'] > 0) {
            return false;
        }

        $q = "SELECT COUNT(*) c FROM folders_rel WHERE polymorphic_module = 'Emails' ".
            "AND polymorphic_id = " . $this->db->quoted($id) . " AND folder_id = " . $this->db->quoted($this->id);

        $checkEmailQuery = "SELECT count(*) c FROM folders_rel WHERE polymorphic_module = 'Emails' ".
            "AND folder_id = " . $this->db->quoted($id) . " AND deleted = 0";

        $resultSet = $this->db->query($checkEmailQuery);
        $a = $this->db->fetchByAssoc($resultSet);

        if ($a['c'] > 0) {
            return false;
        }

        $query = "SELECT * FROM folders WHERE id = " . $this->db->quoted($id);
        $r = $this->db->query($query);
        $a = $this->db->fetchByAssoc($r);

        if ($a['has_child'] == 1) {
            $query2 = "SELECT id FROM folders WHERE parent_folder = " . $this->db->quoted($id);
            $r2 = $this->db->query($query2);

            while ($a2 = $this->db->fetchByAssoc($r2)) {
                $canContinue = $this->deleteChildrenCascade($a2['id']);
            }
        }

        if ($canContinue) {
            // flag deleted
            $ownerCheck = ($this->currentUser->is_admin == 0) ? " AND created_by = " . $this->db->quoted($this->currentUser->id) . "" : "";

            $query3 = "UPDATE folders SET deleted = 1 WHERE id = " . $this->db->quoted($id) . $ownerCheck;
            $r3 = $this->db->query($query3);

            // flag rels
            $qRel = "UPDATE folders_rel SET deleted = 1 WHERE folder_id = " . $this->db->quoted($id);
            $rRel = $this->db->query($qRel);

            // delete subscriptions
            $qSub = "DELETE FROM folders_subscriptions WHERE folder_id = " . $this->db->quoted($id);
            $rSub = $this->db->query($qSub);
        }

        return $canContinue;
    }

    /**
     * Saves folder
     *
     * @param  boolean $addSubscriptions Add the Subscriptions
     * @return boolean
     */
    public function save($addSubscriptions = true)
    {
        $this->dynamic_query = $this->db->quote($this->dynamic_query);

        if ((!empty($this->id) && $this->new_with_id == false) || (empty($this->id) && $this->new_with_id == true)) {
            if (empty($this->id) && $this->new_with_id == true) {
                $guid = create_guid();
                $this->id = $guid;
            }

            $query = "INSERT INTO folders (id, name, folder_type, parent_folder, has_child, is_group, " .
                 "is_dynamic, dynamic_query, assign_to_id, created_by, modified_by, deleted) VALUES (" .
                    $this->db->quoted($this->id) . ", " .
                    $this->db->quoted($this->name) . ", " .
                    $this->db->quoted($this->folder_type) . ", " .
                    $this->db->quoted($this->parent_folder) . ", " .
                    $this->db->quoted($this->has_child) . ", " .
                    $this->db->quoted($this->is_group) . ", " .
                    $this->db->quoted($this->is_dynamic) . ", " .
                    $this->db->quoted($this->dynamic_query) . ", " .
                    $this->db->quoted($this->assign_to_id) . ", " .
                    $this->db->quoted($this->currentUser->id) . ", " .
                    $this->db->quoted($this->currentUser->id) . ", 0)";

            if ($addSubscriptions) {
                // create default subscription
                $this->addSubscriptionsToGroupFolder();
            }

            // if parent_id is set, update parent's has_child flag
            if (!empty($this->parent_folder)) {
                $query3 = "UPDATE folders SET has_child = 1 WHERE id = " . $this->db->quoted($this->parent_folder);
                $r3 = $this->db->query($query3);
            }
        } else {
            $query = "UPDATE folders SET " .
                "name = " . $this->db->quoted($this->name) . ", " .
                "parent_folder = " . $this->db->quoted($this->parent_folder) . ", " .
                "dynamic_query = " . $this->db->quoted($this->dynamic_query) . ", " .
                "assign_to_id = " . $this->db->quoted($this->assign_to_id) . ", " .
                "modified_by = " . $this->db->quoted($this->currentUser->id) . " " .
                "WHERE id = " . $this->db->quoted($this->id);
        }

        return $this->db->query($query, true);
    }

    /**
     * Add subscriptions to this group folder.
     *
     * @return void
     */
    public function addSubscriptionsToGroupFolder()
    {
        $this->createSubscriptionForUser($this->currentUser->id);
    }


    /**
     * Add subscriptions to this group folder.
     *
     * @return boolean
     */
    public function createSubscriptionForUser($user_id)
    {
        $guid2 = create_guid();

        $query = "INSERT INTO folders_subscriptions VALUES(" .
            $this->db->quoted($guid2) . ", " .
            $this->db->quoted($this->id) . ", " .
            $this->db->quoted($user_id) . ")";

        return $this->db->query($query);
    }


    /**
     * Update the folder
     *
     * @param  array $fields
     * @return array
     */
    public function updateFolder($fields)
    {
        $id            = $fields['record'];
        $name          = $fields['name'];
        $parent_folder = $fields['parent_folder'];

        // first do the retrieve
        $this->retrieve($id);

        if ($this->has_child) {
            $childrenArray = array();
            $this->findAllChildren($id, $childrenArray);
            if (in_array($parent_folder, $childrenArray)) {
                return array('status' => "failed", 'message' => "Can not add this folder to its children");
            }
        }

        // update has_child to 0 for this parent folder if this is the only child it has
        $query1 = "select count(*) count from folders where deleted = 0 AND parent_folder = " . $this->db->quoted($this->parent_folder);
        $r1 = $this->db->query($query1);
        $a1 = $this->db->fetchByAssoc($r1);

        if ($a1['count'] == 1) {
            $query1 = "UPDATE folders SET has_child = 0 WHERE id = " . $this->db->quoted($this->parent_folder);
            $r1 = $this->db->query($query1);
        }

        $this->name = $name;
        $this->parent_folder = $parent_folder;

        $query2 = "UPDATE folders SET name = " . $this->db->query($this->name) . ", parent_folder = " . $this->db->quoted($this->parent_folder) . "," .
            " dynamic_query = " . $this->db->query($this->dynamic_query) . ", " .
            "modified_by = " . $this->db->query($this->currentUser->id) . " WHERE id = " . $this->db->quoted($this->id);

        $r2 = $this->db->query($query2);

        if (!empty($this->parent_folder)) {
            $query3 = "UPDATE folders SET has_child = 1 WHERE id = " . $this->db->quoted($this->parent_folder);
            $r3 = $this->db->query($query3);
        }

        return array('status' => "done");
    }

    /**
     * Find all the children
     *
     * @param  string $folderId
     * @param  array  &$childrenArray
     * @return void
     */
    public function findAllChildren($folderId, &$childrenArray)
    {
        $query = "SELECT * FROM folders WHERE id = " . $this->db->quoted($folderId);
        $r = $this->db->query($query);
        $a = $this->db->fetchByAssoc($r);

        if ($a['has_child'] == 1) {
            $query2 = "SELECT id FROM folders WHERE deleted = 0 AND parent_folder = " . $this->db->quoted($folderId);
            $r2 = $this->db->query($query2);

            while ($a2 = $this->db->fetchByAssoc($r2)) {
                $childrenArray[] = $a2['id'];
                $this->findAllChildren($a2['id'], $childrenArray);
            }
        }
    }

    /**
    * Retrieves and populates object
    *
    * @param string    $id  ID of folder
    * @return boolean       True on success
    */
    public function retrieve($id)
    {
        $query = "SELECT * FROM folders WHERE id = " . $this->db->quoted($id) . " AND deleted = 0";
        $r = $this->db->query($query);
        $a = $this->db->fetchByAssoc($r);

        if (!empty($a)) {
            foreach ($a as $k => $v) {
                if ($k == 'dynamic_query') {
                    $v = from_html($v);
                }
                $this->$k = $v;
            }

            $new_with_id  = false;
            return true;
        }

        return false;
    }

    /**
     * Get first display folder
     * @return mixed|null
     */
    public function getFirstDisplayFolders(): ?array {
        global $current_user;

        $settingsFolders = $this->getFoldersForSettings($current_user);

        foreach ($settingsFolders['userFolders'] as $folder) {
            $isSelected = $folder['selected'] ?? false;
            if (isFalse($isSelected)) {
                continue;
            }

            return $folder;
        }

        foreach ($settingsFolders['groupFolders'] as $folder) {
            $isSelected = $folder['selected'] ?? false;
            if (isFalse($isSelected)) {
                continue;
            }

            return $folder;
        }

        return null;
    }

    /**
     * Check if it subscribed
     * @param string|null $folderId
     * @param array|null $folders
     * @return bool
     */
    public function isToDisplay(?string $folderId, array $folders = null): bool {
        global $current_user;

        if (empty($folderId)){
            return false;
        }

        if ($folders === null){
            $folders = $this->getFoldersForSettings($current_user);
        }

        if ($this->shouldFolderDisplay($folders['userFolders'] ?? [], $folderId)) {
            return true;
        }

        if ($this->shouldFolderDisplay($folders['groupFolders'] ?? [], $folderId)) {
            return true;
        }

        return false;
    }

    /**
     * @param array $folders
     * @param string $folderId
     * @return bool
     */
    protected function shouldFolderDisplay(array $folders, string $folderId): bool
    {
        if (empty($folders)) {
            return false;
        }

        foreach ($folders as $folder) {
            $isSelected = $folder['selected'] ?? false;
            if (isFalse($isSelected)) {
                continue;
            }
            $id = $folder['id'] ?? '';

            if ($id === $folderId) {
                return true;
            }
        }

        return false;
    }

    /**
     * @param User|null $user
     * @return array
     */
    protected function getUserAccessibleInboundIds(?User $user): array
    {
        $userAccessibleInboundAccountIds = [];
        /** @var InboundEmail $inboundEmail */
        $inboundEmail = BeanFactory::newBean('InboundEmail');
        $accessibleInboundEmails = $inboundEmail->getUserInboundAccounts();

        if (!empty($accessibleInboundEmails)) {
            foreach ($accessibleInboundEmails as $accessibleInboundEmail) {
                if (empty($accessibleInboundEmail)) {
                    continue;
                }
                $id = $accessibleInboundEmail->id ?? '';

                if (!empty($id)) {
                    $userAccessibleInboundAccountIds[$id] = true;
                }
            }
        }

        return $userAccessibleInboundAccountIds;
    }
} // end class def
