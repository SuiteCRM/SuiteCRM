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

require_once('include/SugarFolders/SugarFolders.php');

global $current_user, $mod_strings, $app_strings, $log;

$focus = BeanFactory::newBean('InboundEmail');
if (!empty($_REQUEST['record'])) {
    $focus->retrieve($_REQUEST['record']);
} elseif (!empty($_REQUEST['origin_id'])) {
    $focus->retrieve($_REQUEST['origin_id']);
    unset($focus->id);
    unset($focus->groupfolder_id);
}

$isNewRecord = (empty($focus->id) || $focus->new_with_id);

if (!empty($_REQUEST['created_by']) && is_admin($current_user)) {
    $focus->created_by = $_REQUEST['created_by'];
    $focus->set_created_by = false;
}

if ($isNewRecord && !empty($_REQUEST['created_by']) && !is_admin($current_user)) {
    $_REQUEST['created_by'] = '';
    $focus->created_by = '';
}

if (!$isNewRecord && !empty($_REQUEST['created_by']) && !is_admin($current_user)) {
    unset($_REQUEST['created_by']);
}

if ($isNewRecord && empty($focus->created_by)) {
    $focus->created_by = $current_user->id;
}

$ownerId = $current_user->id;
if (!empty($focus->created_by)) {
    $ownerId = $focus->created_by;
}

$owner = BeanFactory::getBean('Users', $ownerId);

foreach ($focus->column_fields as $field) {
    if ($field === 'email_password' && empty($_REQUEST['email_password']) && !empty($_REQUEST['email_user'])) {
        continue;
    }

    if (!isset($_REQUEST[$field])) {
        continue;
    }

    if ($field !== "group_id") {
        if (is_string($_REQUEST[$field])) {
            $focus->$field = trim($_REQUEST[$field]);
        } else {
            $focus->$field = $_REQUEST[$field];
        }
    }
}

foreach ($focus->additional_column_fields as $field) {
    if (!isset($_REQUEST[$field])) {
        continue;
    }
    if (is_string($_REQUEST[$field])) {
        $value = trim($_REQUEST[$field]);
    } else {
        $value = $_REQUEST[$field];
    }

    $focus->$field = $value;
}

foreach ($focus->required_fields as $field) {
    if (!isset($_REQUEST[$field])) {
        continue;
    }

    if (is_string($_REQUEST[$field])) {
        $value = trim($_REQUEST[$field]);
    } else {
        $value = $_REQUEST[$field];
    }

    $focus->$field = $value;
}

$type = $_REQUEST['type'] ?? '';

if (!empty($_REQUEST['email_password'])) {
    $focus->email_password = $_REQUEST['email_password'];
}

$focus->protocol = $_REQUEST['protocol'];

if (isTrue($_REQUEST['is_create_case'] ?? false)) {
    $focus->mailbox_type = 'createcase';
} elseif (empty($focus->mailbox_type) || $focus->mailbox_type === 'createcase') {
    $focus->mailbox_type = 'pick';
}

if ($type === 'personal') {
    $_REQUEST['is_personal'] = 1;
}

if (!empty($_REQUEST['is_personal'])) {
    $focus->is_personal = isTrue($_REQUEST['is_personal']) ? 1 : 0 ;
}

if ((empty($focus->is_personal) || isFalse($focus->is_personal)) && !is_admin($current_user)){
    sugar_die($app_strings['LBL_NO_ACCESS']);
}

if (isTrue($focus->is_personal)) {
    $focus->mailbox_type = 'pick';

    if (empty($focus->group_id) ) {
        $focus->group_id = $owner->id;
    }
}

if ($type === 'bounce') {
    $focus->mailbox_type = 'bounce';
}

if (!empty($_REQUEST['external_oauth_connection_id'])) {
    $externalOauthConnection = BeanFactory::getBean('ExternalOAuthConnection', $_REQUEST['external_oauth_connection_id']);

    $focusType = $focus->type ?? '';
    if ($focusType === 'bounce') {
        $focusType = 'group';
    }

    if ($externalOauthConnection->type !== $focusType) {
        SugarApplication::appendErrorMessage($mod_strings['LBL_TYPE_DIFFERENT']);
        SugarApplication::redirect('index.php?module=InboundEmail&action=EditView&is_personal=1&type=personal');
        return;
    }
}

/////////////////////////////////////////////////////////
////	SERVICE STRING CONCATENATION
$useSsl = isTrue($_REQUEST['is_ssl'] ?? false);
$optimum = $focus->getSessionConnectionString($focus->server_url, $focus->email_user, $focus->port, $focus->protocol);
if (empty($optimum)) {
    $optimum = $focus->findOptimumSettings($useSsl, $focus->email_user, $focus->email_password, $focus->server_url, $focus->port, $focus->protocol, $focus->mailbox);
} // if
$delimiter = $focus->getSessionInboundDelimiterString($focus->server_url, $focus->email_user, $focus->port, $focus->protocol);

//added check to ensure the $optimum['serial']) is not empty.
if (is_array($optimum) && (count($optimum) > 0) && !empty($optimum['serial'])) {
    $focus->service = $optimum['serial'];
} else {
    // no save
    // allowing bad save to allow Email Campaigns configuration to continue even without IMAP
    $focus->service = "::::::".$focus->protocol."::::"; // save bogus info.
    $error = "&error=true";
}
////	END SERVICE STRING CONCAT
/////////////////////////////////////////////////////////

if (isTrue($_REQUEST['mark_read'] ?? false)) {
    $focus->delete_seen = 0;
} else {
    $focus->delete_seen = 0;
}

// handle stored_options serialization
$onlySince = isTrue($_REQUEST['only_since'] ?? false);

$stored_options = array();
$stored_options['from_name'] = trim($_REQUEST['from_name'] ?? '');
$stored_options['from_addr'] = trim($_REQUEST['from_addr'] ?? '');
isValidEmailAddress($stored_options['from_addr']);
$stored_options['reply_to_name'] = trim($_REQUEST['reply_to_name'] ?? '');
$stored_options['reply_to_addr'] = trim($_REQUEST['reply_to_addr'] ?? '');
$stored_options['only_since'] = $onlySince;
$stored_options['filter_domain'] = $_REQUEST['filter_domain'] ?? '';
$stored_options['email_num_autoreplies_24_hours'] = $_REQUEST['email_num_autoreplies_24_hours'] ?? '';
$stored_options['allow_outbound_group_usage'] = isTrue($_REQUEST['allow_outbound_group_usage'] ?? false);
$stored_options['outbound_email'] = $_REQUEST['outbound_email_id'] ?? null;

if (!$focus->isPop3Protocol()) {
    $stored_options['mailbox'] = (isset($_REQUEST['mailbox']) ? trim($_REQUEST['mailbox']) : "");
    $stored_options['trashFolder'] = (isset($_REQUEST['trashFolder']) ? trim($_REQUEST['trashFolder']) : "");
    $stored_options['sentFolder'] = (isset($_REQUEST['sentFolder']) ? trim($_REQUEST['sentFolder']) : "");
} // if
if ($focus->isMailBoxTypeCreateCase() || ($focus->mailbox_type === 'createcase' && empty($_REQUEST['id']))) {
    $stored_options['distrib_method'] = $_REQUEST['distrib_method'] ?? '';
    $stored_options['create_case_email_template'] = $_REQUEST['create_case_template_id'] ?? '';
    switch ($stored_options['distrib_method']) {
        case 'singleUser':
            $stored_options['distribution_user_name'] = !empty($_REQUEST['distribution_user_name']) ? $_REQUEST['distribution_user_name'] : '';
            $stored_options['distribution_user_id'] = !empty($_REQUEST['distribution_user_id']) ? $_REQUEST['distribution_user_id'] : '';
            break;
        case 'roundRobin':
        case 'leastBusy':
        case 'random':
            $stored_options['distribution_options'] = !empty($_REQUEST['distribution_options']) ? $_REQUEST['distribution_options'] : '';
            break;
        default:
            break;
    }
} // if
$storedOptions['folderDelimiter'] = $delimiter;

////////////////////////////////////////////////////////////////////////////////
////    CREATE MAILBOX QUEUE
////////////////////////////////////////////////////////////////////////////////

if (!empty($type) && $type !== 'personal') {
    if (!isset($focus->id)) {
        $groupId = "";
        if (isset($_REQUEST['group_id']) && empty($_REQUEST['group_id'])) {
            $groupId = $_REQUEST['group_id'];
        } else {
            $groupId = create_guid();
        }
        $focus->group_id = $groupId;
    }


    if (isTrue($_REQUEST['is_auto_import'] ?? false)) {
        if (empty($focus->groupfolder_id)) {
            $groupFolderId = $focus->createAutoImportSugarFolder();
            $focus->groupfolder_id = $groupFolderId;
        }
        $stored_options['isAutoImport'] = true;
    } else {
        $focus->groupfolder_id = "";
        //If the user is turning the auto-import feature off then remove all previous subscriptions.
        if (!empty($focus->fetched_row['groupfolder_id'])) {
            $log->debug("Clearing all subscriptions to folder id: {$focus->fetched_row['groupfolder_id']}");
            $f = new SugarFolder();
            $f->clearSubscriptionsForFolder($focus->fetched_row['groupfolder_id']);
            //Now delete the old group folder.
            $f->retrieve($focus->fetched_row['groupfolder_id']);
            $f->delete();
        }
        $stored_options['isAutoImport'] = false;
    }
}

if (!empty($focus->groupfolder_id)) {
    if (isTrue($_REQUEST['move_messages_to_trash_after_import'] ?? false)) {
        $stored_options['leaveMessagesOnMailServer'] = 0;
    } else {
        $stored_options['leaveMessagesOnMailServer'] = 1;
    }
}

$focus->stored_options = base64_encode(serialize($stored_options));
$log->info('----->InboundEmail now saving self');


////////////////////////////////////////////////////////////////////////////////
////    SEND US TO SAVE DESTINATION
////////////////////////////////////////////////////////////////////////////////

//When an admin is creating an IE account we do not want their private team to be added
//or they may be included in a round robin assignment.
$previousTeamAccessCheck = isset($GLOBALS['sugar_config']['disable_team_access_check']) ? $GLOBALS['sugar_config']['disable_team_access_check'] : null;
$GLOBALS['sugar_config']['disable_team_access_check'] = true;

$focus->save();

$showFolders = sugar_unserialize(base64_decode($current_user->getPreference('showFolders', 'Emails')));
if (!is_array($showFolders)) {
    $showFolders = [];
}
if (!in_array($focus->id, $showFolders)) {
    $showFolders[] = $focus->id;
    $showStore = base64_encode(serialize($showFolders));
    $current_user->setPreference('showFolders', $showStore, 0, 'Emails');
}

$idValidator = new \SuiteCRM\Utility\SuiteValidator();

if ($type === 'personal' && isset($_REQUEST['account_signature_id']) && $idValidator->isValidId($_REQUEST['account_signature_id'])) {
    $email_signatures = $owner->getPreference('account_signatures', 'Emails');
    $email_signatures = sugar_unserialize(base64_decode($email_signatures));
    if (empty($email_signatures)) {
        $email_signatures = array();
    }

    $email_signatures[$focus->id] = $_REQUEST['account_signature_id'];
    $owner->setPreference('account_signatures', base64_encode(serialize($email_signatures)), 0, 'Emails');
}


// Folders
$foldersFound = $focus->db->query('SELECT id FROM folders WHERE folders.id LIKE "'.$focus->id.'"');
$foldersFoundRow = $focus->db->fetchRow($foldersFound);
$sf = new SugarFolder();
if (empty($foldersFoundRow)) {
    // Create Folders
    $focusUser = $owner;
    $params = array(
        // Inbox
        "inbound" => array(
            'name' => $focus->mailbox . ' ('.$focus->name.')',
            'folder_type' => "inbound",
            'has_child' => 1,
            'dynamic_query' => '',
            'is_dynamic' => 1,
            'created_by' => $focusUser->id,
            'modified_by' => $focusUser->id,
        ),
        // My Drafts
        "draft" => array(
            'name' => $mod_strings['LNK_MY_DRAFTS'] . ' ('.$stored_options['sentFolder'].')',
            'folder_type' => "draft",
            'has_child' => 0,
            'dynamic_query' => '',
            'is_dynamic' => 1,
            'created_by' => $focusUser->id,
            'modified_by' => $focusUser->id,
        ),
        // Sent Emails
        "sent" => array(
            'name' => $mod_strings['LNK_SENT_EMAIL_LIST'] . ' ('.$stored_options['sentFolder'].')',
            'folder_type' => "sent",
            'has_child' => 0,
            'dynamic_query' => '',
            'is_dynamic' => 1,
            'created_by' => $focusUser->id,
            'modified_by' => $focusUser->id,
        ),
        // Archived Emails
        "archived" => array(
            'name' => $mod_strings['LBL_LIST_TITLE_MY_ARCHIVES'],
            'folder_type' => "archived",
            'has_child' => 0,
            'dynamic_query' => '',
            'is_dynamic' => 1,
            'created_by' => $focusUser->id,
            'modified_by' => $focusUser->id,
        ),
    );


    require_once("include/SugarFolders/SugarFolders.php");

    $parent_id = '';


    foreach ($params as $type => $type_params) {
        if ($type == "inbound") {
            $folder = new SugarFolder();
            foreach ($params[$type] as $key => $val) {
                $folder->$key = $val;
            }

            $folder->new_with_id = false;
            $folder->id = $focus->id;
            $folder->save();

            $parent_id = $folder->id;
        } else {
            $params[$type]['parent_folder'] = $parent_id;

            $folder = new SugarFolder();
            foreach ($params[$type] as $key => $val) {
                $folder->$key = $val;
            }

            $folder->save();
        }
    }
} else {
    // Update folders
    require_once("include/SugarFolders/SugarFolders.php");
    $foldersFound = $focus->db->query('SELECT * FROM folders WHERE folders.id LIKE "'.$focus->id.'" OR '.
        'folders.parent_folder LIKE "'.$focus->id.'"');
    while ($row = $focus->db->fetchRow($foldersFound)) {
        $name = '';
        switch ($row['folder_type']) {
            case 'inbound':
                $name = $focus->mailbox . ' ('.$focus->name.')';
                break;
            case 'draft':
                $name = $mod_strings['LNK_MY_DRAFTS'] . ' ('.$stored_options['sentFolder'].')';
                break;
            case 'sent':
                $name = $mod_strings['LNK_SENT_EMAIL_LIST'] . ' ('.$stored_options['sentFolder'].')';
                break;
            case 'archived':
                $name = $mod_strings['LBL_LIST_TITLE_MY_ARCHIVES'];
                break;
        }

        $folder = new SugarFolder();
        $folder->retrieve($row['id']);
        $folder->name = $name;
        $folder->save();
    }
}





//Reset the value so no other saves are affected.
$GLOBALS['sugar_config']['disable_team_access_check'] = $previousTeamAccessCheck;



//Sync any changes within the IE account that need to be synced with the Sugar Folder.
//Need to do this post save so the correct team/teamset id is generated correctly.
$monitor_fields = array('name', 'status',
                        );

//Only sync IE accounts with a group folder.  Need to sync new records as team set assignment is processed
//after save.
if (!empty($focus->groupfolder_id)) {
    foreach ($monitor_fields as $singleField) {
        //Check if the value is being changed during save.
        if ($focus->fetched_row[$singleField] != $focus->$singleField) {
            syncSugarFoldersWithBeanChanges($singleField, $focus);
        }
    }
}

//check if we are in campaigns module
if ($_REQUEST['module'] == 'Campaigns') {
    //this is coming from campaign wizard, Just set the error message if it exists and skip the redirect
    if (!empty($error)) {
        $_REQUEST['error'] = true;
    }
} else {

    //this is a normal Inbound Email save, so set up the url and reirect
    $_REQUEST['return_id'] = $focus->id;

    $edit='';
    if (isset($_REQUEST['return_module']) && $_REQUEST['return_module'] != "") {
        $return_module = $_REQUEST['return_module'];
    } else {
        $return_module = "InboundEmail";
    }
    if (isset($_REQUEST['return_action']) && $_REQUEST['return_action'] != "") {
        $return_action = $_REQUEST['return_action'];
    } else {
        $return_action = "DetailView";
    }
    if (isset($_REQUEST['return_id']) && $_REQUEST['return_id'] != "") {
        $return_id = $_REQUEST['return_id'];
    }
    if (!empty($_REQUEST['edit'])) {
        $return_id='';
        $edit='&edit=true';
    }

    $log->debug("Saved record with id of ".$return_id);

    $redirectUrl = "Location: index.php?module=$return_module&action=$return_action&record=$return_id$edit";

    if (isset($error) && $error) {
        $redirectUrl .= $error;
    }

    header($redirectUrl);
}
require('modules/InboundEmail/PostSave.php');


/**
 * Certain updates to the IE account need to be reflected in the related SugarFolder since they are
 * created automatically.  Only valid for IE accounts with auto import turned on.
 *
 * @param string $fieldName The field name that changed
 * @param SugarBean $focus The InboundEmail bean being saved.
 */
function syncSugarFoldersWithBeanChanges($fieldName, $focus)
{
    $f = new SugarFolder();
    $f->retrieve($focus->groupfolder_id);

    switch ($fieldName) {
        case 'name':
        case 'team_id':
        case 'team_set_id':
            $f->$fieldName = $focus->$fieldName;
            $f->save();
            break;

        case 'status':
            if ($focus->status == 'Inactive') {
                $f->clearSubscriptionsForFolder($focus->groupfolder_id);
            } else {
                if ($focus->mailbox_type != 'bounce') {
                    $f->addSubscriptionsToGroupFolder();
                }
            }
            break;
    }
}
