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

if (empty($_REQUEST['record'])) {
    sugar_die('No record provided.');
}

$focus = BeanFactory::newBean('MergeRecords');
$focus->load_merge_bean($_REQUEST['merge_module'], true, $_REQUEST['record']);
if (empty($focus->merge_bean->id)) {
    sugar_die('No record provided.');
}
if (empty($dictionary[$focus->merge_bean->object_name]['duplicate_merge'])) {
    sugar_die('You do not have access to merge this module.');
}
if (!$focus->merge_bean->ACLAccess('edit')) {
    sugar_die('You do not have access to merge this module.');
}

if (empty($_REQUEST['merged_ids'])) {
    sugar_die('No merge records provided.');
}
require_once $focus->merge_bean_file_path;

foreach ($_REQUEST['merged_ids'] as $mergeId) {
    $mergeSource = new $focus->merge_bean_class();
    if (!$mergeSource->retrieve($mergeId)) {
        continue;
    }
    if (!$mergeSource->ACLAccess('edit')) {
        sugar_die('You do not have access to merge this module.');
    }
    if (!$mergeSource->ACLAccess('delete')) {
        sugar_die('You do not have access to merge this module.');
    }
}

foreach ($focus->merge_bean->column_fields as $field) {
    if (isset($_POST[$field])) {
        $value = $_POST[$field];
        if (is_array($value) && !empty($focus->merge_bean->field_defs[$field]['isMultiSelect'])) {
            if (empty($value[0])) {
                unset($value[0]);
            }
            $value = encodeMultienumValue($value);
        }
        // STIC Custom 20250610 JBL - Copy Image merging records
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/672
        if (isset($focus->merge_bean->field_name_map[$field]['type']) && $focus->merge_bean->field_name_map[$field]['type'] == 'image') {
            if (is_array($_POST['merged_ids'])) {
                foreach ($_POST['merged_ids'] as $id) {
                    require_once $focus->merge_bean_file_path;
                    $mergeSource = new $focus->merge_bean_class();
                    if ($mergeSource->retrieve($id) && $mergeSource->$field == $value) {
                        // Copy image
                        $source_file = "upload/{$mergeSource->id}_{$field}";
                        $target_file = "upload/{$focus->merge_bean->id}_{$field}";
                        if (file_exists($source_file)) {
                            copy($source_file, $target_file);
                        }
                        break;
                    }
                }
            }
        }
        // END STIC Custom
        $focus->merge_bean->$field = $value;
    } elseif (isset($focus->merge_bean->field_name_map[$field]['type']) && $focus->merge_bean->field_name_map[$field]['type'] == 'bool') {
        $focus->merge_bean->$field = 0;
    }
}

foreach ($focus->merge_bean->additional_column_fields as $field) {
    if (isset($_POST[$field])) {
        $value = $_POST[$field];
        if (is_array($value) && !empty($focus->merge_bean->field_defs[$field]->properties['isMultiSelect'])) {
            if (empty($value[0])) {
                unset($value[0]);
            }
            $value = encodeMultienumValue($value);
        }
        $focus->merge_bean->$field = $value;
    }
}

global $check_notify;

$_REQUEST['useEmailWidget'] = true;
if (isset($_POST['date_entered'])) {
    // set this to true so we won't unset date_entered when saving
    $focus->merge_bean->update_date_entered = true;
}
$focus->merge_bean->save($check_notify);
unset($_REQUEST['useEmailWidget']);

$return_id = $focus->merge_bean->id;
$return_module = $focus->merge_module;
$return_action = 'DetailView';

//handle related data.

$linked_fields = $focus->merge_bean->get_linked_fields();

$exclude = explode(',', $_REQUEST['merged_links']);

if (is_array($_POST['merged_ids'])) {
    foreach ($_POST['merged_ids'] as $id) {
        require_once $focus->merge_bean_file_path;
        $mergeSource = new $focus->merge_bean_class();
        if (!$mergeSource->retrieve($id)) {
            continue;
        }
        foreach ($linked_fields as $name => $properties) {
            if ($properties['name'] == 'modified_user_link' || $properties['name'] == 'created_by_link' || in_array($properties['name'], $exclude)) {
                continue;
            }
            if (isset($properties['duplicate_merge']) &&
                ($properties['duplicate_merge'] == 'disabled' ||
                    $properties['duplicate_merge'] == 'false' ||
                    $properties['name'] == 'assigned_user_link')
            ) {
                continue;
            }
            if ($name == 'accounts' && $focus->merge_bean->module_dir == 'Opportunities') {
                continue;
            }

            if ($mergeSource->load_relationship($name)) {
                //check to see if loaded relationship is with email address
                $relName = $mergeSource->$name->getRelatedModuleName();
                if (!empty($relName) && strtolower($relName) == 'emailaddresses') {
                    //handle email address merge
                    handleEmailMerge($focus, $name, $mergeSource->$name->get());
                } else {
                    $data = $mergeSource->$name->get();
                    if (is_array($data) && $focus->merge_bean->load_relationship($name)) {
                        foreach ($data as $related_id) {
                            //add to primary bean
                            $focus->merge_bean->$name->add($related_id);
                        }
                    }
                }
            }
        }
        //END Bug #13826
        //delete the child bean, this action will cascade into related data too.
        $mergeSource->mark_deleted($mergeSource->id);
    }
}
$GLOBALS['log']->debug('Merged record with id of '.$return_id);

//do not redirect if noRedirect flag is set.  This is mostly used by Unit tests
if (empty($_REQUEST['noRedirect'])) {
    header("Location: index.php?action=$return_action&module=$return_module&record=$return_id");
}

/**
 * This function will compare the email addresses to be merged and only add the email id's
 * of the email addresses that are not duplicates.
 *
 * @param MergeRecord $focus - Merge Bean
 * @param string      $name  name of relationship (email_addresses)
 * @param array       $data  array of email id's that will be merged into existing bean.
 */
function handleEmailMerge($focus, $name, $data)
{
    $mrgArray = array();
    //get the email id's to merge
    $existingData = $data;

    //make sure id's to merge exist and are in array format

    //get the existing email id's
    $focus->merge_bean->load_relationship($name);
    $exData = $focus->merge_bean->$name->get();

    if (!is_array($existingData) || empty($existingData)) {
        return;
    }
    //query email and retrieve existing email address
    $exEmailQuery = 'Select id, email_address from email_addresses where id in (';
    $first = true;
    foreach ($exData as $id) {
        if ($first) {
            $exEmailQuery .= " '$id' ";
            $first = false;
        } else {
            $exEmailQuery .= ", '$id' ";
            $first = false;
        }
    }
    $exEmailQuery .= ')';

    $exResult = $focus->merge_bean->db->query($exEmailQuery);
    $existingEmails = array();
    while (($row = $focus->merge_bean->db->fetchByAssoc($exResult)) != null) {
        $existingEmails[$row['id']] = $row['email_address'];
    }

    //query email and retrieve email address to be linked.
    $newEmailQuery = 'Select id, email_address from email_addresses where id in (';
    $first = true;
    foreach ($existingData as $id) {
        if ($first) {
            $newEmailQuery .= " '$id' ";
            $first = false;
        } else {
            $newEmailQuery .= ", '$id' ";
            $first = false;
        }
    }
    $newEmailQuery .= ')';

    $newResult = $focus->merge_bean->db->query($newEmailQuery);
    $newEmails = array();
    while (($row = $focus->merge_bean->db->fetchByAssoc($newResult)) != null) {
        $newEmails[$row['id']] = $row['email_address'];
    }

    //compare the two arrays and remove duplicates
    foreach ($newEmails as $k => $n) {
        if (!in_array($n, $existingEmails)) {
            $mrgArray[$k] = $n;
        }
    }

    //add email id's.
    foreach ($mrgArray as $related_id => $related_val) {
        //add to primary bean
        $focus->merge_bean->$name->add($related_id);
    }
}
