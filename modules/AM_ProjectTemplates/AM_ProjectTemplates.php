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
 *//**
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
 * THIS CLASS IS FOR DEVELOPERS TO MAKE CUSTOMIZATIONS IN
 */
require_once('modules/AM_ProjectTemplates/AM_ProjectTemplates_sugar.php');
#[\AllowDynamicProperties]
class AM_ProjectTemplates extends AM_ProjectTemplates_sugar
{
    public function __construct()
    {
        parent::__construct();
    }


    public function save($check_notify = false)
    {
        $focus = $this;

        if ((isset($_POST['isSaveFromDetailView']) && $_POST['isSaveFromDetailView'] == 'true') ||
            (isset($_POST['is_ajax_call']) && !empty($_POST['is_ajax_call']) && !empty($focus->id) ||
            (isset($_POST['return_action']) && $_POST['return_action'] == 'SubPanelViewer') && !empty($focus->id))||
             !isset($_POST['user_invitees']) // we need to check that user_invitees exists before processing, it is ok to be empty
        ) {
            parent::save(true) ;
            $return_id = $focus->id;
        } else {
            if (!empty($_POST['user_invitees'])) {
                $userInvitees = explode(',', trim($_POST['user_invitees'], ','));
            } else {
                $userInvitees = array();
            }


            if (!empty($_POST['contact_invitees'])) {
                $contactInvitees = explode(',', trim($_POST['contact_invitees'], ','));
            } else {
                $contactInvitees = array();
            }


            $deleteUsers = array();
            $existingUsers = array();

            $deleteContacts = array();
            $existingContacts = array();

            if (!empty($this->id)) {


                ////	REMOVE RESOURCE RELATIONSHIPS
                // Calculate which users to flag as deleted and which to add

                // Get all users for the project template
                $focus->load_relationship('users');
                $users = $focus->get_linked_beans('am_projecttemplates_users_1', 'User');
                foreach ($users as $a) {
                    if (!in_array($a->id, $userInvitees)) {
                        $deleteUsers[$a->id] = $a->id;
                    } else {
                        $existingUsers[$a->id] = $a->id;
                    }
                }

                if (count($deleteUsers) > 0) {
                    $sql = '';
                    foreach ($deleteUsers as $u) {
                        $sql .= ",'" . $u . "'";
                    }
                    $sql = substr($sql, 1);
                    // We could run a delete SQL statement here, but will just mark as deleted instead
                    $sql = "UPDATE am_projecttemplates_users_1_c set deleted = 1 where users_idb in ($sql) AND am_projecttemplates_ida = '". $focus->id . "'";
                    $focus->db->query($sql);
                    echo $sql;
                }

                // Get all contacts for the project
                $focus->load_relationship('contacts');
                $contacts = $focus->get_linked_beans('am_projecttemplates_contacts_1', 'Contact');
                foreach ($contacts as $a) {
                    if (!in_array($a->id, $contactInvitees)) {
                        $deleteContacts[$a->id] = $a->id;
                    } else {
                        $existingContacts[$a->id] = $a->id;
                    }
                }

                if (count($deleteContacts) > 0) {
                    $sql = '';
                    foreach ($deleteContacts as $u) {
                        $sql .= ",'" . $u . "'";
                    }
                    $sql = substr($sql, 1);
                    // We could run a delete SQL statement here, but will just mark as deleted instead
                    $sql = "UPDATE am_projecttemplates_contacts_1_c set deleted = 1 where contacts_idb in ($sql) AND am_projecttemplates_ida = '". $focus->id . "'";
                    $focus->db->query($sql);
                    echo $sql;
                }

                ////	END REMOVE
            }

            $return_id = parent::save($check_notify);
            $focus->retrieve($return_id);

            ////	REBUILD INVITEE RELATIONSHIPS

            // Process users
            $focus->load_relationship('users');
            $focus->get_linked_beans('am_projecttemplates_users_1', 'User');
            foreach ($userInvitees as $user_id) {
                if (empty($user_id) || isset($existingUsers[$user_id]) || isset($deleteUsers[$user_id])) {
                    continue;
                }
                $focus->am_projecttemplates_users_1->add($user_id);
            }

            // Process contacts
            $focus->load_relationship('contacts');
            $focus->get_linked_beans('am_projecttemplates_contacts_1', 'Contact');
            foreach ($contactInvitees as $contact_id) {
                if (empty($contact_id) || isset($existingContacts[$contact_id]) || isset($deleteContacts[$contact_id])) {
                    continue;
                }
                $focus->am_projecttemplates_contacts_1->add($contact_id);
            }

            ////	END REBUILD INVITEE RELATIONSHIPS
            ///////////////////////////////////////////////////////////////////////////
        }

        return $return_id;
    }
}
