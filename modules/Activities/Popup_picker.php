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

require_once 'include/upload_file.php';
require_once 'include/utils/db_utils.php';

global $currentModule;

global $focus;
global $action;

global $app_strings;
global $app_list_strings;
//we don't want the parent module's string file, but rather the string file specific to this subpanel
global $current_language, $beanList, $beanFiles;
$current_module_strings = return_module_language($current_language, 'Activities');

$focus = BeanFactory::getBean($_REQUEST['module_name']);

#[\AllowDynamicProperties]
class Popup_Picker
{

    /**
     * sole constructor
     */
    public function __construct()
    {
    }

    public function process_page()
    {
        global $sugar_config;
        global $focus;
        global $mod_strings;
        global $app_strings;
        global $app_list_strings;
        global $timedate;

        $sugar_config = $sugar_config ?? [];


        $summary_list = array();
        $task_list = array();
        $meeting_list = array();
        $calls_list = array();
        $emails_list = array();
        $notes_list = array();

        if (!empty($_REQUEST['record'])) {
            $result = $focus->retrieve($_REQUEST['record']);
            if ($result == null) {
                sugar_die($app_strings['ERROR_NO_RECORD']);
            }
        }

        $activitiesRels = array(
            'tasks' => 'Task',
            'meetings' => 'Meeting',
            'calls' => 'Call',
            'emails' => 'Email',
            'notes' => 'Note'
        );
        //Setup the arrays to store the linked records.
        foreach ($activitiesRels as $relMod => $beanName) {
            $varname = 'focus_' . $relMod . '_list';
            ${$varname} = array();
        }
        foreach ($focus->get_linked_fields() as $field => $def) {
            if ($focus->load_relationship($field)) {
                $relTable = BeanFactory::getBean($focus->$field->getRelatedModuleName())->table_name;
                if (array_key_exists($relTable, $activitiesRels)) {
                    $varname = 'focus_' . $relTable . '_list';
                    ${$varname} =
                        sugarArrayMerge(${$varname}, $focus->get_linked_beans($field, $activitiesRels[$relTable]));
                }
            }
        }

        foreach ($focus_tasks_list as $task) {
            if (!$task->ACLAccess('list')) {
                continue;
            }

            if (empty($task->date_due) || $task->date_due == '0000-00-00') {
                $date_due = '';
            } else {
                $date_due = $task->date_due;
            }

            if ($task->status !== "Not Started"
                && $task->status !== "In Progress"
                && $task->status !== "Pending Input") {
                $ts = '';
                if (!empty($task->fetched_row['date_due'])) {
                    //tasks can have an empty date due field
                    $ts = $timedate->fromDb($task->fetched_row['date_due'])->ts;
                }
                $summary_list[] = array('name' => $task->name,
                    'id' => $task->id,
                    'type' => "Task",
                    'direction' => '',
                    'module' => "Tasks",
                    'status' => $app_list_strings['task_status_dom'][$task->status],
                    'parent_id' => $task->parent_id,
                    'parent_type' => $task->parent_type,
                    'parent_name' => $task->parent_name,
                    'contact_id' => $task->contact_id,
                    'contact_name' => $task->contact_name,
                    'date_modified' => $date_due,
                    'description' => $this->getTaskDetails($task),
                    'date_type' => $app_strings['DATA_TYPE_DUE'] ?? '',
                    'sort_value' => $ts,
                    'image' => SugarThemeRegistry::current()->getImageURL('Tasks.svg')
                );
            } else {
                $open_activity_list[] = array('name' => $task->name,
                    'id' => $task->id,
                    'type' => "Task",
                    'direction' => '',
                    'module' => "Tasks",
                    'status' => $app_list_strings['task_status_dom'][$task->status],
                    'parent_id' => $task->parent_id,
                    'parent_type' => $task->parent_type,
                    'parent_name' => $task->parent_name,
                    'contact_id' => $task->contact_id,
                    'contact_name' => $task->contact_name,
                    'date_due' => $date_due,
                    'description' => $this->getTaskDetails($task),
                    'date_type' => $app_strings['DATA_TYPE_DUE'] ?? ''
                );
            }
        } // end Tasks

        foreach ($focus_meetings_list as $meeting) {
            if (!$meeting->ACLAccess('list')) {
                continue;
            }

            if (empty($meeting->contact_id) && empty($meeting->contact_name)) {
                $meeting_contacts = $meeting->get_linked_beans('contacts', 'Contact');
                if (!empty($meeting_contacts[0]->id) && !empty($meeting_contacts[0]->name)) {
                    $meeting->contact_id = $meeting_contacts[0]->id;
                    $meeting->contact_name = $meeting_contacts[0]->name;
                }
            }
            if ($meeting->status !== 'Planned') {
                $summary_list[] = array(
                    'name' => $meeting->name,
                    'id' => $meeting->id,
                    'type' => $mod_strings['LBL_MEETING_TYPE'],
                    'direction' => '',
                    'module' => 'Meetings',
                    'module' => 'Meetings',
                    'status' => $app_list_strings['meeting_status_dom'][$meeting->status],
                    'parent_id' => $meeting->parent_id,
                    'parent_type' => $app_list_strings['parent_type_display'][$meeting->parent_type],
                    'parent_name' => $meeting->parent_name,
                    'contact_id' => $meeting->contact_id,
                    'contact_name' => $meeting->contact_name,
                    'date_modified' => $meeting->date_start,
                    'description' => $this->formatDescription($meeting->description),
                    'date_type' => $mod_strings['LBL_DATA_TYPE_START'],
                    'sort_value' => $timedate->fromDb($meeting->fetched_row['date_start'])->ts,
                    'image' => SugarThemeRegistry::current()->getImageURL('Meetings.svg')
                );
            } else {
                $open_activity_list[] = array(
                    'name' => $meeting->name,
                    'id' => $meeting->id,
                    'type' => $mod_strings['LBL_MEETING_TYPE'],
                    'direction' => '',
                    'module' => 'Meetings',
                    'status' => $app_list_strings['meeting_status_dom'][$meeting->status],
                    'parent_id' => $meeting->parent_id,
                    'parent_type' => $app_list_strings['parent_type_display'][$meeting->parent_type],
                    'parent_name' => $meeting->parent_name,
                    'contact_id' => $meeting->contact_id,
                    'contact_name' => $meeting->contact_name,
                    'date_due' => $meeting->date_start,
                    'description' => $this->formatDescription($meeting->description),
                    'date_type' => $mod_strings['LBL_DATA_TYPE_START']
                );
            }
        } // end Meetings

        foreach ($focus_calls_list as $call) {
            if (!$call->ACLAccess('list')) {
                continue;
            }

            if (empty($call->contact_id) && empty($call->contact_name)) {
                $call_contacts = $call->get_linked_beans('contacts', 'Contact');
                if (!empty($call_contacts[0]->id) && !empty($call_contacts[0]->name)) {
                    $call->contact_id = $call_contacts[0]->id;
                    $call->contact_name = $call_contacts[0]->name;
                }
            }

            if ($call->status !== 'Planned') {
                $summary_list[] = array(
                    'name' => $call->name,
                    'id' => $call->id,
                    'type' => $mod_strings['LBL_CALL_TYPE'],
                    'direction' => $call->direction,
                    'module' => 'Calls',
                    'status' => $app_list_strings['call_status_dom'][$call->status],
                    'parent_id' => $call->parent_id,
                    'parent_type' => $app_list_strings['parent_type_display'][$call->parent_type],
                    'parent_name' => $call->parent_name,
                    'contact_id' => $call->contact_id,
                    'contact_name' => $call->contact_name,
                    'date_modified' => $call->date_start,
                    'description' => $this->formatDescription($call->description),
                    'date_type' => $mod_strings['LBL_DATA_TYPE_START'],
                    'sort_value' => $timedate->fromDb($call->fetched_row['date_start'])->ts,
                    'image' => SugarThemeRegistry::current()->getImageURL('Calls.svg')
                );
            } else {
                $open_activity_list[] = array(
                    'name' => $call->name,
                    'id' => $call->id,
                    'direction' => $call->direction,
                    'type' => $mod_strings['LBL_CALL_TYPE'],
                    'module' => 'Calls',
                    'status' => $app_list_strings['call_status_dom'][$call->status],
                    'parent_id' => $call->parent_id,
                    'parent_type' => $app_list_strings['parent_type_display'][$call->parent_type],
                    'parent_name' => $call->parent_name,
                    'contact_id' => $call->contact_id,
                    'contact_name' => $call->contact_name,
                    'date_due' => $call->date_start,
                    'description' => $this->formatDescription($call->description),
                    'date_type' => $mod_strings['LBL_DATA_TYPE_START']
                );
            }
        } // end Calls

        foreach ($focus_emails_list as $email) {
            if (!$email->ACLAccess('list')) {
                continue;
            }
            if (empty($email->contact_id) && empty($email->contact_name)) {
                $email_contacts = $email->get_linked_beans('contacts', 'Contact');
                if (!empty($email_contacts[0]->id) && !empty($email_contacts[0]->name)) {
                    $email->contact_id = $email_contacts[0]->id;
                    $email->contact_name = $email_contacts[0]->name;
                }
            }
            $ts = '';
            if (!empty($email->fetched_row['date_sent_received'])) {
                //emails can have an empty date sent field
                $ts = $timedate->fromDb($email->fetched_row['date_sent_received'])->ts;
            } elseif (!empty($email->fetched_row['date_entered'])) {
                $ts = $timedate->fromDb($email->fetched_row['date_entered'])->ts;
            }

            $summary_list[] = array(
                'name' => $email->name,
                'id' => $email->id,
                'type' => $mod_strings['LBL_EMAIL_TYPE'],
                'direction' => '',
                'module' => 'Emails',
                'status' => '',
                'parent_id' => $email->parent_id,
                'parent_type' => $app_list_strings['parent_type_display'][$email->parent_type],
                'parent_name' => $email->parent_name,
                'contact_id' => $email->contact_id,
                'contact_name' => $email->contact_name,
                'date_modified' => $email->date_sent_received,
                'description' => $this->getEmailDetails($email),
                'date_type' => $mod_strings['LBL_DATA_TYPE_SENT'],
                'sort_value' => $ts,
                'image' => SugarThemeRegistry::current()->getImageURL('Emails.svg')
            );
        } //end Emails

        // Bug 46439 'No email archived when clicking on View Summary' (All condition)
        if (method_exists($focus, 'get_unlinked_email_query')) {
            $queryArray = $focus->get_unlinked_email_query(array('return_as_array' => 'true'));
            $query = $queryArray['select'];
            $query .= $queryArray['from'];
            if (!empty($queryArray['join_tables'])) {
                foreach ($queryArray['join_tables'] as $join_table) {
                    if ($join_table != '') {
                        $query .= ', ' . $join_table . ' ';
                    }
                }
            }
            $query .= $queryArray['join'];
            $query .= $queryArray['where'];
            $emails = BeanFactory::newBean('Emails');
            $focus_unlinked_emails_list = $emails->process_list_query($query, 0);
            $focus_unlinked_emails_list = $focus_unlinked_emails_list['list'];
            foreach ($focus_unlinked_emails_list as $email) {
                $email->retrieve($email->id);

                $summary_list[] = array(
                    'name' => $email->name,
                    'id' => $email->id,
                    'type' => $mod_strings['LBL_EMAIL_TYPE'],
                    'direction' => '',
                    'module' => 'Emails',
                    'status' => '',
                    'parent_id' => $email->parent_id,
                    'parent_type' => $app_list_strings['parent_type_display'][$email->parent_type],
                    'parent_name' => $email->parent_name,
                    'contact_id' => $email->contact_id,
                    'contact_name' => $email->contact_name,
                    'date_modified' => $email->date_sent_received . ' ' . $email->time_start,
                    'description' => $this->getEmailDetails($email),
                    'date_type' => $mod_strings['LBL_DATA_TYPE_SENT'],
                    'sort_value' => strtotime($email->fetched_row['date_sent_received'] . ' GMT'),
                    'image' => SugarThemeRegistry::current()->getImageURL('Emails.svg')
                );
            }
        } //end Unlinked Emails

        foreach ($focus_notes_list as $note) {
            if (!$note->ACLAccess('list')) {
                continue;
            }
            if ($note->ACLAccess('view')) {
                $summary_list[] = array(
                    'name' => $note->name,
                    'id' => $note->id,
                    'type' => $mod_strings['LBL_NOTE_TYPE'],
                    'direction' => '',
                    'module' => 'Notes',
                    'status' => '',
                    'parent_id' => $note->parent_id,
                    'parent_type' => $app_list_strings['parent_type_display'][$note->parent_type] ?? '',
                    'parent_name' => $note->parent_name,
                    'contact_id' => $note->contact_id,
                    'contact_name' => $note->contact_name,
                    'date_modified' => $note->date_modified,
                    'description' => $this->formatDescription($note->description),
                    'date_type' => $mod_strings['LBL_DATA_TYPE_MODIFIED'],
                    'sort_value' => strtotime($note->fetched_row['date_modified'] . ' GMT'),
                    'image' => SugarThemeRegistry::current()->getImageURL('Notes.svg')
                );
                if (!empty($note->filename)) {
                    $count = count($summary_list);
                    $count--;
                    $summary_list[$count]['filename'] = $note->filename;
                    $summary_list[$count]['fileurl'] = UploadFile::get_url($note->filename, $note->id);
                }
            }
        } // end Notes


        if (count($summary_list) > 0) {
            array_multisort(array_column($summary_list, 'sort_value'), SORT_DESC, $summary_list);

            foreach ($summary_list as $list) {
                if ($list['module'] === 'Tasks') {
                    $task_list[] = $list;
                } elseif ($list['module'] === 'Meetings') {
                    $meeting_list[] = $list;
                } elseif ($list['module'] === 'Calls') {
                    $calls_list[] = $list;
                } elseif ($list['module'] === 'Emails') {
                    $emails_list[] = $list;
                } elseif ($list['module'] === 'Notes') {
                    $notes_list[] = $list;
                }
            }
        }

        $template = new Sugar_Smarty();
        $template->assign('app', $app_strings);
        $template->assign('mod', $mod_strings);
        $theme = SugarThemeRegistry::current();
        $css = $theme->getCSS();
        $template->assign('css', $css);
        $template->assign('theme', SugarThemeRegistry::current());
        $template->assign('langHeader', get_language_header());
        $template->assign('summaryList', $summary_list);
        $template->assign('taskslist', $task_list);
        $template->assign('meetingList', $meeting_list);
        $template->assign('callsList', $calls_list);
        $template->assign('emailsList', $emails_list);
        $template->assign('notesList', $notes_list);
        $ieCompatMode = false;
        if (isset($sugar_config['meta_tags']) && isset($sugar_config['meta_tags']['ieCompatMode'])) {
            $ieCompatMode = $sugar_config['meta_tags']['ieCompatMode'];
        }

        $template->assign('ieCompatMode', $ieCompatMode);
        $charset = isset($app_strings['LBL_CHARSET']) ? $app_strings['LBL_CHARSET'] : $sugar_config['default_charset'];
        $template->assign('charset', $charset);

        $title = getClassicModuleTitle(
            $focus->module_dir,
            array(translate('LBL_MODULE_NAME', $focus->module_dir), $focus->name),
            false
        );

        $template->assign('title', $title);


        return $template->fetch('modules/Activities/tpls/PopupBody.tpl');
    }

    /**
     * @param $email
     *
     * @return string
     */
    public function getEmailDetails($email)
    {
        $details = "";

        if (!empty($email->to_addrs)) {
            $details .= 'To: ' . $email->to_addrs . '<br>';
        }
        if (!empty($email->from_addr)) {
            $details .= 'From: ' . $email->from_addr . '<br>';
        }
        if (!empty($email->cc_addrs)) {
            $details .= 'CC: ' . $email->cc_addrs . '<br>';
        }
        if (!empty($email->from_addr) || !empty($email->cc_addrs) || !empty($email->to_addrs)) {
            $details .= '<br>';
        }

        // cn: bug 8433 - history does not distinguish b/t text/html emails
        $details .= empty($email->description_html) ? $this->formatDescription($email->description) :
            $this->formatDescription(strip_tags(br2nl(from_html($email->description_html))));

        return $details;
    }

    /**
     * @param $task
     *
     * @return string
     */
    public function getTaskDetails($task)
    {
        global $app_strings;

        $details = "";
        if (!empty($task->date_start) && $task->date_start != '0000-00-00') {
            $details .= $app_strings['DATA_TYPE_START'] ?? '' . $task->date_start . '<br>';
            $details .= '<br>';
        }
        $details .= $this->formatDescription($task->description);

        return $details;
    }

    /**
     * @param $description
     *
     * @return string
     */
    public function formatDescription($description)
    {
        return nl2br($description);
    }
} // end of class Popup_Picker
