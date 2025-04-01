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




// load the correct demo data and main application language file depending upon the installer language selected; if
// it's not found fall back on en_us
if (file_exists("include/language/{$current_language}.lang.php")) {
    require_once("include/language/{$current_language}.lang.php");
} else {
    require_once("include/language/en_us.lang.php");
}
require_once('install/UserDemoData.php');
require_once('install/TeamDemoData.php');

global $sugar_demodata;
if (file_exists("install/demoData.{$current_language}.php")) {
    require_once("install/demoData.{$current_language}.php");
} else {
    require_once("install/demoData.en_us.php");
}

$last_name_count = is_countable($sugar_demodata['last_name_array']) ? count($sugar_demodata['last_name_array']) : 0;
$first_name_count = is_countable($sugar_demodata['first_name_array']) ? count($sugar_demodata['first_name_array']) : 0;
$company_name_count = is_countable($sugar_demodata['company_name_array']) ? count($sugar_demodata['company_name_array']) : 0;
$street_address_count = is_countable($sugar_demodata['street_address_array']) ? count($sugar_demodata['street_address_array']) : 0;
$city_array_count = is_countable($sugar_demodata['city_array']) ? count($sugar_demodata['city_array']) : 0;
global $app_list_strings;
global $sugar_config;
$_REQUEST['useEmailWidget'] = "true";
if (empty($app_list_strings)) {
    $app_list_strings = return_app_list_strings_language('en_us');
}
/*
 * Seed the random number generator with a fixed constant.  This will make all installs of the same code have the same
 * seed data.  This facilitates cross database testing..
 */
mt_srand(93285903);
$db = DBManagerFactory::getInstance();
$timedate = TimeDate::getInstance();
// Set the max time to one hour (helps Windows load the seed data)
ini_set("max_execution_time", "3600");
// ensure we have enough memory
$memory_needed  = 256;
$memory_limit   = ini_get('memory_limit');
if ($memory_limit != "" && $memory_limit != "-1") { // if memory_limit is set
    rtrim($memory_limit, 'M');
    $memory_limit_int = (int) $memory_limit;
    if ($memory_limit_int < $memory_needed) {
        ini_set("memory_limit", $memory_needed . "M");
    }
}
$large_scale_test = empty($sugar_config['large_scale_test']) ?
    false : $sugar_config['large_scale_test'];

$seed_user = BeanFactory::newBean('Users');
$user_demo_data = new UserDemoData($seed_user, $large_scale_test);
$user_demo_data->create_demo_data();
$number_contacts = 200;
$number_companies = 50;
$number_leads = 200;
$large_scale_test = empty($sugar_config['large_scale_test']) ? false : $sugar_config['large_scale_test'];
// If large scale test is set to true, increase the seed data.
if ($large_scale_test) {
    // increase the cuttoff time to 1 hour
    ini_set("max_execution_time", "3600");
    $number_contacts = 100000;
    $number_companies = 15000;
    $number_leads = 100000;
}


$possible_duration_hours_arr = array( 0, 1, 2, 3);
$possible_duration_minutes_arr = array('00' => '00','15' => '15', '30' => '30', '45' => '45');
$account_ids = array();
$accounts = array();
$opportunity_ids = array();

// Determine the assigned user for all demo data.  This is the default user if set, or admin
$assigned_user_name = "admin";
if (!empty($sugar_config['default_user_name']) &&
    !empty($sugar_config['create_default_user']) &&
    $sugar_config['create_default_user']) {
    $assigned_user_name = $sugar_config['default_user_name'];
}

// Look up the user id for the assigned user
$seed_user = BeanFactory::newBean('Users');
$assigned_user_id = $seed_user->retrieve_user_id($assigned_user_name);
$patterns[] = '/ /';
$patterns[] = '/\./';
$patterns[] = '/&/';
$patterns[] = '/\//';
$replacements[] = '';
$replacements[] = '';
$replacements[] = '';
$replacements[] = '';

///////////////////////////////////////////////////////////////////////////////
////	ACCOUNTS

for ($i = 0; $i < $number_companies; $i++) {
    $account_name = $sugar_demodata['company_name_array'][mt_rand(0, $company_name_count-1)];
    // Create new accounts.
    $account = BeanFactory::newBean('Accounts');
    $account->name = $account_name;
    $account->phone_office = create_phone_number();
    $account->assigned_user_id = $assigned_user_id;
    $account->emailAddress->addAddress(createEmailAddress(), true);
    $account->emailAddress->addAddress(createEmailAddress());
    $account->website = createWebAddress();
    $account->billing_address_street = $sugar_demodata['street_address_array'][mt_rand(0, $street_address_count-1)];
    $account->billing_address_city = $sugar_demodata['city_array'][mt_rand(0, $city_array_count-1)];
    if ($i % 3 == 1) {
        $account->billing_address_state = "NY";
        $assigned_user_id = mt_rand(9, 10);
        if ($assigned_user_id == 9) {
            $account->assigned_user_name = "seed_will";
            $account->assigned_user_id = $account->assigned_user_name."_id";
        } else {
            $account->assigned_user_name = "seed_chris";
            $account->assigned_user_id = $account->assigned_user_name."_id";
        }

        $account->assigned_user_id = $account->assigned_user_name."_id";
    } else {
        $account->billing_address_state = "CA";
        $assigned_user_id = mt_rand(6, 8);
        if ($assigned_user_id == 6) {
            $account->assigned_user_name = "seed_sarah";
        } elseif ($assigned_user_id == 7) {
            $account->assigned_user_name = "seed_sally";
        } else {
            $account->assigned_user_name = "seed_max";
        }

        $account->assigned_user_id = $account->assigned_user_name."_id";
    }

    $account->billing_address_postalcode = mt_rand(10000, 99999);
    $account->billing_address_country = 'USA';
    $account->shipping_address_street = $account->billing_address_street;
    $account->shipping_address_city = $account->billing_address_city;
    $account->shipping_address_state = $account->billing_address_state;
    $account->shipping_address_postalcode = $account->billing_address_postalcode;
    $account->shipping_address_country = $account->billing_address_country;
    $account->industry = array_rand($app_list_strings['industry_dom']);
    $account->account_type = "Customer";
    $account->save();
    $account_ids[] = $account->id;
    $accounts[] = $account;

    // Create a case for the account
    $case = BeanFactory::newBean('Cases');
    $case->account_id = $account->id;
    $case->priority = array_rand($app_list_strings['case_priority_dom']);
    $case->status = array_rand($app_list_strings['case_status_dom']);
    $case->name = $sugar_demodata['case_seed_names'][mt_rand(0, 4)];
    $case->assigned_user_id = $account->assigned_user_id;
    $case->assigned_user_name = $account->assigned_user_name;
    $case->save();

    // Create a bug for the account
    $bug = BeanFactory::newBean('Bugs');
    $bug->account_id = $account->id;
    $bug->priority = array_rand($app_list_strings['bug_priority_dom']);
    $bug->status = array_rand($app_list_strings['bug_status_dom']);
    $bug->name = $sugar_demodata['bug_seed_names'][mt_rand(0, 4)];
    $bug->assigned_user_id = $account->assigned_user_id;
    $bug->assigned_user_name = $account->assigned_user_name;
    $bug->save();

    $note = BeanFactory::newBean('Notes');
    $note->parent_type = 'Accounts';
    $note->parent_id = $account->id;
    $seed_data_index = mt_rand(0, 3);
    $note->name = $sugar_demodata['note_seed_names_and_Descriptions'][$seed_data_index][0];
    $note->description = $sugar_demodata['note_seed_names_and_Descriptions'][$seed_data_index][1];
    $note->assigned_user_id = $account->assigned_user_id;
    $note->assigned_user_name = $account->assigned_user_name;
    $note->save();

    $call = BeanFactory::newBean('Calls');
    $call->parent_type = 'Accounts';
    $call->parent_id = $account->id;
    $call->name = $sugar_demodata['call_seed_data_names'][mt_rand(0, 3)];
    $call->assigned_user_id = $account->assigned_user_id;
    $call->assigned_user_name = $account->assigned_user_name;
    $call->direction='Outbound';
    $call->date_start = create_date(). ' ' . create_time();
    $call->duration_hours='0';
    $call->duration_minutes='30';
    $call->account_id =$account->id;
    $call->status='Planned';
    $call->save();

    //Set the user to accept the call
    $seed_user->id = $call->assigned_user_id;
    $call->set_accept_status($seed_user, 'accept');

    //Create new opportunities
    $opp = BeanFactory::newBean('Opportunities');
    $opp->assigned_user_id = $account->assigned_user_id;
    $opp->assigned_user_name = $account->assigned_user_name;
    $opp->name = substr($account_name." - 1000 units", 0, 50);
    $opp->date_closed = create_date();
    $opp->lead_source = array_rand($app_list_strings['lead_source_dom']);
    $opp->sales_stage = array_rand($app_list_strings['sales_stage_dom']);
    // If the deal is already one, make the date closed occur in the past.
    if ($opp->sales_stage == "Closed Won" || $opp->sales_stage == "Closed Lost") {
        $opp->date_closed = create_past_date();
    }
    $opp->opportunity_type = array_rand($app_list_strings['opportunity_type_dom']);
    $amount = array("10000", "25000", "50000", "75000");
    $key = array_rand($amount);
    $opp->amount = $amount[$key];
    $probability = array("10", "70", "40", "60");
    $key = array_rand($probability);
    $opp->probability = $probability[$key];
    $opp->save();
    $opportunity_ids[] = $opp->id;
    // Create a linking table entry to assign an account to the opportunity.
    $opp->set_relationship('accounts_opportunities', array('opportunity_id'=>$opp->id ,'account_id'=> $account->id), false);
}

$titles = $sugar_demodata['titles'];
$account_max = count($account_ids) - 1;
$first_name_max = $first_name_count - 1;
$last_name_max = $last_name_count - 1;
$street_address_max = $street_address_count - 1;
$city_array_max = $city_array_count - 1;
$lead_source_max = (is_countable($app_list_strings['lead_source_dom']) ? count($app_list_strings['lead_source_dom']) : 0) - 1;
$lead_status_max = (is_countable($app_list_strings['lead_status_dom']) ? count($app_list_strings['lead_status_dom']) : 0) - 1;
$title_max = (is_countable($titles) ? count($titles) : 0) - 1;
///////////////////////////////////////////////////////////////////////////////
////	DEMO CONTACTS
for ($i=0; $i<$number_contacts; $i++) {
    $contact = BeanFactory::newBean('Contacts');
    $contact->first_name = $sugar_demodata['first_name_array'][mt_rand(0, $first_name_max)];
    $contact->last_name = $sugar_demodata['last_name_array'][mt_rand(0, $last_name_max)];
    $contact->assigned_user_id = $account->assigned_user_id;
    $contact->primary_address_street = $sugar_demodata['street_address_array'][mt_rand(0, $street_address_max)];
    $contact->primary_address_city = $sugar_demodata['city_array'][mt_rand(0, $city_array_max)];
    $contact->lead_source = array_rand($app_list_strings['lead_source_dom']);
    $contact->title = $titles[mt_rand(0, $title_max)];
    $contact->emailAddress->addAddress(createEmailAddress(), true, true);
    $contact->emailAddress->addAddress(createEmailAddress(), false, false, false, true);
    $assignedUser = BeanFactory::newBean('Users');
    $assignedUser->retrieve($contact->assigned_user_id);
    $contact->assigned_user_id = $assigned_user_id;
    $contact->email1 = createEmailAddress();
    $key = array_rand($sugar_demodata['street_address_array']);
    $contact->primary_address_street = $sugar_demodata['street_address_array'][$key];
    $key = array_rand($sugar_demodata['city_array']);
    $contact->primary_address_city = $sugar_demodata['city_array'][$key];
    $contact->lead_source = array_rand($app_list_strings['lead_source_dom']);
    $contact->title = $titles[array_rand($titles)];
    $contact->phone_work = create_phone_number();
    $contact->phone_home = create_phone_number();
    $contact->phone_mobile = create_phone_number();
    $account_number = mt_rand(0, $account_max);
    $account_id = $account_ids[$account_number];
    // Fill in a bogus address
    $contacts_account = $accounts[$account_number];
    $contact->primary_address_state = $contacts_account->billing_address_state;
    $contact->assigned_user_id = $contacts_account->assigned_user_id;
    $contact->assigned_user_name = $contacts_account->assigned_user_name;
    $contact->primary_address_postalcode = mt_rand(10000, 99999);
    $contact->primary_address_country = 'USA';
    $contact->save();
    // Create a linking table entry to assign an account to the contact.
    $contact->set_relationship('accounts_contacts', array('contact_id'=>$contact->id ,'account_id'=> $account_id), false);
    // This assumes that there will be one opportunity per company in the seed data.
    $opportunity_key = array_rand($opportunity_ids);
    $contact->set_relationship('opportunities_contacts', array('contact_id'=>$contact->id ,'opportunity_id'=> $opportunity_ids[$opportunity_key], 'contact_role'=>$app_list_strings['opportunity_relationship_type_default_key']), false);

    //Create new tasks
    $task = BeanFactory::newBean('Tasks');
    $key = array_rand($sugar_demodata['task_seed_data_names']);
    $task->name = $sugar_demodata['task_seed_data_names'][$key];
    //separate date and time field have been merged into one.
    $task->date_due = create_date() . ' ' . create_time();
    $task->date_due_flag = 0;
    $task->assigned_user_id = $contacts_account->assigned_user_id;
    $task->assigned_user_name = $contacts_account->assigned_user_name;
    $task->priority = array_rand($app_list_strings['task_priority_dom']);
    $task->status = array_rand($app_list_strings['task_status_dom']);
    $task->contact_id = $contact->id;
    if ($contact->primary_address_city == "San Mateo") {
        $task->parent_id = $account_id;
        $task->parent_type = 'Accounts';
    }
    $task->save();

    //Create new meetings
    $meeting = BeanFactory::newBean('Meetings');
    $key = array_rand($sugar_demodata['meeting_seed_data_names']);
    $meeting->name = $sugar_demodata['meeting_seed_data_names'][$key];
    $meeting->date_start = create_date(). ' ' . create_time();
    //$meeting->time_start = date("H:i",time());
    $meeting->duration_hours = array_rand($possible_duration_hours_arr);
    $meeting->duration_minutes = array_rand($possible_duration_minutes_arr);
    $meeting->assigned_user_id = $assigned_user_id;
    $meeting->assigned_user_id = $contacts_account->assigned_user_id;
    $meeting->assigned_user_name = $contacts_account->assigned_user_name;
    $meeting->description = $sugar_demodata['meeting_seed_data_descriptions'];
    $meeting->status = array_rand($app_list_strings['meeting_status_dom']);
    $meeting->contact_id = $contact->id;
    $meeting->parent_id = $account_id;
    $meeting->parent_type = 'Accounts';
    // dont update vcal
    $meeting->update_vcal  = false;
    $meeting->save();
    // leverage the seed user to set the acceptance status on the meeting.
    $seed_user->id = $meeting->assigned_user_id;
    $meeting->set_accept_status($seed_user, 'accept');

    //Create new emails
    $email = BeanFactory::newBean('Emails');
    $key = array_rand($sugar_demodata['email_seed_data_subjects']);
    $email->name = $sugar_demodata['email_seed_data_subjects'][$key];
    $email->date_start = create_date();
    $email->time_start = create_time();
    $email->duration_hours = array_rand($possible_duration_hours_arr);
    $email->duration_minutes = array_rand($possible_duration_minutes_arr);
    $email->assigned_user_id = $assigned_user_id;
    $email->assigned_user_id = $contacts_account->assigned_user_id;
    $email->assigned_user_name = $contacts_account->assigned_user_name;
    $email->description = $sugar_demodata['email_seed_data_descriptions'];
    $email->status = 'sent';
    $email->parent_id = $account_id;
    $email->parent_type = 'Accounts';
    $email->to_addrs = $contact->emailAddress->getPrimaryAddress($contact);
    $email->from_addr = $assignedUser->emailAddress->getPrimaryAddress($assignedUser);
    isValidEmailAddress($email->from_addr);
    $email->from_addr_name = $email->from_addr;
    $email->to_addrs_names = $email->to_addrs;
    $email->type = 'out';
    $email->save();
    $email->load_relationship('contacts');
    $email->contacts->add($contact);
    $email->load_relationship('accounts');
    $email->accounts->add($contacts_account);
}

for ($i=0; $i<$number_leads; $i++) {
    $lead = BeanFactory::newBean('Leads');
    $lead->account_name = $sugar_demodata['company_name_array'][mt_rand(0, $company_name_count-1)];
    $lead->first_name = $sugar_demodata['first_name_array'][mt_rand(0, $first_name_max)];
    $lead->last_name = $sugar_demodata['last_name_array'][mt_rand(0, $last_name_max)];
    $lead->primary_address_street = $sugar_demodata['street_address_array'][mt_rand(0, $street_address_max)];
    $lead->primary_address_city = $sugar_demodata['city_array'][mt_rand(0, $city_array_max)];
    $lead->lead_source = array_rand($app_list_strings['lead_source_dom']);
    $lead->title = $sugar_demodata['titles'][mt_rand(0, $title_max)];
    $lead->phone_work = create_phone_number();
    $lead->phone_home = create_phone_number();
    $lead->phone_mobile = create_phone_number();
    $lead->emailAddress->addAddress(createEmailAddress(), true);
    // Fill in a bogus address
    $lead->primary_address_state = $sugar_demodata['primary_address_state'];
    $leads_account = $accounts[$account_number];
    $lead->primary_address_state = $leads_account->billing_address_state;
    $lead->status = array_rand($app_list_strings['lead_status_dom']);
    $lead->lead_source = array_rand($app_list_strings['lead_source_dom']);
    if ($i % 3 == 1) {
        $lead->billing_address_state = $sugar_demodata['billing_address_state']['east'];
        $assigned_user_id = mt_rand(9, 10);
        if ($assigned_user_id == 9) {
            $lead->assigned_user_name = "seed_will";
            $lead->assigned_user_id = $lead->assigned_user_name."_id";
        } else {
            $lead->assigned_user_name = "seed_chris";
            $lead->assigned_user_id = $lead->assigned_user_name."_id";
        }

        $lead->assigned_user_id = $lead->assigned_user_name."_id";
    } else {
        $lead->billing_address_state = $sugar_demodata['billing_address_state']['west'];
        $assigned_user_id = mt_rand(6, 8);
        if ($assigned_user_id == 6) {
            $lead->assigned_user_name = "seed_sarah";
        } else {
            if ($assigned_user_id == 7) {
                $lead->assigned_user_name = "seed_sally";
            } else {
                $lead->assigned_user_name = "seed_max";
            }
        }

        $lead->assigned_user_id = $lead->assigned_user_name."_id";
    }


    // If this is a large scale test, switch to the bulk teams 90% of the time.
    if ($large_scale_test) {
        if (mt_rand(0, 100) < 90) {
            $assigned_team = $team_demo_data->get_random_team();
            $lead->assigned_user_name = $assigned_team;
        } else {
        }
    }
    $lead->primary_address_postalcode = mt_rand(10000, 99999);
    $lead->primary_address_country = $sugar_demodata['primary_address_country'];
    $lead->save();
}


///
/// SEED DATA FOR EMAIL TEMPLATES
///
if (!empty($sugar_demodata['emailtemplates_seed_data'])) {
    foreach ($sugar_demodata['emailtemplates_seed_data'] as $v) {
        $EmailTemp = BeanFactory::newBean('EmailTemplates');
        $EmailTemp->name = $v['name'];
        $EmailTemp->description = $v['description'];
        $EmailTemp->subject = $v['subject'];
        $EmailTemp->body = $v['text_body'];
        $EmailTemp->body_html = $v['body'];
        $EmailTemp->deleted = 0;
        $EmailTemp->published = 'off';
        $EmailTemp->text_only = 0;
        $id =$EmailTemp->save();
    }
}
///
/// SEED DATA FOR PROJECT AND PROJECT TASK
///
include_once('modules/Project/Project.php');
include_once('modules/ProjectTask/ProjectTask.php');
// Project: Audit Plan
$project = BeanFactory::newBean('Project');
$project->name = $sugar_demodata['project_seed_data']['audit']['name'];
$project->description = $sugar_demodata['project_seed_data']['audit']['description'];
$project->assigned_user_id = 1;
$project->estimated_start_date = $sugar_demodata['project_seed_data']['audit']['estimated_start_date'];
$project->estimated_end_date = $sugar_demodata['project_seed_data']['audit']['estimated_end_date'];
$project->status = $sugar_demodata['project_seed_data']['audit']['status'];
$project->priority = $sugar_demodata['project_seed_data']['audit']['priority'];
$audit_plan_id = $project->save();

$project_task_id_counter = 1;  // all the project task IDs cannot be 1, so using couter
foreach ($sugar_demodata['project_seed_data']['audit']['project_tasks'] as $v) {
    $project_task = BeanFactory::newBean('ProjectTask');
    $project_task->assigned_user_id = 1;
    $project_task->name = $v['name'];
    $project_task->date_start = $v['date_start'];
    $project_task->date_finish = $v['date_finish'];
    $project_task->project_id = $audit_plan_id;
    $project_task->project_task_id = $project_task_id_counter;
    $project_task->description = $v['description'];
    $project_task->duration = $v['duration'];
    $project_task->duration_unit = $v['duration_unit'];
    $project_task->percent_complete = $v['percent_complete'];
    $communicate_stakeholders_id = $project_task->save();

    $project_task_id_counter++;
}
