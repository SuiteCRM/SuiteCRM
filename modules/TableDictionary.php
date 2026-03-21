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


include("metadata/accounts_bugsMetaData.php");
include("metadata/accounts_casesMetaData.php");
include("metadata/accounts_contactsMetaData.php");
include("metadata/accounts_opportunitiesMetaData.php");
include("metadata/calls_contactsMetaData.php");
include("metadata/calendar_accounts_meetingsMetaData.php");
include("metadata/calls_usersMetaData.php");
include("metadata/calls_leadsMetaData.php");
include("metadata/cases_bugsMetaData.php");
include("metadata/contacts_bugsMetaData.php");
include("metadata/contacts_casesMetaData.php");
include("metadata/configMetaData.php");
include("metadata/contacts_usersMetaData.php");
include("metadata/custom_fieldsMetaData.php");
include("metadata/email_addressesMetaData.php");
include("metadata/emails_beansMetaData.php");
include("metadata/foldersMetaData.php");
include("metadata/import_mapsMetaData.php");
include("metadata/meetings_contactsMetaData.php");
include("metadata/meetings_usersMetaData.php");
include("metadata/meetings_leadsMetaData.php");
include("metadata/opportunities_contactsMetaData.php");
include("metadata/user_feedsMetaData.php");
include("metadata/users_passwordLinkMetaData.php");
include("metadata/prospect_list_campaignsMetaData.php");
include("metadata/prospect_lists_prospectsMetaData.php");
include("metadata/roles_modulesMetaData.php");
include("metadata/roles_usersMetaData.php");
//include("metadata/project_relationMetaData.php");
include("metadata/outboundEmailMetaData.php");
include("metadata/addressBookMetaData.php");
include("metadata/project_bugsMetaData.php");
include("metadata/project_casesMetaData.php");
include("metadata/project_productsMetaData.php");
include("metadata/projects_accountsMetaData.php");
include("metadata/projects_contactsMetaData.php");
include("metadata/projects_opportunitiesMetaData.php");



//ACL RELATIONSHIPS
include("metadata/acl_roles_actionsMetaData.php");
include("metadata/acl_roles_usersMetaData.php");
// INBOUND EMAIL
include("metadata/inboundEmail_autoreplyMetaData.php");
include("metadata/inboundEmail_cacheTimestampMetaData.php");
include("metadata/email_cacheMetaData.php");
include("metadata/email_marketing_prospect_listsMetaData.php");
include("metadata/users_signaturesMetaData.php");
//linked documents.
include("metadata/linked_documentsMetaData.php");

// Documents, so we can start replacing Notes as the primary way to attach something to something else.
include("metadata/documents_accountsMetaData.php");
include("metadata/documents_contactsMetaData.php");
include("metadata/documents_opportunitiesMetaData.php");
include("metadata/documents_casesMetaData.php");
include("metadata/documents_bugsMetaData.php");
include("metadata/oauth_nonce.php");
include("metadata/cron_remove_documentsMetaData.php");

//konwledge base
include('metadata/aok_knowledgebase_categoriesMetaData.php');

include('metadata/am_projecttemplates_project_1MetaData.php');
include('metadata/am_projecttemplates_contacts_1MetaData.php');
include('metadata/am_projecttemplates_users_1MetaData.php');

include('metadata/am_tasktemplates_am_projecttemplatesMetaData.php');
include('metadata/aos_contracts_documentsMetaData.php');
include('metadata/aos_quotes_aos_contractsMetaData.php');
include('metadata/aos_quotes_aos_invoicesMetaData.php');
include('metadata/aos_quotes_projectMetaData.php');
include('metadata/aow_processed_aow_actionsMetaData.php');
include('metadata/fp_event_locations_fp_events_1MetaData.php');
include('metadata/fp_events_contactsMetaData.php');
include('metadata/fp_events_fp_event_delegates_1MetaData.php');
include('metadata/fp_events_fp_event_locations_1MetaData.php');
include('metadata/fp_events_leads_1MetaData.php');
include('metadata/fp_events_prospects_1MetaData.php');
include('metadata/jjwg_maps_jjwg_areasMetaData.php');
include('metadata/jjwg_maps_jjwg_markersMetaData.php');
include('metadata/project_contacts_1MetaData.php');
include('metadata/project_users_1MetaData.php');
include('metadata/securitygroups_acl_rolesMetaData.php');
include('metadata/securitygroups_defaultsMetaData.php');
include('metadata/securitygroups_recordsMetaData.php');
include('metadata/securitygroups_usersMetaData.php');

include 'metadata/surveyquestionoptions_surveyquestionresponsesMetaData.php';

if (file_exists('custom/application/Ext/TableDictionary/tabledictionary.ext.php')) {
    include('custom/application/Ext/TableDictionary/tabledictionary.ext.php');
}
