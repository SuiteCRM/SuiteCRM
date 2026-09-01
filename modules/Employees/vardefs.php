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

global $dictionary;
if (empty($dictionary['User'])) {
    include('modules/Users/vardefs.php');
}
$dictionary['Employee']=$dictionary['User'];
//users of employees modules are not allowed to change the employee/user status.
$dictionary['Employee']['fields']['status']['massupdate']=false;
$dictionary['Employee']['fields']['is_admin']['massupdate']=false;
//begin bug 48033
$dictionary['Employee']['fields']['UserType']['massupdate']=false;
$dictionary['Employee']['fields']['messenger_type']['massupdate']=false;
$dictionary['Employee']['fields']['email_link_type']['massupdate']=false;
//end bug 48033
$dictionary['Employee']['fields']['email1']['required']=false;
$dictionary['Employee']['fields']['email_addresses']['required']=false;
$dictionary['Employee']['fields']['email_addresses_primary']['required']=false;
// bugs 47553 & 49716
$dictionary['Employee']['fields']['status']['studio']=false;
$dictionary['Employee']['fields']['status']['required']=false;

// Employees is a directory view open to every authenticated user - hide internal/
// bookkeeping and relationship-metadata fields that don't belong in that view.
$dictionary['Employee']['fields']['UserType']['api-visible']=false;
$dictionary['Employee']['fields']['receive_notifications']['api-visible']=false;
$dictionary['Employee']['fields']['date_entered']['api-visible']=false;
$dictionary['Employee']['fields']['date_modified']['api-visible']=false;
$dictionary['Employee']['fields']['modified_user_id']['api-visible']=false;
$dictionary['Employee']['fields']['modified_by_name']['api-visible']=false;
$dictionary['Employee']['fields']['created_by']['api-visible']=false;
$dictionary['Employee']['fields']['created_by_name']['api-visible']=false;
$dictionary['Employee']['fields']['deleted']['api-visible']=false;
$dictionary['Employee']['fields']['portal_only']['api-visible']=false;
$dictionary['Employee']['fields']['show_on_employees']['api-visible']=false;
$dictionary['Employee']['fields']['is_group']['api-visible']=false;
$dictionary['Employee']['fields']['c_accept_status_fields']['api-visible']=false;
$dictionary['Employee']['fields']['m_accept_status_fields']['api-visible']=false;
$dictionary['Employee']['fields']['accept_status_id']['api-visible']=false;
$dictionary['Employee']['fields']['accept_status_name']['api-visible']=false;
$dictionary['Employee']['fields']['prospect_lists']['api-visible']=false;
$dictionary['Employee']['fields']['eapm']['api-visible']=false;
$dictionary['Employee']['fields']['oauth_tokens']['api-visible']=false;
$dictionary['Employee']['fields']['project_resource']['api-visible']=false;
$dictionary['Employee']['fields']['project_users_1']['api-visible']=false;
$dictionary['Employee']['fields']['am_projecttemplates_resources']['api-visible']=false;
$dictionary['Employee']['fields']['am_projecttemplates_users_1']['api-visible']=false;
$dictionary['Employee']['fields']['SecurityGroups']['api-visible']=false;
$dictionary['Employee']['fields']['securitygroup_noninher_fields']['api-visible']=false;
$dictionary['Employee']['fields']['securitygroup_noninherit_id']['api-visible']=false;
$dictionary['Employee']['fields']['securitygroup_noninheritable']['api-visible']=false;
$dictionary['Employee']['fields']['securitygroup_primary_group']['api-visible']=false;
$dictionary['Employee']['fields']['factor_auth']['api-visible']=false;
$dictionary['Employee']['fields']['factor_auth_interface']['api-visible']=false;
$dictionary['Employee']['fields']['calls']['api-visible']=false;
$dictionary['Employee']['fields']['meetings']['api-visible']=false;
$dictionary['Employee']['fields']['email_link_type']['api-visible']=false;
$dictionary['Employee']['fields']['editor_type']['api-visible']=false;
$dictionary['Employee']['fields']['reportees']['api-visible']=false;
$dictionary['Employee']['fields']['aclroles']['api-visible']=false;
$dictionary['Employee']['fields']['calendar_accounts']['api-visible']=false;
$dictionary['Employee']['fields']['contacts_sync']['api-visible']=false;
