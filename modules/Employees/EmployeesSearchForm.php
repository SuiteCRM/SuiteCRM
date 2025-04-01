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


require_once('include/SearchForm/SearchForm2.php');

#[\AllowDynamicProperties]
class EmployeesSearchForm extends SearchForm
{
    /**
     * This builds an EmployeesSearchForm from a classic search form.
     */
    public function __construct(SearchForm $oldSearchForm)
    {
        parent::__construct($oldSearchForm->seed, $oldSearchForm->module, $oldSearchForm->action);
        $this->setup(
            // $searchdefs
            array($oldSearchForm->module => $oldSearchForm->searchdefs),
            // $searchFields
            array($oldSearchForm->module => $oldSearchForm->searchFields),
            // $tpl
            $oldSearchForm->tpl,
            // $displayView
            $oldSearchForm->displayView,
            // listViewDefs
            $oldSearchForm->listViewDefs
        );
        
        $this->lv = $oldSearchForm->lv;
    }
    
    public function generateSearchWhere($add_custom_fields = false, $module = '')
    {
        $onlyActive = false;
        if (isset($this->searchFields['open_only_active_users']['value'])) {
            if ($this->searchFields['open_only_active_users']['value'] == 1) {
                $onlyActive = true;
            }
            unset($this->searchFields['open_only_active_users']['value']);
        }
        $where_clauses = parent::generateSearchWhere($add_custom_fields, $module);
        
        if ($onlyActive) {
            $where_clauses[] = "users.employee_status = 'Active'";
        }
        
        // Add in code to remove portal/group/hidden users
        $where_clauses[] = "users.portal_only = 0";
        $where_clauses[] = "(users.is_group = 0 or users.is_group is null)";
        $where_clauses[] = "users.show_on_employees = 1";
        return $where_clauses;
    }
}
