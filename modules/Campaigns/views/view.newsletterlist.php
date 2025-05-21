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

/**
 */


#[\AllowDynamicProperties]
class ViewNewsLetterList extends ViewList
{
    public function processSearchForm()
    {
        // we have a query
        if (!empty($_SERVER['HTTP_REFERER']) && preg_match('/action=EditView/', (string) $_SERVER['HTTP_REFERER'])) { // from EditView cancel
            $this->searchForm->populateFromArray($this->storeQuery->query);
        } else {
            $this->searchForm->populateFromRequest();
        }
        $where_clauses = $this->searchForm->generateSearchWhere(true, $this->seed->module_dir);
        $where_clauses[] = "campaigns.campaign_type in ('NewsLetter')";
        if ((is_countable($where_clauses) ? count($where_clauses) : 0) > 0) {
            $this->where = '('. implode(' ) AND ( ', $where_clauses) . ')';
        }
        $GLOBALS['log']->info("List View Where Clause: $this->where");


        echo $this->searchForm->display($this->headers);
    }
    
    /**
     * @see SugarView::preDisplay()
     */
    public function preDisplay()
    {
        global $mod_strings;
        $mod_strings['LBL_MODULE_TITLE'] = $mod_strings['LBL_NEWSLETTER_TITLE'];
        $mod_strings['LBL_LIST_FORM_TITLE'] = $mod_strings['LBL_NEWSLETTER_LIST_FORM_TITLE'];
        parent::preDisplay();
    }
}
