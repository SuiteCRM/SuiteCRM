<?php
/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2016 SalesAgility Ltd.
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
 * FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more
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
 * reasonably feasible for  technical reasons, the Appropriate Legal Notices must
 * display the words  "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * Class SpotsController.
 */
class SpotsController extends SugarController
{
    protected $nullSqlPlaceholder = '';
    protected $action_remap = array('DetailView' => 'editview', 'index' => 'listview');

    //These are the file paths for the cached results of the spot data sets
    protected $spotFilePath = 'cache/modules/Spots/';
    protected $accountsFilName = 'accounts';
    protected $servicesFileName = 'service';
    protected $salesFileName = 'sales';
    protected $leadsFileName = 'leads';
    protected $marketingsFileName = 'marketing';
    protected $marketingActivitiesFileName = 'marketingActivity';
    protected $activitiesFileName = 'activities';
    protected $quotesFileName = 'quotes';
    protected $fileSuffix = 'json';

    //This is to allow for the data and key values to be separated
    protected $keySeparator = '___';

    //This is when to consider a data file as stale and replace it (should not be an issue if the scheduler is running)
    //This is the time in seconds, so an hour is 3600
    protected $spotsStaleTime = 3600;

    /**
     * This returns a string of the type of db being used.
     *
     * @return a string of the type of db being used (mysql, mssql or undefined)
     */
    public function getDatabaseType()
    {
        global $sugar_config;
        $dbType = 'undefined';
        if ($sugar_config['dbconfig']['db_type'] == 'mysql') {
            $dbType = 'mysql';
        } elseif ($sugar_config['dbconfig']['db_type'] == 'mssql') {
            $dbType = 'mssql';
        }

        return $dbType;
    }

    /**
     * This takes in a type and checks if it is one of the enum types
     */
    public function isEnumType($type)
    {
        if($type === 'enum' || $type === 'dynamicenum' || $type === 'multienum'){
            return true;
        }
        else
        {
            return false;
        }
    }

    /**
     * This is a duplicate of the build_report_access_query in AOR_Report (here for autonomy).
     *
     * @param SugarBean $module the $module to return the access query for
     * @param string    $alias  the alias for the table
     *
     * @return string $where the where clause to represent access
     */
    public function buildSpotsAccessQuery(SugarBean $module, $alias)
    {
        $module->table_name = $alias;
        $where = '';
        if ($module->bean_implements('ACL') && ACLController::requireOwner($module->module_dir, 'list')) {
            global $current_user;
            $owner_where = $module->getOwnerWhere($current_user->id);
            $where = ' AND '.$owner_where;
        }

        if (file_exists('modules/SecurityGroups/SecurityGroup.php')) {
            /* BEGIN - SECURITY GROUPS */
            if ($module->bean_implements('ACL') && ACLController::requireSecurityGroup($module->module_dir, 'list')) {
                require_once 'modules/SecurityGroups/SecurityGroup.php';
                global $current_user;
                $owner_where = $module->getOwnerWhere($current_user->id);
                $group_where = SecurityGroup::getGroupWhere($alias, $module->module_dir, $current_user->id);
                if (!empty($owner_where)) {
                    $where .= ' AND ('.$owner_where.' or '.$group_where.') ';
                } else {
                    $where .= ' AND '.$group_where;
                }
            }
        }

        return $where;
    }

    /**
     * Returns the cached account file, will create it first if it is out of date / does not exist.
     *
     * @return string returns a string representation of the accounts file
     */
    public function action_getAccountsSpotsData()
    {
        $language = $GLOBALS['current_language'];
        $userId = $_SESSION['authenticated_user_id'];
        $fileLocation = $this->spotFilePath.$userId.'_'.$this->accountsFilName .'_' . $language . '.'. $this->fileSuffix;
        if (file_exists($fileLocation) && (time() - filemtime($fileLocation) < $this->spotsStaleTime)) {
            echo file_get_contents($fileLocation);
        } else {
            $this->action_createAccountsSpotsData($fileLocation);
            echo file_get_contents($fileLocation);
        }
    }

    /**
     * This creates the cached file for accounts.
     *
     * @param string $filepath the filepath to save the cached file
     */
    public function action_createAccountsSpotsData($filepath)
    {
        global $mod_strings;
        global $app_list_strings;

        $returnArray = [];
        $db = DBManagerFactory::getInstance();

        $query = <<<EOF
        SELECT
            COALESCE(name,'$this->nullSqlPlaceholder') as accountName,
            COALESCE(account_type,'$this->nullSqlPlaceholder') as account_type,
            COALESCE(industry,'$this->nullSqlPlaceholder') as industry,
            COALESCE(billing_address_country,'$this->nullSqlPlaceholder') as billing_address_country
        FROM accounts
        WHERE accounts.deleted = 0
EOF;

        $accounts = BeanFactory::getBean('Accounts');
        $aclWhere = $this->buildSpotsAccessQuery($accounts, $accounts->table_name);

        $queryString = $query.$aclWhere;

        $result = $db->query($queryString);

        while ($row = $db->fetchByAssoc($result)) {
            $x = new stdClass();
            $x->LBL_AN_ACCOUNTS_ACCOUNT_NAME = $row['accountName'].$this->keySeparator.$row['accountName'];
            if($this->isEnumType($accounts->field_defs['account_type']['type'])){
                $options = $accounts->field_defs['account_type']['options'];
                $label = $app_list_strings[$options][$row['account_type']];
                $x->LBL_AN_ACCOUNTS_ACCOUNT_TYPE = $row['account_type'].$this->keySeparator.$label;
            }else{
                $x->LBL_AN_ACCOUNTS_ACCOUNT_TYPE = $row['account_type'].$this->keySeparator.$row['account_type'];

            }
            if($this->isEnumType($accounts->field_defs['industry']['type'])){
                $options = $accounts->field_defs['industry']['options'];
                $label = $app_list_strings[$options][$row['industry']];
                $x->LBL_AN_ACCOUNTS_ACCOUNT_INDUSTRY = $row['industry'].$this->keySeparator.$label;
            }else{
                $x->LBL_AN_ACCOUNTS_ACCOUNT_INDUSTRY = $row['industry'].$this->keySeparator.$row['industry'];

            }
            $x->LBL_AN_ACCOUNTS_ACCOUNT_BILLING_COUNTRY = $row['billing_address_country'].$this->keySeparator.$row['billing_address_country'];
            $returnArray[] = $x;
        }
        file_put_contents($filepath, json_encode($returnArray));
    }

    /**
     * Returns the cached leads file, will create it first if it is out of date / does not exist.
     *
     * @return string returns a string representation of the leads file
     */
    public function action_getLeadsSpotsData()
    {
        $language = $GLOBALS['current_language'];
        $userId = $_SESSION['authenticated_user_id'];
        $fileLocation = $this->spotFilePath.$userId.'_'.$this->leadsFileName .'_' . $language . '.'. $this->fileSuffix;
        if (file_exists($fileLocation) && (time() - filemtime($fileLocation) < $this->spotsStaleTime)) {
            echo file_get_contents($fileLocation);
        } else {
            $this->action_createLeadsSpotsData($fileLocation);
            echo file_get_contents($fileLocation);
        }
    }

    /**
     * This creates the cached file for leads.
     *
     * @param string $filepath the filepath to save the cached file
     */
    public function action_createLeadsSpotsData($filepath)
    {
        global $mod_strings;
        global $app_list_strings;
        $returnArray = [];
        $db = DBManagerFactory::getInstance();

        $mysqlSelect = <<<EOF
        SELECT
            RTRIM(LTRIM(CONCAT(COALESCE(users.first_name,''),' ',COALESCE(users.last_name,'')))) as assignedUser,
            leads.status,
            COALESCE(lead_source, '$this->nullSqlPlaceholder') as leadSource,
			COALESCE(campaigns.name, '$this->nullSqlPlaceholder') as campaignName,
			CAST(YEAR(leads.date_entered) as CHAR(10)) as year,
            COALESCE(QUARTER(leads.date_entered),'$this->nullSqlPlaceholder') as quarter,
			concat('(',MONTH(leads.date_entered),') ',MONTHNAME(leads.date_entered)) as month,
			CAST(WEEK(leads.date_entered) as CHAR(5)) as week,
			DAYNAME(leads.date_entered) as day
EOF;

        $mssqlSelect = <<<EOF
        SELECT
            RTRIM(LTRIM(COALESCE(users.first_name,'')+' '+COALESCE(users.last_name,''))) as assignedUser,
            leads.status,
            COALESCE(lead_source, '$this->nullSqlPlaceholder') as leadSource,
			COALESCE(campaigns.name, '$this->nullSqlPlaceholder') as campaignName,
			CAST(YEAR(leads.date_entered) as CHAR(10)) as year,
            COALESCE(DATEPART(qq,leads.date_entered),'$this->nullSqlPlaceholder') as quarter,
			'(' + CAST(DATEPART(mm,leads.date_entered)as CHAR(12)) + ') ' + DATENAME(month,DATEPART(mm,leads.date_entered)) as month,
			CAST(DATEPART(wk,leads.date_entered) as CHAR(5)) as week,
			DATENAME(weekday,leads.date_entered) as day
EOF;

        $fromClause = <<<EOF
        FROM leads
        INNER JOIN users
            ON leads.assigned_user_id = users.id
		LEFT JOIN campaigns
			ON leads.campaign_id = campaigns.id
			AND campaigns.deleted = 0
EOF;
        $whereClause = <<<EOF
        WHERE leads.deleted = 0
        AND users.deleted = 0
EOF;

        $query = '';
        if ($this->getDatabaseType() === 'mssql') {
            $query = $mssqlSelect.' '.$fromClause.' '.$whereClause;
        } elseif ($this->getDatabaseType() === 'mysql') {
            $query = $mysqlSelect.' '.$fromClause.' '.$whereClause;
        } else {
            $GLOBALS['log']->error($mod_strings['LBL_AN_UNSUPPORTED_DB']);

            return;
        }

        $leads = BeanFactory::getBean('Leads');
        $users = BeanFactory::getBean('Users');
        $campaigns = BeanFactory::getBean('Campaigns');
        $aclWhereLeads = $this->buildSpotsAccessQuery($leads, $leads->table_name);
        $aclWhereUsers = $this->buildSpotsAccessQuery($users, $users->table_name);
        $aclWhereCampaigns = $this->buildSpotsAccessQuery($campaigns, $campaigns->table_name);

        $queryString = $query.$aclWhereLeads.$aclWhereUsers.$aclWhereCampaigns;
        $result = $db->query($queryString);

        while ($row = $db->fetchByAssoc($result)) {
            $x = new stdClass();
            $x->LBL_AN_LEADS_ASSIGNED_USER = $row['assignedUser'].$this->keySeparator.$row['assignedUser'];
            if($this->isEnumType($leads->field_defs['status']['type'])){
                $options = $leads->field_defs['status']['options'];
                $label = $app_list_strings[$options][$row['status']];
                $x->LBL_AN_LEADS_STATUS = $row['status'].$this->keySeparator.$label;
            }else{
                $x->LBL_AN_LEADS_STATUS = $row['status'].$this->keySeparator.$row['status'];

            }
            if($this->isEnumType($leads->field_defs['lead_source']['type'])){
                $options = $leads->field_defs['lead_source']['options'];
                $label = $app_list_strings[$options][$row['leadSource']];
                $x->LBL_AN_LEADS_LEAD_SOURCE = $row['leadSource'].$this->keySeparator.$label;
            }else{
                $x->LBL_AN_LEADS_LEAD_SOURCE = $row['leadSource'].$this->keySeparator.$row['leadSource'];

            }
            $x->LBL_AN_LEADS_CAMPAIGN_NAME = $row['campaignName'].$this->keySeparator.$row['campaignName'];
            $x->LBL_AN_LEADS_YEAR = $row['year'].$this->keySeparator.$row['year'];
            $x->LBL_AN_LEADS_QUARTER = $row['quarter'].$this->keySeparator.$row['quarter'];
            $x->LBL_AN_LEADS_MONTH = $row['month'].$this->keySeparator.$row['month'];
            $x->LBL_AN_LEADS_WEEK = $row['week'].$this->keySeparator.$row['week'];
            $x->LBL_AN_LEADS_DAY = $row['day'].$this->keySeparator.$row['day'];

            $returnArray[] = $x;
        }
        file_put_contents($filepath, json_encode($returnArray));
    }

    /**
     * Returns the cached sales file, will create it first if it is out of date / does not exist.
     *
     * @return string returns a string representation of the sales file
     */
    public function action_getSalesSpotsData()
    {
        $language = $GLOBALS['current_language'];
        $userId = $_SESSION['authenticated_user_id'];
        $fileLocation = $this->spotFilePath.$userId.'_'.$this->salesFileName .'_' . $language . '.'. $this->fileSuffix;
        if (file_exists($fileLocation) && (time() - filemtime($fileLocation) < $this->spotsStaleTime)) {
            echo file_get_contents($fileLocation);
        } else {
            $this->action_createSalesSpotsData($fileLocation);
            echo file_get_contents($fileLocation);
        }
    }

    /**
     * This creates the cached file for sales.
     *
     * @param string $filepath the filepath to save the cached file
     */
    public function action_createSalesSpotsData($filepath)
    {
        global $mod_strings;
        global $app_list_strings;
        $returnArray = [];
        $db = DBManagerFactory::getInstance();

        $mysqlSelect = <<<EOF
        SELECT
			accounts.name as accountName,
            opportunities.name as opportunityName,
            RTRIM(LTRIM(CONCAT(COALESCE(first_name,''),' ',COALESCE(last_name,'')))) as assignedUser,
            COALESCE(opportunity_type,'$this->nullSqlPlaceholder') as opportunityType,
            lead_source,
            amount,
            sales_stage,
            probability,
            date_closed as expectedCloseDate,
			COALESCE(QUARTER(date_closed),'$this->nullSqlPlaceholder') as salesQuarter,
			concat('(',MONTH(date_closed),') ',MONTHNAME(date_closed)) as salesMonth,
			CAST(WEEK(date_closed) as CHAR(5)) as salesWeek,
			DAYNAME(date_closed) as salesDay,
			CAST(YEAR(date_closed) as CHAR(10)) as salesYear,
            COALESCE(campaigns.name,'$this->nullSqlPlaceholder') as campaign
EOF;

        $mssqlSelect = <<<EOF
        SELECT
			accounts.name as accountName,
            opportunities.name as opportunityName,
            RTRIM(LTRIM(COALESCE(first_name,'')+' '+COALESCE(last_name,''))) as assignedUser,
            COALESCE(opportunity_type,'$this->nullSqlPlaceholder') as opportunityType,
            lead_source,
            amount,
            sales_stage,
            probability,
            date_closed as expectedCloseDate,
            COALESCE(DATEPART(qq,date_closed),'$this->nullSqlPlaceholder') as salesQuarter,
			'(' + CAST(DATEPART(mm,date_closed)as CHAR(12)) + ') ' + DATENAME(month,DATEPART(mm,date_closed)) as salesMonth,
			CAST(DATEPART(wk,date_closed) as CHAR(5)) as salesWeek,
			DATENAME(weekday,date_closed) as salesDay,
			CAST(YEAR(date_closed) as CHAR(10)) as salesYear,
            COALESCE(campaigns.name,'$this->nullSqlPlaceholder') as campaign
EOF;

        $fromClause = <<<EOF
        FROM opportunities
		INNER JOIN accounts_opportunities
			ON accounts_opportunities.opportunity_id = opportunities.id
		INNER JOIN accounts
			ON accounts_opportunities.account_id = accounts.id
        INNER JOIN users
            ON opportunities.assigned_user_id = users.id
        LEFT JOIN campaigns
            ON opportunities.campaign_id = campaigns.id
            AND campaigns.deleted = 0
EOF;
        $whereClause = <<<EOF
        WHERE opportunities.deleted = 0
        AND accounts_opportunities.deleted = 0
        AND accounts.deleted = 0
        AND users.deleted = 0
EOF;

        $query = '';
        if ($this->getDatabaseType() === 'mssql') {
            $query = $mssqlSelect.' '.$fromClause.' '.$whereClause;
        } elseif ($this->getDatabaseType() === 'mysql') {
            $query = $mysqlSelect.' '.$fromClause.' '.$whereClause;
        } else {
            $GLOBALS['log']->error($mod_strings['LBL_AN_UNSUPPORTED_DB']);

            return;
        }

        $opps = BeanFactory::getBean('Opportunities');
        $accounts = BeanFactory::getBean('Accounts');
        $users = BeanFactory::getBean('Users');
        $campaigns = BeanFactory::getBean('Campaigns');
        $aclWhereOpps = $this->buildSpotsAccessQuery($opps, $opps->table_name);
        $aclWhereAccounts = $this->buildSpotsAccessQuery($accounts, $accounts->table_name);
        $aclWhereUsers = $this->buildSpotsAccessQuery($users, $users->table_name);
        $aclWhereCampaigns = $this->buildSpotsAccessQuery($campaigns, $campaigns->table_name);

        $queryString = $query.$aclWhereOpps.$aclWhereAccounts.$aclWhereUsers.$aclWhereCampaigns;
        $result = $db->query($queryString);

        while ($row = $db->fetchByAssoc($result)) {
            $x = new stdClass();
            $x->LBL_AN_SALES_ACCOUNT_NAME = $row['accountName'].$this->keySeparator.$row['accountName'];
            $x->LBL_AN_SALES_OPPORTUNITY_NAME = $row['opportunityName'].$this->keySeparator.$row['opportunityName'];
            $x->LBL_AN_SALES_ASSIGNED_USER = $row['assignedUser'].$this->keySeparator.$row['assignedUser'];
            if($this->isEnumType($opps->field_defs['opportunity_type']['type'])){
                $options = $opps->field_defs['opportunity_type']['options'];
                $label = $app_list_strings[$options][$row['opportunityType']];
                $x->LBL_AN_SALES_OPPORTUNITY_TYPE = $row['opportunityType'].$this->keySeparator.$label;
            }else{
                $x->LBL_AN_SALES_OPPORTUNITY_TYPE = $row['opportunityType'].$this->keySeparator.$row['opportunityType'];
            }
            if($this->isEnumType($opps->field_defs['lead_source']['type'])){
                $options = $opps->field_defs['lead_source']['options'];
                $label = $app_list_strings[$options][$row['lead_source']];
                $x->LBL_AN_SALES_LEAD_SOURCE = $row['lead_source'].$this->keySeparator.$label;
            }else{
                $x->LBL_AN_SALES_LEAD_SOURCE = $row['lead_source'].$this->keySeparator.$row['lead_source'];
            }

            $x->LBL_AN_SALES_AMOUNT = $row['amount'].$this->keySeparator.$row['amount'];

            if($this->isEnumType($opps->field_defs['sales_stage']['type'])){
                $options = $opps->field_defs['sales_stage']['options'];
                $label = $app_list_strings[$options][$row['sales_stage']];
                $x->LBL_AN_SALES_STAGE = $row['sales_stage'].$this->keySeparator.$label;
            }else{
                $x->LBL_AN_SALES_STAGE = $row['sales_stage'].$this->keySeparator.$row['sales_stage'];
            }

            $x->LBL_AN_SALES_PROBABILITY = $row['probability'].$this->keySeparator.$row['probability'];
            $x->LBL_AN_SALES_DATE = $row['date_closed'].$this->keySeparator.$row['date_closed'];

            $x->LBL_AN_SALES_QUARTER = $row['salesQuarter'].$this->keySeparator.$row['salesQuarter'];
            $x->LBL_AN_SALES_MONTH = $row['salesMonth'].$this->keySeparator.$row['salesMonth'];
            $x->LBL_AN_SALES_WEEK = $row['salesWeek'].$this->keySeparator.$row['salesWeek'];
            $x->LBL_AN_SALES_DAY = $row['salesDay'].$this->keySeparator.$row['salesDay'];
            $x->LBL_AN_SALES_YEAR = $row['salesYear'].$this->keySeparator.$row['salesYear'];
            $x->LBL_AN_SALES_CAMPAIGN = $row['campaign'].$this->keySeparator.$row['campaign'];

            $returnArray[] = $x;
        }
        file_put_contents($filepath, json_encode($returnArray));
    }

    /**
     * Returns the cached service file, will create it first if it is out of date / does not exist.
     *
     * @return string returns a string representation of the service file
     */
    public function action_getServiceSpotsData()
    {
        $language = $GLOBALS['current_language'];
        $userId = $_SESSION['authenticated_user_id'];
        $fileLocation = $this->spotFilePath.$userId.'_'.$this->servicesFileName .'_' . $language . '.'. $this->fileSuffix;
        if (file_exists($fileLocation) && (time() - filemtime($fileLocation) < $this->spotsStaleTime)) {
            echo file_get_contents($fileLocation);
        } else {
            $this->action_createServiceSpotsData($fileLocation);
            echo file_get_contents($fileLocation);
        }
    }

    /**
     * This creates the cached file for service.
     *
     * @param string $filepath the filepath to save the cached file
     */
    public function action_createServiceSpotsData($filepath)
    {
        global $mod_strings;
        global $app_list_strings;
        $returnArray = [];
        $db = DBManagerFactory::getInstance();

        $mysqlSelect = <<<EOF
        SELECT
            accounts.name,
            cases.state,
            cases.status,
            cases.priority,
            DAYNAME(cases.date_entered) as day,
            CAST(WEEK(cases.date_entered) as CHAR(5)) as week,
            concat('(',MONTH(cases.date_entered),') ',MONTHNAME(cases.date_entered)) as month,
            COALESCE(QUARTER(cases.date_entered),'$this->nullSqlPlaceholder') as quarter,
            CAST(YEAR(cases.date_entered) as CHAR(10)) as year,
            COALESCE(NULLIF(RTRIM(LTRIM(CONCAT(COALESCE(u2.first_name,''),' ',COALESCE(u2.last_name,'')))),''),'$this->nullSqlPlaceholder') as contactName,
            RTRIM(LTRIM(CONCAT(COALESCE(users.first_name,''),' ',COALESCE(users.last_name,'')))) as assignedUser
EOF;
        $mssqlSelect = <<<EOF
        SELECT
            accounts.name,
            cases.state,
            cases.status,
            cases.priority,
            DATENAME(weekday,cases.date_entered) as day,
            CAST(DATEPART(wk,cases.date_entered) as CHAR(5)) as week,
            '(' + CAST(DATEPART(mm,cases.date_entered)as CHAR(12)) + ') ' + DATENAME(month,DATEPART(mm,cases.date_entered)) as month,
            COALESCE(DATEPART(qq,cases.date_entered),'$this->nullSqlPlaceholder') as quarter,
            CAST(YEAR(cases.date_entered) as CHAR(10)) as year,
            COALESCE(NULLIF(RTRIM(LTRIM(COALESCE(u2.first_name,'') + ' ' + COALESCE(u2.last_name,''))),''),'$this->nullSqlPlaceholder') as contactName,
            RTRIM(LTRIM(COALESCE(users.first_name,'') + ' ' + COALESCE(users.last_name,''))) as assignedUser
EOF;

        $fromClause = <<<EOF
        FROM cases
        INNER JOIN users
            ON cases.assigned_user_id = users.id
        INNER JOIN accounts
            ON cases.account_id = accounts.id
        LEFT JOIN users u2
            ON cases.contact_created_by_id = u2.id
            AND u2.deleted = 0
EOF;
        $whereClause = <<<EOF
        WHERE cases.deleted = 0
        AND users.deleted = 0
        AND accounts.deleted = 0
EOF;

        $query = '';
        if ($this->getDatabaseType() === 'mssql') {
            $query = $mssqlSelect.' '.$fromClause.' '.$whereClause;
        } elseif ($this->getDatabaseType() === 'mysql') {
            $query = $mysqlSelect.' '.$fromClause.' '.$whereClause;
        } else {
            $GLOBALS['log']->error($mod_strings['LBL_AN_UNSUPPORTED_DB']);

            return;
        }

        $cases = BeanFactory::getBean('Cases');
        $accounts = BeanFactory::getBean('Accounts');
        $users = BeanFactory::getBean('Users');
        $aclWhereCases = $this->buildSpotsAccessQuery($cases, $cases->table_name);
        $aclWhereAccounts = $this->buildSpotsAccessQuery($accounts, $accounts->table_name);
        $aclWhereUsers = $this->buildSpotsAccessQuery($users, $users->table_name);

        $queryString = $query.$aclWhereCases.$aclWhereAccounts.$aclWhereUsers;
        $result = $db->query($queryString);

        while ($row = $db->fetchByAssoc($result)) {
            $x = new stdClass();
            $x->LBL_AN_SERVICE_ACCOUNT_NAME = $row['name'].$this->keySeparator.$row['name'];
            if($this->isEnumType($cases->field_defs['state']['type'])){
                $options = $cases->field_defs['state']['options'];
                $label = $app_list_strings[$options][$row['state']];
                $x->LBL_AN_SERVICE_STATE = $row['state'].$this->keySeparator.$label;
            }else{
                $x->LBL_AN_SERVICE_STATE = $row['state'].$this->keySeparator.$row['state'];
            }

            if($this->isEnumType($cases->field_defs['status']['type'])){
                $options = $cases->field_defs['status']['options'];
                $label = $app_list_strings[$options][$row['status']];
                $x->LBL_AN_SERVICE_STATUS = $row['status'].$this->keySeparator.$label;
            }else{
                $x->LBL_AN_SERVICE_STATUS = $row['status'].$this->keySeparator.$row['status'];
            }

            if($this->isEnumType($cases->field_defs['priority']['type'])){
                $options = $cases->field_defs['priority']['options'];
                $label = $app_list_strings[$options][$row['priority']];
                $x->LBL_AN_SERVICE_PRIORITY = $row['priority'].$this->keySeparator.$label;
            }else{
                $x->LBL_AN_SERVICE_PRIORITY = $row['priority'].$this->keySeparator.$row['priority'];
            }

            $x->LBL_AN_SERVICE_CREATED_DAY = $row['day'].$this->keySeparator.$row['day'];
            $x->LBL_AN_SERVICE_CREATED_WEEK = $row['week'].$this->keySeparator.$row['week'];
            $x->LBL_AN_SERVICE_CREATED_MONTH = $row['month'].$this->keySeparator.$row['month'];
            $x->LBL_AN_SERVICE_CREATED_QUARTER = $row['quarter'].$this->keySeparator.$row['quarter'];
            $x->LBL_AN_SERVICE_CREATED_YEAR = $row['year'].$this->keySeparator.$row['year'];
            $x->LBL_AN_SERVICE_CONTACT_NAME = $row['contactName'].$this->keySeparator.$row['contactName'];
            $x->LBL_AN_SERVICE_ASSIGNED_TO = $row['assignedUser'].$this->keySeparator.$row['assignedUser'];

            $returnArray[] = $x;
        }
        file_put_contents($filepath, json_encode($returnArray));
    }

    /**
     * Returns the cached activities file, will create it first if it is out of date / does not exist.
     *
     * @return string returns a string representation of the activities file
     */
    public function action_getActivitiesSpotsData()
    {
        $language = $GLOBALS['current_language'];
        $userId = $_SESSION['authenticated_user_id'];
        $fileLocation = $this->spotFilePath.$userId.'_'.$this->activitiesFileName .'_' . $language . '.'. $this->fileSuffix;
        if (file_exists($fileLocation) && (time() - filemtime($fileLocation) < $this->spotsStaleTime)) {
            echo file_get_contents($fileLocation);
        } else {
            $this->action_createActivitiesSpotsData($fileLocation);
            echo file_get_contents($fileLocation);
        }
    }

    /**
     * This creates the cached file for activities.
     *
     * @param string $filepath the filepath to save the cached file
     */
    public function action_createActivitiesSpotsData($filepath)
    {
        global $mod_strings;
        global $app_list_strings;

        $returnArray = [];
        $db = DBManagerFactory::getInstance();

        $mysqlQueryCalls = <<<EOF
        SELECT
            'LBL_ACTIVITIES_CALL' as type
            , calls.name
            , calls.status
            , RTRIM(LTRIM(CONCAT(COALESCE(users.first_name,''),' ',COALESCE(users.last_name,'')))) as assignedUser
        FROM calls
        LEFT JOIN users
            ON calls.assigned_user_id = users.id
            AND users.deleted = 0
        WHERE calls.deleted = 0
EOF;

        $mysqlQueryMeetings = <<<EOF
        UNION
        SELECT
            'LBL_ACTIVITIES_MEETING' as type
            , meetings.name
            , meetings.status
            , RTRIM(LTRIM(CONCAT(COALESCE(users.first_name,''),' ',COALESCE(users.last_name,'')))) as assignedUser
        FROM meetings
        LEFT JOIN users
            ON meetings.assigned_user_id = users.id
            AND users.deleted = 0
        WHERE meetings.deleted = 0
EOF;

        $mysqlQueryTasks = <<<EOF
        UNION
        SELECT
            'LBL_ACTIVITIES_TASK' as type
            , tasks.name
            , tasks.status
            , RTRIM(LTRIM(CONCAT(COALESCE(users.first_name,''),' ',COALESCE(users.last_name,'')))) as assignedUser
        FROM tasks
        LEFT JOIN users
            ON tasks.assigned_user_id = users.id
            AND users.deleted = 0
        WHERE tasks.deleted = 0
EOF;

        $mssqlQueryCalls = <<<EOF
        SELECT
            'LBL_ACTIVITIES_CALL' as type
            , calls.name
            , calls.status
            , RTRIM(LTRIM(COALESCE(users.first_name,'') + ' ' + COALESCE(users.last_name,''))) as assignedUser
        FROM calls
        LEFT JOIN users
            ON calls.assigned_user_id = users.id
            AND users.deleted = 0
        WHERE calls.deleted = 0
EOF;
        $mssqlQueryMeetings = <<<EOF
        UNION
        SELECT
            'LBL_ACTIVITIES_MEETING' as type
            , meetings.name
            , meetings.status
            , RTRIM(LTRIM(COALESCE(users.first_name,'') + ' ' + COALESCE(users.last_name,''))) as assignedUser
        FROM meetings
        LEFT JOIN users
            ON meetings.assigned_user_id = users.id
            AND users.deleted = 0
        WHERE meetings.deleted = 0
EOF;
        $mssqlQueryTasks = <<<EOF
        UNION
        SELECT
            'LBL_ACTIVITIES_TASK' as type
            , tasks.name
            , tasks.status
            , RTRIM(LTRIM(COALESCE(users.first_name,'') + ' ' + COALESCE(users.last_name,''))) as assignedUser
        FROM tasks
        LEFT JOIN users
            ON tasks.assigned_user_id = users.id
            AND users.deleted = 0
        WHERE tasks.deleted = 0
EOF;

        $calls = BeanFactory::getBean('Calls');
        $aclWhereCalls = $this->buildSpotsAccessQuery($calls, $calls->table_name);
        $meetings = BeanFactory::getBean('Meetings');
        $aclWhereMeetings = $this->buildSpotsAccessQuery($meetings, $meetings->table_name);
        $tasks = BeanFactory::getBean('Tasks');
        $aclWhereTasks = $this->buildSpotsAccessQuery($tasks, $tasks->table_name);

        $query = '';
        if ($this->getDatabaseType() === 'mssql') {
            $query = $mssqlQueryCalls.$aclWhereCalls.$mssqlQueryMeetings.$aclWhereMeetings.$mssqlQueryTasks.$aclWhereTasks;
        } elseif ($this->getDatabaseType() === 'mysql') {
            $query = $mysqlQueryCalls.$aclWhereCalls.$mysqlQueryMeetings.$aclWhereMeetings.$mysqlQueryTasks.$aclWhereTasks;
        } else {
            $GLOBALS['log']->error($mod_strings['LBL_AN_UNSUPPORTED_DB']);

            return;
        }

        $result = $db->query($query);

        while ($row = $db->fetchByAssoc($result)) {
            $x = new stdClass();
            //Translate the hard-coded type value
            $type = $row['type'];
            $x->LBL_AN_ACTIVITIES_TYPE = $row['type'].$this->keySeparator.translate($row['type'],'Spots');;

            if($type === 'LBL_ACTIVITIES_CALL')
            {//Calls
                if($this->isEnumType($calls->field_defs['status']['type'])){
                    $options = $calls->field_defs['status']['options'];
                    $label = $app_list_strings[$options][$row['status']];
                    $x->LBL_AN_ACTIVITIES_STATUS = $row['status'].$this->keySeparator.$label;
                }else{
                    $x->LBL_AN_ACTIVITIES_STATUS = $row['status'].$this->keySeparator.$row['status'];
                }
            }
            elseif($type === 'LBL_ACTIVITIES_MEETING')
            {//Meetings
                if($this->isEnumType($meetings->field_defs['status']['type'])){
                    $options = $meetings->field_defs['status']['options'];
                    $label = $app_list_strings[$options][$row['status']];
                    $x->LBL_AN_ACTIVITIES_STATUS = $row['status'].$this->keySeparator.$label;
                }else{
                    $x->LBL_AN_ACTIVITIES_STATUS = $row['status'].$this->keySeparator.$row['status'];
                }
            }
            else
            {//Tasks
                if($this->isEnumType($tasks->field_defs['status']['type'])){
                    $options = $tasks->field_defs['status']['options'];
                    $label = $app_list_strings[$options][$row['status']];
                    $x->LBL_AN_ACTIVITIES_STATUS = $row['status'].$this->keySeparator.$label;
                }else{
                    $x->LBL_AN_ACTIVITIES_STATUS = $row['status'].$this->keySeparator.$row['status'];
                }
            }

            $x->LBL_AN_ACTIVITIES_NAME = $row['name'].$this->keySeparator.$row['name'];
            //$x->LBL_AN_ACTIVITIES_STATUS = $row['status'].$this->keySeparator.$row['status'];
            $x->LBL_AN_ACTIVITIES_ASSIGNED_TO = $row['assignedUser'].$this->keySeparator.$row['assignedUser'];

            $returnArray[] = $x;
        }
        file_put_contents($filepath, json_encode($returnArray));
    }

    /**
     * Returns the cached marketing file, will create it first if it is out of date / does not exist.
     *
     * @return string returns a string representation of the marketing file
     */
    public function action_getMarketingSpotsData()
    {
        $language = $GLOBALS['current_language'];
        $userId = $_SESSION['authenticated_user_id'];
        $fileLocation = $this->spotFilePath.$userId.'_'.$this->marketingsFileName .'_' . $language . '.'. $this->fileSuffix;
        if (file_exists($fileLocation) && (time() - filemtime($fileLocation) < $this->spotsStaleTime)) {
            echo file_get_contents($fileLocation);
        } else {
            $this->action_createMarketingSpotsData($fileLocation);
            echo file_get_contents($fileLocation);
        }
    }

    /**
     * This creates the cached file for marketing.
     *
     * @param string $filepath the filepath to save the cached file as
     */
    public function action_createMarketingSpotsData($filepath)
    {
        global $mod_strings;
        global $app_list_strings;
        $returnArray = [];
        $db = DBManagerFactory::getInstance();

        $mysqlSelect = <<<EOF
        SELECT
              COALESCE(campaigns.status,'$this->nullSqlPlaceholder') as campaignStatus
            , COALESCE(campaigns.campaign_type,'$this->nullSqlPlaceholder') as campaignType
            , COALESCE(campaigns.budget,'$this->nullSqlPlaceholder') as campaignBudget
            , COALESCE(campaigns.expected_cost,'$this->nullSqlPlaceholder') as campaignExpectedCost
            , COALESCE(campaigns.expected_revenue,'$this->nullSqlPlaceholder') as campaignExpectedRevenue
            , opportunities.name as opportunityName
            , opportunities.amount as opportunityAmount
            , opportunities.sales_stage as opportunitySalesStage
            , RTRIM(LTRIM(CONCAT(COALESCE(users.first_name,''),' ',COALESCE(users.last_name,'')))) as assignedUser
            , accounts.name as accountsName
EOF;

        $mssqlSelect = <<<EOF
        SELECT
              COALESCE(campaigns.status,'$this->nullSqlPlaceholder') as campaignStatus
            , COALESCE(campaigns.campaign_type,'$this->nullSqlPlaceholder') as campaignType
            , COALESCE(campaigns.budget,'$this->nullSqlPlaceholder') as campaignBudget
            , COALESCE(campaigns.expected_cost,'$this->nullSqlPlaceholder') as campaignExpectedCost
            , COALESCE(campaigns.expected_revenue,'$this->nullSqlPlaceholder') as campaignExpectedRevenue
            , opportunities.name as opportunityName
            , opportunities.amount as opportunityAmount
            , opportunities.sales_stage as opportunitySalesStage
            , RTRIM(LTRIM(COALESCE(users.first_name,'') + ' ' + COALESCE(users.last_name,''))) as assignedUser
            , accounts.name as accountsName
EOF;

        $fromClause = <<<EOF
        FROM opportunities
        LEFT JOIN users
            ON opportunities.assigned_user_id = users.id
            AND users.deleted = 0
        LEFT JOIN accounts_opportunities
            ON opportunities.id =  accounts_opportunities.opportunity_id
            AND accounts_opportunities.deleted = 0
        LEFT JOIN accounts
            ON accounts_opportunities.account_id = accounts.id
            AND accounts.deleted = 0
        LEFT JOIN campaigns
            ON opportunities.campaign_id = campaigns.id
            AND campaigns.deleted = 0
EOF;
        $whereClause = <<<EOF
        WHERE opportunities.deleted = 0
EOF;

        $query = '';
        if ($this->getDatabaseType() === 'mssql') {
            $query = $mssqlSelect.' '.$fromClause.' '.$whereClause;
        } elseif ($this->getDatabaseType() === 'mysql') {
            $query = $mysqlSelect.' '.$fromClause.' '.$whereClause;
        } else {
            $GLOBALS['log']->error($mod_strings['LBL_AN_UNSUPPORTED_DB']);

            return;
        }
        $opps = BeanFactory::getBean('Opportunities');
        $users = BeanFactory::getBean('Users');
        $accounts = BeanFactory::getBean('Accounts');
        $campaigns = BeanFactory::getBean('Campaigns');
        $aclWhereOpps = $this->buildSpotsAccessQuery($opps, $opps->table_name);
        $aclWhereUsers = $this->buildSpotsAccessQuery($users, $users->table_name);
        $aclWhereAccounts = $this->buildSpotsAccessQuery($accounts, $accounts->table_name);
        $aclWhereCampaigns = $this->buildSpotsAccessQuery($campaigns, $campaigns->table_name);

        $queryString = $query.$aclWhereOpps.$aclWhereUsers.$aclWhereAccounts.$aclWhereCampaigns;
        $result = $db->query($queryString);

        while ($row = $db->fetchByAssoc($result)) {
            $x = new stdClass();
            if($this->isEnumType($campaigns->field_defs['status']['type'])){
                $options = $campaigns->field_defs['status']['options'];
                $label = $app_list_strings[$options][$row['campaignStatus']];
                $x->LBL_AN_MARKETING_STATUS = $row['campaignStatus'].$this->keySeparator.$label;
            }else{
                $x->LBL_AN_MARKETING_STATUS = $row['campaignStatus'].$this->keySeparator.$row['campaignStatus'];
            }
            if($this->isEnumType($campaigns->field_defs['campaign_type']['type'])){
                $options = $campaigns->field_defs['campaign_type']['options'];
                $label = $app_list_strings[$options][$row['campaignType']];
                $x->LBL_AN_MARKETING_TYPE = $row['campaignType'].$this->keySeparator.$label;
            }else{
                $x->LBL_AN_MARKETING_TYPE = $row['campaignType'].$this->keySeparator.$row['campaignType'];
            }

            $x->LBL_AN_MARKETING_BUDGET = $row['campaignBudget'].$this->keySeparator.$row['campaignBudget'];
            $x->LBL_AN_MARKETING_EXPECTED_COST = $row['campaignExpectedCost'].$this->keySeparator.$row['campaignExpectedCost'];
            $x->LBL_AN_MARKETING_EXPECTED_REVENUE = $row['campaignExpectedRevenue'].$this->keySeparator.$row['campaignExpectedRevenue'];
            $x->LBL_AN_MARKETING_OPPORTUNITY_NAME = $row['opportunityName'].$this->keySeparator.$row['opportunityName'];
            $x->LBL_AN_MARKETING_OPPORTUNITY_AMOUNT = $row['opportunityAmount'].$this->keySeparator.$row['opportunityAmount'];
            if($this->isEnumType($opps->field_defs['sales_stage']['type'])){
                $options = $opps->field_defs['sales_stage']['options'];
                $label = $app_list_strings[$options][$row['opportunitySalesStage']];
                $x->LBL_AN_MARKETING_OPPORTUNITY_SALES_STAGE = $row['opportunitySalesStage'].$this->keySeparator.$label;
            }else{
                $x->LBL_AN_MARKETING_OPPORTUNITY_SALES_STAGE = $row['opportunitySalesStage'].$this->keySeparator.$row['opportunitySalesStage'];
            }
            $x->LBL_AN_MARKETING_OPPORTUNITY_ASSIGNED_TO = $row['assignedUser'].$this->keySeparator.$row['assignedUser'];
            $x->LBL_AN_MARKETING_ACCOUNT_NAME = $row['accountsName'].$this->keySeparator.$row['accountsName'];

            $returnArray[] = $x;
        }
        file_put_contents($filepath, json_encode($returnArray));
    }

    /**
     * Returns the cached marketing activity file, will create it first if it is out of date / does not exist.
     *
     * @return string returns a string representation of the marketing activity file
     */
    public function action_getMarketingActivitySpotsData()
    {
        $language = $GLOBALS['current_language'];
        $userId = $_SESSION['authenticated_user_id'];
        $fileLocation = $this->spotFilePath.$userId.'_'.$this->marketingActivitiesFileName .'_' . $language . '.'. $this->fileSuffix;
        if (file_exists($fileLocation) && (time() - filemtime($fileLocation) < $this->spotsStaleTime)) {
            echo file_get_contents($fileLocation);
        } else {
            $this->action_createMarketingActivitySpotsData($fileLocation);
            echo file_get_contents($fileLocation);
        }
    }

    /**
     * This creates the cached file for marketing activity.
     *
     * @param string $filepath the filepath to save the cached file
     */
    public function action_createMarketingActivitySpotsData($filepath)
    {
        global $mod_strings;
        global $app_list_strings;
        $returnArray = [];
        $db = DBManagerFactory::getInstance();

        $query = <<<EOF
        SELECT
            campaigns.name,
            campaign_log.activity_date,
            campaign_log.activity_type,
            campaign_log.related_type,
            campaign_log.related_id
        FROM campaigns
        LEFT JOIN campaign_log
            ON campaigns.id = campaign_log.campaign_id
            and campaign_log.deleted = 0
        where campaigns.deleted = 0

EOF;

        $campaigns = BeanFactory::getBean('Campaigns');
        $campaignLog = BeanFactory::getBean('CampaignLog');
        $aclWhereCampaigns = $this->buildSpotsAccessQuery($campaigns, $campaigns->table_name);

        $queryString = $query.$aclWhereCampaigns;
        $result = $db->query($queryString);

        while ($row = $db->fetchByAssoc($result)) {
            $x = new stdClass();
            $x->LBL_AN_MARKETINGACTIVITY_CAMPAIGN_NAME = $row['name'].$this->keySeparator.$row['name'];
            $x->LBL_AN_MARKETINGACTIVITY_ACTIVITY_DATE = $row['activity_date'].$this->keySeparator.$row['activity_date'];
            if($this->isEnumType($campaignLog->field_defs['activity_type']['type'])){
                $options = $campaignLog->field_defs['activity_type']['options'];
                $label = $app_list_strings[$options][$row['activity_type']];
                $x->LBL_AN_MARKETINGACTIVITY_ACTIVITY_TYPE = $row['activity_type'].$this->keySeparator.$label;
            }else{
                $x->LBL_AN_MARKETINGACTIVITY_ACTIVITY_TYPE = $row['activity_type'].$this->keySeparator.$row['activity_type'];
            }

            $x->LBL_AN_MARKETINGACTIVITY_RELATED_TYPE = $row['related_type'].$this->keySeparator.$row['related_type'];
            $x->LBL_AN_MARKETINGACTIVITY_RELATED_ID = $row['related_id'].$this->keySeparator.$row['related_id'];

            $returnArray[] = $x;
        }
        file_put_contents($filepath, json_encode($returnArray));
    }

    /**
     * Returns the cached quotes file, will create it first if it is out of date / does not exist.
     *
     * @return string returns a string representation of the quotes file
     */
    public function action_getQuotesSpotsData()
    {
        $language = $GLOBALS['current_language'];
        $userId = $_SESSION['authenticated_user_id'];
        $fileLocation = $this->spotFilePath.$userId.'_'.$this->quotesFileName .'_' . $language . '.'. $this->fileSuffix;
        if (file_exists($fileLocation) && (time() - filemtime($fileLocation) < $this->spotsStaleTime)) {
            echo file_get_contents($fileLocation);
        } else {
            $this->action_createQuotesSpotsData($fileLocation);
            echo file_get_contents($fileLocation);
        }
    }

    /**
     * This creates the cached file for quotes.
     *
     * @param string $filepath the filepath to save the cached file
     */
    public function action_createQuotesSpotsData($filepath)
    {
        global $mod_strings;
        global $app_list_strings;

        $returnArray = [];
        $db = DBManagerFactory::getInstance();

        $mysqlSelect = <<<EOF
            SELECT
            opportunities.name as opportunityName,
            opportunities.opportunity_type as opportunityType,
            opportunities.lead_source as opportunityLeadSource,
            opportunities.sales_stage as opportunitySalesStage,
            accounts.name as accountName,
            RTRIM(LTRIM(CONCAT(COALESCE(contacts.first_name,''),' ',COALESCE(contacts.last_name,'')))) as contactName,
            aos_products.name as productName,
            RTRIM(LTRIM(CONCAT(COALESCE(users.first_name,''),' ',COALESCE(users.last_name,'')))) as assignedUser,
            aos_products_quotes.product_qty as productQty,
            aos_products_quotes.product_list_price as productListPrice,
            aos_products_quotes.product_cost_price as productCostPrice,
            aos_products.price as productPrice,
            aos_products_quotes.product_discount as productDiscount,
            aos_quotes.discount_amount as discountAmount,
            aos_product_categories.name as categoryName,
            aos_products_quotes.product_total_price as productTotal,
            aos_quotes.total_amount as grandTotal,
            case
                when aos_products_quotes.product_id = 0 then 'Service'
                else 'Product'
            end itemType,
            aos_quotes.date_entered as dateCreated,
            DAYNAME(aos_quotes.date_entered) as dateCreatedDay,
            CAST(WEEK(aos_quotes.date_entered) as CHAR(5)) as dateCreatedWeek,
            concat('(',MONTH(aos_quotes.date_entered),') ',MONTHNAME(aos_quotes.date_entered)) as dateCreatedMonth,
            COALESCE(QUARTER(aos_quotes.date_entered),'$this->nullSqlPlaceholder') as dateCreatedQuarter,
            YEAR(aos_quotes.date_entered) as dateCreatedYear
EOF;

        $mssqlSelect = <<<EOF
            SELECT
            opportunities.name as opportunityName,
            opportunities.opportunity_type as opportunityType,
            opportunities.lead_source as opportunityLeadSource,
            opportunities.sales_stage as opportunitySalesStage,
            accounts.name as accountName,
            RTRIM(LTRIM(COALESCE(contacts.first_name,'') + ' ' + COALESCE(contacts.last_name,''))) as contactName,
            aos_products.name as productName,
            RTRIM(LTRIM(COALESCE(users.first_name,'') + ' ' + COALESCE(users.last_name,''))) as assignedUser,
            aos_products_quotes.product_qty as productQty,
            aos_products_quotes.product_list_price as productListPrice,
            aos_products_quotes.product_cost_price as productCostPrice,
            aos_products.price as productPrice,
            aos_products_quotes.product_discount as productDiscount,
            aos_quotes.discount_amount as discountAmount,
            aos_product_categories.name as categoryName,
            aos_products_quotes.product_total_price as productTotal,
            aos_quotes.total_amount as grandTotal,
            case
                when aos_products_quotes.product_id = 0 then 'Service'
                else 'Product'
            end itemType,
            aos_quotes.date_entered as dateCreated,
            DATENAME(weekday,aos_quotes.date_entered) as dateCreatedDay,
            CAST(DATEPART(wk,aos_quotes.date_entered) as CHAR(5)) as dateCreatedWeek,
            '(' + CAST(DATEPART(mm,aos_quotes.date_entered)as CHAR(12)) + ') ' + DATENAME(month,DATEPART(mm,aos_quotes.date_entered)) as dateCreatedMonth,
            COALESCE(DATEPART(qq,aos_quotes.date_entered),'$this->nullSqlPlaceholder') as dateCreatedQuarter,
            CAST(YEAR(aos_quotes.date_entered) as CHAR(10)) as dateCreatedYear
EOF;

        $fromClause = <<<EOF
        FROM aos_quotes
        LEFT JOIN accounts
            ON aos_quotes.billing_account_id = accounts.id
            AND accounts.deleted = 0
        LEFT JOIN contacts
            ON aos_quotes.billing_contact_id = contacts.id
            AND contacts.deleted = 0
        LEFT JOIN aos_products_quotes
            ON aos_quotes.id = aos_products_quotes.parent_id
            AND aos_products_quotes.deleted = 0
        LEFT JOIN aos_products
            ON aos_products_quotes.product_id = aos_products.id
            AND aos_products.deleted = 0
        LEFT JOIN opportunities
            ON aos_quotes.opportunity_id = opportunities.id
            AND opportunities.deleted = 0
        LEFT JOIN users
            ON aos_quotes.assigned_user_id = users.id
            AND users.deleted = 0
        LEFT JOIN aos_product_categories
            ON aos_products.aos_product_category_id = aos_product_categories.id
            AND aos_product_categories.deleted = 0
EOF;
        $whereClause = <<<EOF
        WHERE aos_quotes.deleted = 0
EOF;

        $query = '';
        if ($this->getDatabaseType() === 'mssql') {
            $query = $mssqlSelect.' '.$fromClause.' '.$whereClause;
        } elseif ($this->getDatabaseType() === 'mysql') {
            $query = $mysqlSelect.' '.$fromClause.' '.$whereClause;
        } else {
            $GLOBALS['log']->error($mod_strings['LBL_AN_UNSUPPORTED_DB']);

            return;
        }

        $opps = BeanFactory::getBean('Opportunities');
        $quotes = BeanFactory::getBean('AOS_Quotes');
        $accounts = BeanFactory::getBean('Accounts');
        $contacts = BeanFactory::getBean('Contacts');
        $aosProductQuotes = BeanFactory::getBean('AOS_Products_Quotes');
        $aosProducts = BeanFactory::getBean('AOS_Products');
        $users = BeanFactory::getBean('Users');
        $asoProductCategories = BeanFactory::getBean('AOS_Product_Categories');

        $aclWhereOpps = $this->buildSpotsAccessQuery($opps, $opps->table_name);
        $aclWhereQuotes = $this->buildSpotsAccessQuery($quotes, $quotes->table_name);
        $aclWhereAccounts = $this->buildSpotsAccessQuery($accounts, $accounts->table_name);
        $aclWhereContacts = $this->buildSpotsAccessQuery($contacts, $contacts->table_name);
        $aclWhereProductQuotes = $this->buildSpotsAccessQuery($aosProductQuotes, $aosProductQuotes->table_name);
        $aclWhereUsers = $this->buildSpotsAccessQuery($users, $users->table_name);
        $aclWhereProductCategories = $this->buildSpotsAccessQuery($asoProductCategories, $asoProductCategories->table_name);
        $aclWhereProducts = $this->buildSpotsAccessQuery($aosProducts, $aosProducts->table_name);

        $queryString = $query.$aclWhereOpps.$aclWhereQuotes.$aclWhereAccounts.$aclWhereContacts.$aclWhereProductQuotes.$aclWhereUsers.$aclWhereProductCategories.$aclWhereProducts;
        $result = $db->query($queryString);


        while ($row = $db->fetchByAssoc($result)) {
            $x = new stdClass();
            $x->LBL_AN_QUOTES_OPPORTUNITY_NAME = $row['opportunityName'].$this->keySeparator.$row['opportunityName'];
            if($this->isEnumType($opps->field_defs['opportunity_type']['type'])){
                $options = $opps->field_defs['opportunity_type']['options'];
                $label = $app_list_strings[$options][$row['opportunityType']];
                $x->LBL_AN_QUOTES_OPPORTUNITY_TYPE = $row['opportunityType'].$this->keySeparator.$label;
            }else{
                $x->LBL_AN_QUOTES_OPPORTUNITY_TYPE = $row['opportunityType'].$this->keySeparator.$row['opportunityType'];
            }

            if($this->isEnumType($opps->field_defs['lead_source']['type'])){
                $options = $opps->field_defs['lead_source']['options'];
                $label = $app_list_strings[$options][$row['opportunityLeadSource']];
                $x->LBL_AN_QUOTES_OPPORTUNITY_LEAD_SOURCE = $row['opportunityLeadSource'].$this->keySeparator.$label;
            }else{
                $x->LBL_AN_QUOTES_OPPORTUNITY_LEAD_SOURCE = $row['opportunityLeadSource'].$this->keySeparator.$row['opportunityLeadSource'];
            }

            if($this->isEnumType($opps->field_defs['sales_stage']['type'])){
                $options = $opps->field_defs['sales_stage']['options'];
                $label = $app_list_strings[$options][$row['opportunitySalesStage']];
                $x->LBL_AN_QUOTES_OPPORTUNITY_SALES_STAGE = $row['opportunitySalesStage'].$this->keySeparator.$label;
            }else{
                $x->LBL_AN_QUOTES_OPPORTUNITY_SALES_STAGE = $row['opportunitySalesStage'].$this->keySeparator.$row['opportunitySalesStage'];
            }

            $x->LBL_AN_QUOTES_ACCOUNT_NAME = $row['accountName'].$this->keySeparator.$row['accountName'];
            $x->LBL_AN_QUOTES_CONTACT_NAME = $row['contactName'].$this->keySeparator.$row['contactName'];
            $x->LBL_AN_QUOTES_ITEM_NAME = $row['productName'].$this->keySeparator.$row['productName'];
            $x->LBL_AN_QUOTES_ITEM_TYPE = $row['itemType'].$this->keySeparator.$row['itemType'];
            $x->LBL_AN_QUOTES_ITEM_CATEGORY = $row['categoryName'].$this->keySeparator.$row['categoryName'];
            $x->LBL_AN_QUOTES_ITEM_QTY = $row['productQty'].$this->keySeparator.$row['productQty'];
            $x->LBL_AN_QUOTES_ITEM_LIST_PRICE = $row['productListPrice'].$this->keySeparator.$row['productListPrice'];
            $x->LBL_AN_QUOTES_ITEM_SALE_PRICE = $row['productPrice'].$this->keySeparator.$row['productPrice'];
            $x->LBL_AN_QUOTES_ITEM_COST_PRICE = $row['productCostPrice'].$this->keySeparator.$row['productCostPrice'];
            $x->LBL_AN_QUOTES_ITEM_DISCOUNT_PRICE = $row['productDiscount'].$this->keySeparator.$row['productDiscount'];
            $x->LBL_AN_QUOTES_ITEM_DISCOUNT_AMOUNT = $row['discountAmount'].$this->keySeparator.$row['discountAmount'];
            $x->LBL_AN_QUOTES_ITEM_TOTAL = $row['productTotal'].$this->keySeparator.$row['productTotal'];
            $x->LBL_AN_QUOTES_GRAND_TOTAL = $row['grandTotal'].$this->keySeparator.$row['grandTotal'];
            $x->LBL_AN_QUOTES_ASSIGNED_TO = $row['assignedUser'].$this->keySeparator.$row['assignedUser'];
            $x->LBL_AN_QUOTES_DATE_CREATED = $row['dateCreated'].$this->keySeparator.$row['dateCreated'];
            $x->LBL_AN_QUOTES_DAY_CREATED = $row['dateCreatedDay'].$this->keySeparator.$row['dateCreatedDay'];
            $x->LBL_AN_QUOTES_WEEK_CREATED = $row['dateCreatedWeek'].$this->keySeparator.$row['dateCreatedWeek'];
            $x->LBL_AN_QUOTES_MONTH_CREATED = $row['dateCreatedMonth'].$this->keySeparator.$row['dateCreatedMonth'];
            $x->LBL_AN_QUOTES_QUARTER_CREATED = $row['dateCreatedQuarter'].$this->keySeparator.$row['dateCreatedQuarter'];
            $x->LBL_AN_QUOTES_YEAR_CREATED = $row['dateCreatedYear'].$this->keySeparator.$row['dateCreatedYear'];

            $returnArray[] = $x;
        }
        file_put_contents($filepath, json_encode($returnArray));
    }
}