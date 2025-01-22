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

/**
 * Class SpotsController.
 */
#[\AllowDynamicProperties]
class SpotsController extends SugarController
{
    protected $nullSqlPlaceholder = '';
    protected $action_remap = array('DetailView' => 'editview', 'index' => 'listview');

    //These are the file paths for the cached results of the spot data sets
    protected $spotFilePath = 'cache/modules/Spots/';
    protected $accountsFileName = 'accounts.json';
    protected $servicesFileName = 'service.json';
    protected $salesFileName = 'sales.json';
    protected $leadsFileName = 'leads.json';
    protected $marketingsFileName = 'marketing.json';
    protected $marketingActivitiesFileName = 'marketingActivity.json';
    protected $activitiesFileName = 'activities.json';
    protected $quotesFileName = 'quotes.json';

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

        return $module->buildAccessWhere('list');
    }

    /**
     * Returns the cached account file, will create it first if it is out of date / does not exist.
     *
     * @return string returns a string representation of the accounts file
     */
    public function action_getAccountsSpotsData()
    {
        $userId = $_SESSION['authenticated_user_id'];
        $fileLocation = $this->spotFilePath.$userId.'_'.$this->accountsFileName;
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
        $returnArray = array();
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
            $x->{$mod_strings['LBL_AN_ACCOUNTS_ACCOUNT_NAME']} = $row['accountName'];
            $x->{$mod_strings['LBL_AN_ACCOUNTS_ACCOUNT_TYPE']} = $row['account_type'];
            $x->{$mod_strings['LBL_AN_ACCOUNTS_ACCOUNT_INDUSTRY']} = $row['industry'];
            $x->{$mod_strings['LBL_AN_ACCOUNTS_ACCOUNT_BILLING_COUNTRY']} = $row['billing_address_country'];
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
        $userId = $_SESSION['authenticated_user_id'];
        $fileLocation = $this->spotFilePath.$userId.'_'.$this->leadsFileName;
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
        $returnArray = array();
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
            $x->{$mod_strings['LBL_AN_LEADS_ASSIGNED_USER']} = $row['assignedUser'];
            $x->{$mod_strings['LBL_AN_LEADS_STATUS']} = $row['status'];
            $x->{$mod_strings['LBL_AN_LEADS_LEAD_SOURCE']} = $row['leadSource'];
            $x->{$mod_strings['LBL_AN_LEADS_CAMPAIGN_NAME']} = $row['campaignName'];
            $x->{$mod_strings['LBL_AN_LEADS_YEAR']} = $row['year'];
            $x->{$mod_strings['LBL_AN_LEADS_QUARTER']} = $row['quarter'];
            $x->{$mod_strings['LBL_AN_LEADS_MONTH']} = $row['month'];
            $x->{$mod_strings['LBL_AN_LEADS_WEEK']} = $row['week'];
            $x->{$mod_strings['LBL_AN_LEADS_DAY']} = $row['day'];

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
        $userId = $_SESSION['authenticated_user_id'];
        $fileLocation = $this->spotFilePath.$userId.'_'.$this->salesFileName;
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
        $returnArray = array();
        $db = DBManagerFactory::getInstance();

        $mysqlSelect = <<<EOF
        SELECT
			accounts.name as accountName,
            opportunities.name as opportunityName,
            RTRIM(LTRIM(CONCAT(COALESCE(first_name,''),' ',COALESCE(last_name,'')))) as assignedUser,
            COALESCE(opportunity_type,'$this->nullSqlPlaceholder') as opportunity_type,
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
            COALESCE(opportunity_type,'$this->nullSqlPlaceholder') as opportunity_type,
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
            $x->{$mod_strings['LBL_AN_SALES_ACCOUNT_NAME']} = $row['accountName'];
            $x->{$mod_strings['LBL_AN_SALES_OPPORTUNITY_NAME']} = $row['opportunityName'];
            $x->{$mod_strings['LBL_AN_SALES_ASSIGNED_USER']} = $row['assignedUser'];
            $x->{$mod_strings['LBL_AN_SALES_OPPORTUNITY_TYPE']} = $row['opportunity_type'];
            $x->{$mod_strings['LBL_AN_SALES_LEAD_SOURCE']} = $row['lead_source'];
            $x->{$mod_strings['LBL_AN_SALES_AMOUNT']} = $row['amount'];
            $x->{$mod_strings['LBL_AN_SALES_STAGE']} = $row['sales_stage'];
            $x->{$mod_strings['LBL_AN_SALES_PROBABILITY']} = $row['probability'];
            $x->{$mod_strings['LBL_AN_SALES_DATE']} = $row['expectedCloseDate'];

            $x->{$mod_strings['LBL_AN_SALES_QUARTER']} = $row['salesQuarter'];
            $x->{$mod_strings['LBL_AN_SALES_MONTH']} = $row['salesMonth'];
            $x->{$mod_strings['LBL_AN_SALES_WEEK']} = $row['salesWeek'];
            $x->{$mod_strings['LBL_AN_SALES_DAY']} = $row['salesDay'];
            $x->{$mod_strings['LBL_AN_SALES_YEAR']} = $row['salesYear'];
            $x->{$mod_strings['LBL_AN_SALES_CAMPAIGN']} = $row['campaign'];

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
        $userId = $_SESSION['authenticated_user_id'];
        $fileLocation = $this->spotFilePath.$userId.'_'.$this->servicesFileName;
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
        $returnArray = array();
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
            $x->{$mod_strings['LBL_AN_SERVICE_ACCOUNT_NAME']} = $row['name'];
            $x->{$mod_strings['LBL_AN_SERVICE_STATE']} = $row['state'];
            $x->{$mod_strings['LBL_AN_SERVICE_STATUS']} = $row['status'];
            $x->{$mod_strings['LBL_AN_SERVICE_PRIORITY']} = $row['priority'];
            $x->{$mod_strings['LBL_AN_SERVICE_CREATED_DAY']} = $row['day'];
            $x->{$mod_strings['LBL_AN_SERVICE_CREATED_WEEK']} = $row['week'];
            $x->{$mod_strings['LBL_AN_SERVICE_CREATED_MONTH']} = $row['month'];
            $x->{$mod_strings['LBL_AN_SERVICE_CREATED_QUARTER']} = $row['quarter'];
            $x->{$mod_strings['LBL_AN_SERVICE_CREATED_YEAR']} = $row['year'];
            $x->{$mod_strings['LBL_AN_SERVICE_CONTACT_NAME']} = $row['contactName'];
            $x->{$mod_strings['LBL_AN_SERVICE_ASSIGNED_TO']} = $row['assignedUser'];

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
        $userId = $_SESSION['authenticated_user_id'];
        $fileLocation = $this->spotFilePath.$userId.'_'.$this->activitiesFileName;
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
        $returnArray = array();
        $db = DBManagerFactory::getInstance();

        $mysqlQueryCalls = <<<EOF
        SELECT
            'call' as type
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
        UNION ALL
        SELECT
            'meeting' as type
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
        UNION ALL
        SELECT
            'task' as type
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
            'call' as type
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
        UNION ALL
        SELECT
            'meeting' as type
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
        UNION ALL
        SELECT
            'task' as type
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
            $x->{$mod_strings['LBL_AN_ACTIVITIES_TYPE']} = $row['type'];
            $x->{$mod_strings['LBL_AN_ACTIVITIES_NAME']} = $row['name'];
            $x->{$mod_strings['LBL_AN_ACTIVITIES_STATUS']} = $row['status'];
            $x->{$mod_strings['LBL_AN_ACTIVITIES_ASSIGNED_TO']} = $row['assignedUser'];

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
        $userId = $_SESSION['authenticated_user_id'];
        $fileLocation = $this->spotFilePath.$userId.'_'.$this->marketingsFileName;
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
        $returnArray = array();
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
            $x->{$mod_strings['LBL_AN_MARKETING_STATUS']} = $row['campaignStatus'];
            $x->{$mod_strings['LBL_AN_MARKETING_TYPE']} = $row['campaignType'];
            $x->{$mod_strings['LBL_AN_MARKETING_BUDGET']} = $row['campaignBudget'];
            $x->{$mod_strings['LBL_AN_MARKETING_EXPECTED_COST']} = $row['campaignExpectedCost'];
            $x->{$mod_strings['LBL_AN_MARKETING_EXPECTED_REVENUE']} = $row['campaignExpectedRevenue'];
            $x->{$mod_strings['LBL_AN_MARKETING_OPPORTUNITY_NAME']} = $row['opportunityName'];
            $x->{$mod_strings['LBL_AN_MARKETING_OPPORTUNITY_AMOUNT']} = $row['opportunityAmount'];
            $x->{$mod_strings['LBL_AN_MARKETING_OPPORTUNITY_SALES_STAGE']} = $row['opportunitySalesStage'];
            $x->{$mod_strings['LBL_AN_MARKETING_OPPORTUNITY_ASSIGNED_TO']} = $row['assignedUser'];
            $x->{$mod_strings['LBL_AN_MARKETING_ACCOUNT_NAME']} = $row['accountsName'];

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
        $userId = $_SESSION['authenticated_user_id'];
        $fileLocation = $this->spotFilePath.$userId.'_'.$this->marketingActivitiesFileName;
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
        $returnArray = array();
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
        $aclWhereCampaigns = $this->buildSpotsAccessQuery($campaigns, $campaigns->table_name);

        $queryString = $query.$aclWhereCampaigns;
        $result = $db->query($queryString);

        while ($row = $db->fetchByAssoc($result)) {
            $x = new stdClass();
            $x->{$mod_strings['LBL_AN_MARKETINGACTIVITY_CAMPAIGN_NAME']} = $row['name'];
            $x->{$mod_strings['LBL_AN_MARKETINGACTIVITY_ACTIVITY_DATE']} = $row['activity_date'];
            $x->{$mod_strings['LBL_AN_MARKETINGACTIVITY_ACTIVITY_TYPE']} = $row['activity_type'];
            $x->{$mod_strings['LBL_AN_MARKETINGACTIVITY_RELATED_TYPE']} = $row['related_type'];
            $x->{$mod_strings['LBL_AN_MARKETINGACTIVITY_RELATED_ID']} = $row['related_id'];

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
        $userId = $_SESSION['authenticated_user_id'];
        $fileLocation = $this->spotFilePath.$userId.'_'.$this->quotesFileName;
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
        $returnArray = array();
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
            $x->{$mod_strings['LBL_AN_QUOTES_OPPORTUNITY_NAME']} = $row['opportunityName'];
            $x->{$mod_strings['LBL_AN_QUOTES_OPPORTUNITY_TYPE']} = $row['opportunityType'];
            $x->{$mod_strings['LBL_AN_QUOTES_OPPORTUNITY_LEAD_SOURCE']} = $row['opportunityLeadSource'];
            $x->{$mod_strings['LBL_AN_QUOTES_OPPORTUNITY_SALES_STAGE']} = $row['opportunitySalesStage'];
            $x->{$mod_strings['LBL_AN_QUOTES_ACCOUNT_NAME']} = $row['accountName'];
            $x->{$mod_strings['LBL_AN_QUOTES_CONTACT_NAME']} = $row['contactName'];
            $x->{$mod_strings['LBL_AN_QUOTES_ITEM_NAME']} = $row['productName'];
            $x->{$mod_strings['LBL_AN_QUOTES_ITEM_TYPE']} = $row['itemType'];
            $x->{$mod_strings['LBL_AN_QUOTES_ITEM_CATEGORY']} = $row['categoryName'];
            $x->{$mod_strings['LBL_AN_QUOTES_ITEM_QTY']} = $row['productQty'];
            $x->{$mod_strings['LBL_AN_QUOTES_ITEM_LIST_PRICE']} = $row['productListPrice'];
            $x->{$mod_strings['LBL_AN_QUOTES_ITEM_SALE_PRICE']} = $row['productPrice'];
            $x->{$mod_strings['LBL_AN_QUOTES_ITEM_COST_PRICE']} = $row['productCostPrice'];
            $x->{$mod_strings['LBL_AN_QUOTES_ITEM_DISCOUNT_PRICE']} = $row['productDiscount'];
            $x->{$mod_strings['LBL_AN_QUOTES_ITEM_DISCOUNT_AMOUNT']} = $row['discountAmount'];
            $x->{$mod_strings['LBL_AN_QUOTES_ITEM_TOTAL']} = $row['productTotal'];
            $x->{$mod_strings['LBL_AN_QUOTES_GRAND_TOTAL']} = $row['grandTotal'];
            $x->{$mod_strings['LBL_AN_QUOTES_ASSIGNED_TO']} = $row['assignedUser'];
            $x->{$mod_strings['LBL_AN_QUOTES_DATE_CREATED']} = $row['dateCreated'];
            $x->{$mod_strings['LBL_AN_QUOTES_DAY_CREATED']} = $row['dateCreatedDay'];
            $x->{$mod_strings['LBL_AN_QUOTES_WEEK_CREATED']} = $row['dateCreatedWeek'];
            $x->{$mod_strings['LBL_AN_QUOTES_MONTH_CREATED']} = $row['dateCreatedMonth'];
            $x->{$mod_strings['LBL_AN_QUOTES_QUARTER_CREATED']} = $row['dateCreatedQuarter'];
            $x->{$mod_strings['LBL_AN_QUOTES_YEAR_CREATED']} = $row['dateCreatedYear'];

            $returnArray[] = $x;
        }
        file_put_contents($filepath, json_encode($returnArray));
    }
}
