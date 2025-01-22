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

 * Description:
 */

#[\AllowDynamicProperties]
class Campaign extends SugarBean
{
    public $field_name_map;

    // Stored fields
    public $id;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $assigned_user_id;
    public $created_by;
    public $created_by_name;
    public $currency_id;
    public $modified_by_name;
    public $name;
    public $start_date;
    public $end_date;
    public $status;
    public $expected_cost;
    public $budget;
    public $actual_cost;
    public $expected_revenue;
    public $campaign_type;
    public $objective;
    public $content;
    public $tracker_key;
    public $tracker_text;
    public $tracker_count;
    public $refer_url;
    public $impressions;

    // These are related
    public $assigned_user_name;
    public $note_id;

    // module name definitions and table relations
    public $table_name = "campaigns";
    public $rel_prospect_list_table = "prospect_list_campaigns";
    public $object_name = "Campaign";
    public $module_dir = 'Campaigns';
    public $importable = true;

    // This is used to retrieve related fields from form posts.
    public $additional_column_fields = [
        'assigned_user_name',
        'assigned_user_id',
        'note_id',
    ];

    public $relationship_fields = array('prospect_list_id' => 'prospect_lists', 'note_id' => 'notes',);

    public $new_schema = true;

    /**
     *
     * @var string
     */
    public $survey_id;

    /**
     *
     * @var string
     */
    public $survey_name;

    public function list_view_parse_additional_sections(&$listTmpl)
    {
        global $locale;

        // take $assigned_user_id and get the Username value to assign
        $assId = $this->getFieldValue('assigned_user_id');

        $query = "SELECT first_name, last_name FROM users WHERE id = '".$assId."'";
        $result = $this->db->query($query);
        $user = $this->db->fetchByAssoc($result);

        if (!empty($user)) {
            if (is_array($user)) {
                $fullName = $locale->getLocaleFormattedName($user['first_name'], $user['last_name']);
            } else /*if(is_object($user))*/ {
                $fullName = $locale->getLocaleFormattedName($user->first_name, $user->last_name);
            }
            $listTmpl->assign('ASSIGNED_USER_NAME', $fullName);
        }
    }


    public function get_summary_text()
    {
        return $this->name;
    }

    public function create_export_query($order_by, $where, $relate_link_join='')
    {
        $custom_join = $this->getCustomJoin(true, true, $where);
        $custom_join['join'] .= $relate_link_join;
        $query = "SELECT
            campaigns.*,
            users.user_name as assigned_user_name ";
        $query .=  $custom_join['select'];
        $query .= " FROM campaigns ";
        $query .= "LEFT JOIN users
                      ON campaigns.assigned_user_id=users.id";
        $query .=  $custom_join['join'];

        $where_auto = " campaigns.deleted=0";

        if ($where != "") {
            $query .= " where $where AND ".$where_auto;
        } else {
            $query .= " where ".$where_auto;
        }

        if ($order_by != "") {
            $query .= " ORDER BY $order_by";
        } else {
            $query .= " ORDER BY campaigns.name";
        }
        return $query;
    }



    public function clear_campaign_prospect_list_relationship($campaign_id, $prospect_list_id='')
    {
        if (!empty($prospect_list_id)) {
            $prospect_clause = " and prospect_list_id = '$prospect_list_id' ";
        } else {
            $prospect_clause = '';
        }

        $query = "DELETE FROM $this->rel_prospect_list_table WHERE campaign_id='$campaign_id' AND deleted = '0' " . $prospect_clause;
        $this->db->query($query, true, "Error clearing campaign to prospect_list relationship: ");
    }



    public function mark_relationships_deleted($id)
    {
        $this->clear_campaign_prospect_list_relationship($id);
    }

    public function fill_in_additional_list_fields()
    {
        parent::fill_in_additional_list_fields();
    }

    public function fill_in_additional_detail_fields()
    {
        parent::fill_in_additional_detail_fields();
        //format numbers.

        //don't need additional formatting here.
        //$this->budget=format_number($this->budget);
        //$this->expected_cost=format_number($this->expected_cost);
        //$this->actual_cost=format_number($this->actual_cost);
        //$this->expected_revenue=format_number($this->expected_revenue);
    }


    public function update_currency_id($fromid, $toid)
    {
    }


    public function get_list_view_data()
    {
        $temp_array = $this->get_list_view_array();
        if ($this->campaign_type != 'Email') {
            $temp_array['OPTIONAL_LINK']="display:none";
        }
        $temp_array['TRACK_CAMPAIGN_TITLE'] = translate("LBL_TRACK_BUTTON_TITLE", 'Campaigns');
        $temp_array['TRACK_CAMPAIGN_IMAGE'] = SugarThemeRegistry::current()->getImageURL('view_status.gif');
        $temp_array['LAUNCH_WIZARD_TITLE'] = translate("LBL_TO_WIZARD_TITLE", 'Campaigns');
        $temp_array['LAUNCH_WIZARD_IMAGE'] = SugarThemeRegistry::current()->getImageURL('edit_wizard.gif');
        $temp_array['TRACK_VIEW_ALT_TEXT'] = translate("LBL_TRACK_BUTTON_TITLE", 'Campaigns');
        $temp_array['LAUNCH_WIZ_ALT_TEXT'] = translate("LBL_TO_WIZARD_TITLE", 'Campaigns');

        return $temp_array;
    }
    /**
    	builds a generic search based on the query string using or
    	do not include any $this-> because this is called on without having the class instantiated
    */
    public function build_generic_where_clause($the_query_string)
    {
        $where_clauses = array();
        $the_query_string = $this->db->quote($the_query_string);
        array_push($where_clauses, "campaigns.name like '$the_query_string%'");

        $the_where = "";
        foreach ($where_clauses as $clause) {
            if ($the_where != "") {
                $the_where .= " or ";
            }
            $the_where .= $clause;
        }


        return $the_where;
    }

    public function save($check_notify = false)
    {

            //US DOLLAR
        if (isset($this->amount) && !empty($this->amount)) {
            $currency = BeanFactory::newBean('Currencies');
            $currency->retrieve($this->currency_id);
            $this->amount_usdollar = $currency->convertToDollar($this->amount);
        }


        // Bug53301
        if ($this->campaign_type != 'NewsLetter') {
            $this->frequency = '';
        }

		return parent::save($check_notify);

	}


    public function mark_deleted($id)
    {
        $query = "update contacts set campaign_id = null where campaign_id = '{$id}' ";
        $this->db->query($query);
        $query = "update accounts set campaign_id = null where campaign_id = '{$id}' ";
        $this->db->query($query);
        // bug49632 - delete campaign logs for the campaign as well
        $query = "update campaign_log set deleted = 1 where campaign_id = '{$id}' ";
        $this->db->query($query);
        return parent::mark_deleted($id);
    }

    public function set_notification_body($xtpl, $camp)
    {
        $xtpl->assign("CAMPAIGN_NAME", $camp->name);
        $xtpl->assign("CAMPAIGN_AMOUNT", $camp->budget);
        $xtpl->assign("CAMPAIGN_CLOSEDATE", $camp->end_date);
        $xtpl->assign("CAMPAIGN_STATUS", $camp->status);
        $xtpl->assign("CAMPAIGN_DESCRIPTION", nl2br($camp->content));

        return $xtpl;
    }

    public function track_log_leads()
    {
        $this->load_relationship('log_entries');
        $query_array = $this->log_entries->getQuery(true);

        $query_array['select'] = 'SELECT campaign_log.* ';
        $query_array['where']  = $query_array['where']. " AND activity_type = 'lead' AND archived = 0 AND target_id IS NOT NULL";

        return implode(' ', $query_array);
    }

    public function track_log_entries($type=array())
    {
        //get arguments being passed in
        $args = func_get_args();
        $mkt_id ='';

        $this->load_relationship('log_entries');
        $query_array = $this->log_entries->getQuery(true);

        //if one of the arguments is marketing ID, then we need to filter by it
        foreach ($args as $arg) {
            if (isset($arg['EMAIL_MARKETING_ID_VALUE'])) {
                $mkt_id = $arg['EMAIL_MARKETING_ID_VALUE'];
            }

            if (isset($arg['group_by'])) {
                $query_array['group_by'] = $arg['group_by'];
            }
        }



        if (empty($type)) {
            $type[0]='targeted';
        }

        $query_array['select'] ="SELECT campaign_log.* ";
        $query_array['where'] = $query_array['where']. " AND activity_type='{$type[0]}' AND archived=0";
        //add filtering by marketing id, if it exists
        if (!empty($mkt_id)) {
            $query_array['where'] = $query_array['where']. " AND marketing_id ='$mkt_id' ";
        }

        //B.F. #37943
        if (isset($query_array['group_by'])) {
            //perform the inner join with the group by if a marketing id is defined, which means we need to filter out duplicates.
            //if no marketing id is specified then we are displaying results from multiple marketing emails and it is understood there might be duplicate target entries
            if (!empty($mkt_id)) {
                $group_by = str_replace("campaign_log", "cl", (string) $query_array['group_by']);
                $join_where = str_replace("campaign_log", "cl", $query_array['where']);
                $query_array['from'] .= " INNER JOIN (select min(id) as id from campaign_log cl $join_where GROUP BY $group_by  ) secondary
					on campaign_log.id = secondary.id	";
            }
            unset($query_array['group_by']);
        } else {
            if (isset($query_array['group_by'])) {
                $query_array['where'] = $query_array['where'] . ' GROUP BY ' . $query_array['group_by'];
                unset($query_array['group_by']);
            }
        }

        $query = (implode(" ", $query_array));
        return $query;
    }


    public function get_queue_items(...$args)
    {
        $mkt_id ='';

        $this->load_relationship('queueitems');
        $query_array = $this->queueitems->getQuery(true);

        //if one of the arguments is marketing ID, then we need to filter by it
        foreach ($args as $arg) {
            if (isset($arg['EMAIL_MARKETING_ID_VALUE'])) {
                $mkt_id = $arg['EMAIL_MARKETING_ID_VALUE'];
            }

            if (isset($arg['group_by'])) {
                $query_array['group_by'] = $arg['group_by'];
            }
        }

        //add filtering by marketing id, if it exists, and if where key is not empty
        if (!empty($mkt_id) && !empty($query_array['where'])) {
            $query_array['where'] = $query_array['where']. " AND marketing_id ='$mkt_id' ";
        }

        //get select query from email man
        $man = BeanFactory::newBean('EmailMan');
        $listquery= $man->create_queue_items_query('', str_replace(array("WHERE","where"), "", (string) $query_array['where']), null, $query_array);
        return $listquery;
    }
    //	function get_prospect_list_entries() {
    //		$this->load_relationship('prospectlists');
    //		$query_array = $this->prospectlists->getQuery(true);
//
    //		$query=<<<EOQ
    //			SELECT distinct prospect_lists.*,
    //			(case  when (email_marketing.id is null) then default_message.id else email_marketing.id end) marketing_id,
    //			(case  when  (email_marketing.id is null) then default_message.name else email_marketing.name end) marketing_name
//
    //			FROM prospect_lists
//
    //			INNER JOIN prospect_list_campaigns ON (prospect_lists.id=prospect_list_campaigns.prospect_list_id AND prospect_list_campaigns.campaign_id='{$this->id}')
//
    //			LEFT JOIN email_marketing on email_marketing.message_for = prospect_lists.id and email_marketing.campaign_id = '{$this->id}'
    //			and email_marketing.deleted =0 and email_marketing.status='active'
//
    //			LEFT JOIN email_marketing default_message on default_message.message_for = prospect_list_campaigns.campaign_id and
    //			default_message.campaign_id = '{$this->id}' and default_message.deleted =0
    //			and default_message.status='active'
//
    //			WHERE prospect_list_campaigns.deleted=0 AND prospect_lists.deleted=0
//
    //EOQ;
    //		return $query;
    //	}

    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':return true;
        }
        return false;
    }


    /**
     * create_list_count_query
     * Overrode this method from SugarBean to handle the distinct parameter used to filter out
     * duplicate entries for some of the subpanel listivews.  Without the distinct filter, the
     * list count would be inaccurate because one-to-many email_marketing entries may be associated
     * with a campaign.
     *
     * @param string $query Select query string
     * @param array $param array of arguments
     * @return string count query
     *
     */
    public function create_list_count_query($query, $params=array())
    {
        //include the distinct filter if a marketing id is defined, which means we need to filter out duplicates by the passed in group by.
        //if no marketing id is specified, it is understood there might be duplicate target entries so no need to filter out
        if ((strpos($query, 'marketing_id') !== false)&& isset($params['distinct'])) {
            $pattern = '/SELECT(.*?)(\s){1}FROM(\s){1}/is';  // ignores the case
            $replacement = 'SELECT COUNT(DISTINCT ' . $params['distinct'] . ') c FROM ';
            $query = preg_replace($pattern, $replacement, $query, 1);
            return $query;
        }

        //If distinct parameter not found, default to SugarBean's function
        return parent::create_list_count_query($query);
    }

    /**
     * Returns count of deleted leads,
     * which were created through generated lead form
     *
     * @return integer
     */
    public function getDeletedCampaignLogLeadsCount()
    {
        $query = "SELECT COUNT(*) AS count FROM campaign_log WHERE campaign_id = '" . $this->getFieldValue('id') . "' AND target_id IS NULL AND activity_type = 'lead'";
        $result = $this->db->fetchOne($query);

        return (int)$result['count'];
    }
}
