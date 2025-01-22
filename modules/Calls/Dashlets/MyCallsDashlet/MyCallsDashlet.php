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




require_once('include/Dashlets/DashletGeneric.php');


#[\AllowDynamicProperties]
class MyCallsDashlet extends DashletGeneric
{
    public function __construct($id, $def = null)
    {
        global $current_user, $app_strings, $dashletData;

        $dashletData = $dashletData ?? [];

        require('modules/Calls/Dashlets/MyCallsDashlet/MyCallsDashlet.data.php');

        parent::__construct($id, $def);

        if (empty($def['title'])) {
            $this->title = translate('LBL_LIST_MY_CALLS', 'Calls');
        }
        $this->searchFields = $dashletData['MyCallsDashlet']['searchFields'];
        if (empty($def['filters'])) {
            if (isset($this->searchFields['status'])) {
                if (!empty($this->searchFields['status']['default'])) {
                    $this->filters['status'] = $this->searchFields['status']['default'];
                }
            }
        }
        $this->columns = $dashletData['MyCallsDashlet']['columns'];
        /*$this->columns['set_accept_links']= array('width'    => '10',
                                              'label'    => translate('LBL_ACCEPT_THIS', 'Meetings'),
                                              'sortable' => false,
                                              'related_fields' => array('status'),
                                              'default' => 'true');*/
        $this->seedBean = BeanFactory::newBean('Calls');
    }



    public function process($lvsParams = array(), $id = null)
    {
        global $current_language, $app_list_strings, $current_user;
        $mod_strings = return_module_language($current_language, 'Calls');

        // handle myitems only differently --  set the custom query to show assigned meetings and invitee meetings
        if ($this->myItemsOnly) {
            //join with meeting_users table to process related users
            $this->seedBean->listview_inner_join = array('LEFT JOIN  calls_users c_u on  c_u.call_id = calls.id');

            //set the custom query to include assigned meetings
            $lvsParams['custom_where'] = ' AND (calls.assigned_user_id = \'' . $current_user->id . '\' OR (c_u.user_id = \'' . $current_user->id . '\' AND c_u.deleted = 0  )) ';
        }

        $this->myItemsOnly = false;
        //query needs to be distinct to avoid multiple records being returned for the same meeting (one for each invited user),
        //so we need to make sure date entered is also set so the sort can work with the group by
        $lvsParams['custom_select']=', calls.date_entered ';
        $lvsParams['distinct']=true;

        parent::process($lvsParams);

        $keys = array();
        foreach ($this->lvs->data['data'] as $num => $row) {
            $keys[] = $row['ID'];
        }


        if (!empty($keys)) {
            $query = "SELECT call_id, accept_status FROM calls_users WHERE deleted = 0 and user_id = '" . $current_user->id . "' AND call_id IN ('" . implode("','", $keys). "')";
            $result = DBManagerFactory::getInstance()->query($query);

            while ($row = DBManagerFactory::getInstance()->fetchByAssoc($result)) {
                $rowNums = $this->lvs->data['pageData']['idIndex'][$row['call_id']]; // figure out which rows have this guid
                foreach ($rowNums as $rowNum) {
                    $this->lvs->data['data'][$rowNum]['ACCEPT_STATUS'] = $row['accept_status'];
                }
            }
        }

        foreach ($this->lvs->data['data'] as $rowNum => $row) {
            if (empty($this->lvs->data['data'][$rowNum]['DURATION_HOURS'])) {
                $this->lvs->data['data'][$rowNum]['DURATION'] = '0' . $mod_strings['LBL_HOURS_ABBREV'];
            } else {
                $this->lvs->data['data'][$rowNum]['DURATION'] = $this->lvs->data['data'][$rowNum]['DURATION_HOURS'] . $mod_strings['LBL_HOURS_ABBREV'];
            }

            if (empty($this->lvs->data['data'][$rowNum]['DURATION_MINUTES']) || empty($this->seedBean->minutes_values[$this->lvs->data['data'][$rowNum]['DURATION_MINUTES']])) {
                $this->lvs->data['data'][$rowNum]['DURATION'] .= '00';
            } else {
                $this->lvs->data['data'][$rowNum]['DURATION'] .= $this->seedBean->minutes_values[$this->lvs->data['data'][$rowNum]['DURATION_MINUTES']];
            }
            if ($this->lvs->data['data'][$rowNum]['STATUS'] == $app_list_strings['meeting_status_dom']['Planned']) {
                if ($this->lvs->data['data'][$rowNum]['ACCEPT_STATUS'] == '') {
                    //if no status has been set, then do not show accept options
                    $this->lvs->data['data'][$rowNum]['SET_ACCEPT_LINKS'] = "<div id=\"accept".$this->id."\" ></div>";
                } elseif ($this->lvs->data['data'][$rowNum]['ACCEPT_STATUS'] == 'none') {
                    $this->lvs->data['data'][$rowNum]['SET_ACCEPT_LINKS'] = "<div id=\"accept".$this->id."\"><a title=\"".$app_list_strings['dom_meeting_accept_options']['accept'].
                        "\" href=\"javascript:SUGAR.util.retrieveAndFill('index.php?module=Activities&to_pdf=1&action=SetAcceptStatus&id=".$this->id."&object_type=Call&object_id=".$this->lvs->data['data'][$rowNum]['ID'] . "&accept_status=accept', null, null, SUGAR.mySugar.retrieveDashlet, '{$this->id}');\">".
                        SugarThemeRegistry::current()->getImage("accept_inline", "border='0'", null, null, '.gif', $app_list_strings['dom_meeting_accept_options']['accept']). "</a>&nbsp;<a title=\"".$app_list_strings['dom_meeting_accept_options']['tentative']."\" href=\"javascript:SUGAR.util.retrieveAndFill('index.php?module=Activities&to_pdf=1&action=SetAcceptStatus&id=".$this->id."&object_type=Call&object_id=".$this->lvs->data['data'][$rowNum]['ID'] . "&accept_status=tentative', null, null, SUGAR.mySugar.retrieveDashlet, '{$this->id}');\">".
                        SugarThemeRegistry::current()->getImage("tentative_inline", "border='0'", null, null, '.gif', $app_list_strings['dom_meeting_accept_options']['tentative'])."</a>&nbsp;<a title=\"".$app_list_strings['dom_meeting_accept_options']['decline']. "\" href=\"javascript:SUGAR.util.retrieveAndFill('index.php?module=Activities&to_pdf=1&action=SetAcceptStatus&id=".$this->id."&object_type=Call&object_id=".$this->lvs->data['data'][$rowNum]['ID'] . "&accept_status=decline', null, null, SUGAR.mySugar.retrieveDashlet, '{$this->id}');\">".
                        SugarThemeRegistry::current()->getImage("decline_inline", "border='0'", null, null, '.gif', $app_list_strings['dom_meeting_accept_options']['decline'])."</a></div>";
                } else {
                    $this->lvs->data['data'][$rowNum]['SET_ACCEPT_LINKS'] = $app_list_strings['dom_meeting_accept_status'][$this->lvs->data['data'][$rowNum]['ACCEPT_STATUS']];
                }
            }

            $this->lvs->data['data'][$rowNum]['DURATION'] .= $mod_strings['LBL_MINSS_ABBREV'];
        }
        $this->displayColumns[]= "set_accept_links";
    }

    public function displayOptions()
    {
        $this->processDisplayOptions();
        $this->configureSS->assign('strings', array('general' => $GLOBALS['mod_strings']['LBL_DASHLET_CONFIGURE_GENERAL'],
                                     'filters' => $GLOBALS['mod_strings']['LBL_DASHLET_CONFIGURE_FILTERS'],
                                     'myItems' => translate('LBL_DASHLET_CONFIGURE_MY_ITEMS_ONLY', 'Calls'),
                                     'displayRows' => $GLOBALS['mod_strings']['LBL_DASHLET_CONFIGURE_DISPLAY_ROWS'],
                                     'title' => $GLOBALS['mod_strings']['LBL_DASHLET_CONFIGURE_TITLE'],
                                     'clear' => $GLOBALS['app_strings']['LBL_CLEAR_BUTTON_LABEL'],
                                     'save' => $GLOBALS['app_strings']['LBL_SAVE_BUTTON_LABEL'],
                                     'autoRefresh' => $GLOBALS['app_strings']['LBL_DASHLET_CONFIGURE_AUTOREFRESH'],
                                     ));

        return $this->configureSS->fetch($this->configureTpl);
    }
}
