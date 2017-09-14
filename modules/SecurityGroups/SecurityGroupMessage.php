<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2017 SalesAgility Ltd.
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

class SecurityGroupMessage extends Basic
{
    public $new_schema = true;
    public $module_dir = 'SecurityGroups';
    public $object_name = 'SecurityGroupMessage';
    public $table_name = 'securitygroups_message';
    public $importable = false;

    public $id;
    public $name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $modified_by_name;
    public $created_by;
    public $created_by_name;
    public $description;
    public $deleted;
    public $created_by_link;
    public $modified_user_link;


    public $additional_column_fields = array();
    public $field_defs = array(
        'id' => array('name' => 'id', 'type' => 'char', 'len' => '36', 'default' => '')
    ,
        'name' => array('name' => 'name', 'type' => 'varchar', 'len' => '255',)
    ,
        'date_entered' => array('name' => 'date_entered', 'type' => 'datetime')
    ,
        'date_modified' => array('name' => 'date_modified', 'type' => 'datetime')
    ,
        'modified_user_id' => array('name' => 'modified_user_id', 'type' => 'char', 'len' => '36',)
    ,
        'created_by' => array('name' => 'created_by', 'type' => 'char', 'len' => '36',)
    ,
        'description' => array('name' => 'description', 'type' => 'text', 'len' => '',)
    ,
        'deleted' => array('name' => 'deleted', 'type' => 'bool', 'len' => '1', 'default' => '0', 'required' => true)
    ,
        'securitygroup_id' => array('name' => 'securitygroup_id', 'type' => 'char', 'len' => '36',)
    );

    static function saveMessage($text, $securitygroup_id)
    {
        //if no security group id then must be admin. Otherwise, make sure the user is a member of the group
        global $current_user;
        if (empty($securitygroup_id) && !is_admin($current_user)) {
            return;
        } else {
            if (empty($securitygroup_id)) {
                $securitygroup_id = null; //6.4.0
            }
        }
        $message = new SecurityGroupMessage();
        if (empty($text)) {
            return;
        }
        // || !$feed->ACLAccess('save', true) )return;

        $text = strip_tags($text);
        $message->name = '';
        $message->description = $text;
        $message->securitygroup_id = $securitygroup_id;
        $message->save();
    }

    /**
     * @deprecated deprecated since version 7.6, PHP4 Style Constructors are deprecated and will be remove in 7.8, please update your code, use __construct instead
     */
    function SecurityGroupMessage()
    {
        $deprecatedMessage = 'PHP4 Style Constructors are deprecated and will be remove in 7.8, please update your code';
        if (isset($GLOBALS['log'])) {
            $GLOBALS['log']->deprecated($deprecatedMessage);
        } else {
            trigger_error($deprecatedMessage, E_USER_DEPRECATED);
        }
        self::__construct();
    }

    function __construct()
    {
        parent::__construct();
    }

    function get_list_view_data()
    {
        $data = parent::get_list_view_data();
        $delete = '';

        $group_owner = false;
        $securitygroup_name = "";
        if (empty($data['SECURITYGROUP_ID'])) {
            $securitygroup_name = "All";
        } else {
            require_once('modules/SecurityGroups/SecurityGroup.php');
            $securitygroup = new SecurityGroup();
            $securitygroup->retrieve($data['SECURITYGROUP_ID']);
            $securitygroup_name = $securitygroup->name;

            if ($securitygroup->assigned_user_id == $GLOBALS['current_user']->id) {
                $group_owner = true;
            }
        }

        if (is_admin($GLOBALS['current_user']) || $data['CREATED_BY'] == $GLOBALS['current_user']->id || $group_owner) {
            $delete = SugarThemeRegistry::current()->getImage('delete_inline',
                'width="12" height="12" border="0" align="absmiddle" style="vertical-align: bottom;" onclick=\'Message.deleteMessage("' . $data['ID'] . '", "{this.id}")\'',
                null, null, '.gif', '');
        }

        $username = "";
        if (empty($data['CREATED_BY'])) {
            $username = "Unknown";
        } else {
            require_once('modules/Users/User.php');
            $user = new User();
            $user->retrieve($data['CREATED_BY']);
            $username = $user->user_name;
        }

        $data['NAME'] = $data['DESCRIPTION'];
        $data['NAME'] = '<div class="list view" style="padding:5px;border:none;">' . html_entity_decode($data['NAME']);
        $data['NAME'] .= '<div class="byLineBox" style="padding-top: 2px"><span class="byLineLeft">' . $username . ' [' . $securitygroup_name . ']';
        $data['NAME'] .= '&nbsp;</span><span style="cursor: pointer;" class="byLineRight"> ' . $this->getTimeLapse($data['DATE_ENTERED']) . ' &nbsp;' . $delete . '</span></div>';

        return $data;
    }

    function getTimeLapse($startDate)
    {
        $startDate = $GLOBALS['timedate']->to_db($startDate);
        $start = array();
        preg_match('/(\d+)\-(\d+)\-(\d+) (\d+)\:(\d+)\:(\d+)/', $startDate, $start);
        $end = gmdate('Y-m-d H:i:s');
        $start_time = gmmktime($start[4], $start[5], $start[6], $start[2], $start[3], $start[1]);
        $seconds = time() - $start_time;
        $minutes = $seconds / 60;
        $seconds = $seconds % 60;
        $hours = floor($minutes / 60);
        $minutes = $minutes % 60;
        $days = floor($hours / 24);
        $hours = $hours % 24;
        $weeks = floor($days / 7);
        $days = $days % 7;
        $result = '';
        if ($weeks == 1) {
            $result = translate('LBL_TIME_LAST_WEEK', 'SugarFeed') . ' ';

            return $result;
        } else {
            if ($weeks > 1) {
                $result .= $weeks . ' ' . translate('LBL_TIME_WEEKS', 'SugarFeed') . ' ';
                if ($days > 0) {
                    $result .= $days . ' ' . translate('LBL_TIME_DAYS', 'SugarFeed') . ' ';
                }
            } else {
                if ($days == 1) {
                    $result = translate('LBL_TIME_YESTERDAY', 'SugarFeed') . ' ';

                    return $result;
                } else {
                    if ($days > 1) {
                        $result .= $days . ' ' . translate('LBL_TIME_DAYS', 'SugarFeed') . ' ';
                    } else {
                        if ($hours == 1) {
                            $result .= $hours . ' ' . translate('LBL_TIME_HOUR', 'SugarFeed') . ' ';
                        } else {
                            $result .= $hours . ' ' . translate('LBL_TIME_HOURS', 'SugarFeed') . ' ';
                        }
                        if ($hours < 6) {
                            if ($minutes == 1) {
                                $result .= $minutes . ' ' . translate('LBL_TIME_MINUTE', 'SugarFeed') . ' ';
                            } else {
                                $result .= $minutes . ' ' . translate('LBL_TIME_MINUTES', 'SugarFeed') . ' ';
                            }
                        }
                        if ($hours == 0 && $minutes == 0) {
                            if ($seconds == 1) {
                                $result = $seconds . ' ' . translate('LBL_TIME_SECOND', 'SugarFeed');
                            } else {
                                $result = $seconds . ' ' . translate('LBL_TIME_SECONDS', 'SugarFeed');
                            }
                        }
                    }
                }
            }
        }

        return $result . ' ' . translate('LBL_TIME_AGO', 'SugarFeed');


    }

    function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return false;
        }

        return false;
    }

}
