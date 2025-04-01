<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
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
       'id'=>array('name' =>'id', 'type' =>'char', 'len'=>'36', 'default'=>'')
      , 'name'=>array('name' =>'name', 'type' =>'varchar', 'len'=>'255', )
      , 'date_entered'=>array('name' => 'date_entered','type' => 'datetime')
      , 'date_modified'=>array('name' => 'date_modified','type' => 'datetime')
      , 'modified_user_id'=>array('name' =>'modified_user_id', 'type' =>'char', 'len'=>'36',)
      , 'created_by'=>array('name' =>'created_by', 'type' =>'char', 'len'=>'36',)
      , 'description'=>array('name' =>'description', 'type' =>'text', 'len'=>'',)
      , 'deleted'=>array('name' =>'deleted', 'type' =>'bool', 'len'=>'1', 'default'=>'0', 'required'=>true)
      , 'securitygroup_id'=>array('name' =>'securitygroup_id', 'type' =>'char', 'len'=>'36',)
    );


    public function __construct()
    {
        parent::__construct();
    }





    public function get_list_view_data()
    {
        $data = parent::get_list_view_data();
        $delete = '';

        $group_owner = false;
        $securitygroup_name = "";
        if (empty($data['SECURITYGROUP_ID'])) {
            $securitygroup_name = "All";
        } else {
            require_once('modules/SecurityGroups/SecurityGroup.php');
            $securitygroup = BeanFactory::newBean('SecurityGroups');
            $securitygroup->retrieve($data['SECURITYGROUP_ID']);
            $securitygroup_name = $securitygroup->name;

            if ($securitygroup->assigned_user_id == $GLOBALS['current_user']->id) {
                $group_owner = true;
            }
        }

        if (is_admin($GLOBALS['current_user']) || $data['CREATED_BY'] == $GLOBALS['current_user']->id || $group_owner) {
            $delete = SugarThemeRegistry::current()->getImage('delete_inline', 'width="12" height="12" border="0" align="absmiddle" style="vertical-align: bottom;" onclick=\'Message.deleteMessage("'. $data['ID'] . '", "{this.id}")\'', null, null, '.gif', '');
        }

        $username = "";
        if (empty($data['CREATED_BY'])) {
            $username = "Unknown";
        } else {
            require_once('modules/Users/User.php');
            $user = BeanFactory::newBean('Users');
            $user->retrieve($data['CREATED_BY']);
            $username = $user->user_name;
        }

        $data['NAME'] = $data['DESCRIPTION'];
        $data['NAME'] =  '<div class="list view" style="padding:5px;border:none;">' . html_entity_decode((string) $data['NAME']);
        $data['NAME'] .= '<div class="byLineBox" style="padding-top: 2px"><span class="byLineLeft">'.$username.' ['.$securitygroup_name.']';
        $data['NAME'] .= '&nbsp;</span><span style="cursor: pointer;" class="byLineRight"> '.  $this->getTimeLapse($data['DATE_ENTERED']) . ' &nbsp;' .$delete. '</span></div>';
        return  $data ;
    }


    public static function saveMessage($text, $securitygroup_id)
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
        } // || !$feed->ACLAccess('save', true) )return;

        $text = strip_tags($text);
        $message->name = '';
        $message->description = $text;
        $message->securitygroup_id = $securitygroup_id;
        $message->save();
    }

    public function getTimeLapse($startDate)
    {
        $startDate = $GLOBALS['timedate']->to_db($startDate);
        $start = array();
        preg_match('/(\d+)\-(\d+)\-(\d+) (\d+)\:(\d+)\:(\d+)/', (string) $startDate, $start);
        $end = gmdate('Y-m-d H:i:s');
        $start_time = gmmktime($start[4], $start[5], $start[6], $start[2], $start[3], $start[1]);
        $seconds = time()- $start_time;
        $minutes =   $seconds/60;
        $seconds = $seconds % 60;
        $hours = floor($minutes / 60);
        $minutes = $minutes % 60;
        $days = floor($hours / 24);
        $hours = $hours % 24;
        $weeks = floor($days / 7);
        $days = $days % 7;
        $result = '';
        if ($weeks == 1) {
            $result = translate('LBL_TIME_LAST_WEEK', 'SugarFeed').' ';
            return $result;
        } else {
            if ($weeks > 1) {
                $result .= $weeks . ' '.translate('LBL_TIME_WEEKS', 'SugarFeed').' ';
                if ($days > 0) {
                    $result .= $days . ' '.translate('LBL_TIME_DAYS', 'SugarFeed').' ';
                }
            } else {
                if ($days == 1) {
                    $result = translate('LBL_TIME_YESTERDAY', 'SugarFeed').' ';
                    return $result;
                } else {
                    if ($days > 1) {
                        $result .= $days . ' '. translate('LBL_TIME_DAYS', 'SugarFeed').' ';
                    } else {
                        if ($hours == 1) {
                            $result .= $hours . ' '.translate('LBL_TIME_HOUR', 'SugarFeed').' ';
                        } else {
                            $result .= $hours . ' '.translate('LBL_TIME_HOURS', 'SugarFeed').' ';
                        }
                        if ($hours < 6) {
                            if ($minutes == 1) {
                                $result .= $minutes . ' ' . translate('LBL_TIME_MINUTE', 'SugarFeed'). ' ';
                            } else {
                                $result .= $minutes . ' ' . translate('LBL_TIME_MINUTES', 'SugarFeed'). ' ';
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

    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':return false;
        }
        return false;
    }
}
