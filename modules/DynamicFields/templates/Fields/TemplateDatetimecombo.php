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


require_once('modules/DynamicFields/templates/Fields/TemplateRange.php');

#[\AllowDynamicProperties]
class TemplateDatetimecombo extends TemplateRange
{
    public $type = 'datetimecombo';
    public $len = '';
    public $dateStrings;

    public function __construct()
    {
        parent::__construct();
        global $app_strings;
        $this->dateStrings = array(
            $app_strings['LBL_NONE']=>'',
            $app_strings['LBL_YESTERDAY']=> '-1 day',
            $app_strings['LBL_TODAY']=>'now',
            $app_strings['LBL_TOMORROW']=>'+1 day',
            $app_strings['LBL_NEXT_WEEK']=> '+1 week',
            $app_strings['LBL_NEXT_MONDAY']=>'next monday',
            $app_strings['LBL_NEXT_FRIDAY']=>'next friday',
            $app_strings['LBL_TWO_WEEKS']=> '+2 weeks',
            $app_strings['LBL_NEXT_MONTH']=> '+1 month',
            $app_strings['LBL_FIRST_DAY_OF_NEXT_MONTH']=> 'first day of next month', // must handle this non-GNU date string in SugarBean->populateDefaultValues; if we don't this will evaluate to 1969...
            $app_strings['LBL_THREE_MONTHS']=> '+3 months',  //kbrill Bug #17023
            $app_strings['LBL_SIXMONTHS']=> '+6 months',
            $app_strings['LBL_NEXT_YEAR']=> '+1 year',
        );
    }
    
    public $hoursStrings = array(
        '' => '',
        '01' => '01',
        '02' => '02',
        '03' => '03',
        '04' => '04',
        '05' => '05',
        '06' => '06',
        '07' => '07',
        '08' => '08',
        '09' => '09',
        '10' => '10',
        '11' => '11',
        '12' => '12',
    );
    
    public $hoursStrings24 = array(
        '' => '',
        '00' => '00',
        '01' => '01',
        '02' => '02',
        '03' => '03',
        '04' => '04',
        '05' => '05',
        '06' => '06',
        '07' => '07',
        '08' => '08',
        '09' => '09',
        '10' => '10',
        '11' => '11',
        '12' => '12',
        '13' => '13',
        '14' => '14',
        '15' => '15',
        '16' => '16',
        '17' => '17',
        '18' => '18',
        '19' => '19',
        '20' => '20',
        '21' => '21',
        '22' => '22',
        '23' => '23',
    );
    
    public $minutesStrings = array(
        '' => '',
        '00' => '00',
        '15' => '15',
        '30' => '30',
        '45' => '45',
    );
    
    public $meridiemStrings = array(
        '' => '',
        'am' => 'am',
        'pm' => 'pm',
    );

    public function get_db_default($modify=false)
    {
        return '';
    }

    public function get_field_def()
    {
        $def = parent::get_field_def();
        $def['dbType'] = 'datetime';
        if (!empty($def['default'])) {
            $def['display_default'] = $def['default'];
            $def['default'] = '';
        }
        return $def;
    }
    
    public function populateFromPost()
    {
        parent::populateFromPost();
        if (!empty($_REQUEST['defaultDate']) && !empty($_REQUEST['defaultTime'])) {
            $_REQUEST['default'] = $_REQUEST['defaultDate'].'&'.$_REQUEST['defaultTime'];

            $defaultTime = $_REQUEST['defaultTime'];
            $hours = substr((string) $defaultTime, 0, 2);
            $minutes = substr((string) $defaultTime, 3, 2);
            $meridiem = substr((string) $defaultTime, 5, 2);
            if (empty($meridiem)) {
                if ($hours == '00') {
                    $hours = 12;
                    $meridiem = 'am';
                } else {
                    if ($hours >= 12) {
                        //lets add the PM meridiem, but only subtract 12 if hours is greater than 12
                        if ($hours > 12) {
                            $hours -= 12;
                        }
                        $meridiem = 'pm';
                    } else {
                        $meridiem = 'am';
                    }
                }
                //lets format the string to make sure the leading 0's are added back in for hours and minutes
                $_REQUEST['default'] = $_REQUEST['defaultDate'] . '&' . sprintf('%02d:%02d%s', $hours, $minutes, $meridiem);
            }
        } else {
            $_REQUEST['default'] = '';
        }
        unset($_REQUEST['defaultDate']);
        unset($_REQUEST['defaultTime']);
        
        foreach ($this->vardef_map as $vardef=>$field) {
            if (isset($_REQUEST[$vardef])) {
                //  Bug #48826. Some fields are allowed to have special characters and must be decoded from the request
                // Bug 49774, 49775: Strip html tags from 'formula' and 'dependency'.
                if (is_string($_REQUEST[$vardef]) && in_array($vardef, $this->decode_from_request_fields_map)) {
                    $this->$vardef = html_entity_decode(strip_tags(from_html($_REQUEST[$vardef])));
                } else {
                    $this->$vardef = $_REQUEST[$vardef];
                }

                if ($vardef != $field) {
                    $this->$field = $this->$vardef;
                }
            }
        }
        $GLOBALS['log']->debug('populate: '.print_r($this, true));
    }
}
