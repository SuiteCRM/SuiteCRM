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
class TemplateDate extends TemplateRange
{
    public $type = 'date';
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


    public function get_db_default($modify=false)
    {
        return '';
    }

    //BEGIN BACKWARDS COMPATABILITY
    public function get_xtpl_edit()
    {
        global $timedate;
        $name = $this->name;
        $returnXTPL = array();
        if (!empty($this->help)) {
            $returnXTPL[strtoupper($this->name . '_help')] = translate($this->help, $this->bean->module_dir);
        }
        $returnXTPL['USER_DATEFORMAT'] = $timedate->get_user_date_format();
        $returnXTPL['CALENDAR_DATEFORMAT'] = $timedate->get_cal_date_format();
        if (isset($this->bean->$name)) {
            $returnXTPL[strtoupper($this->name)] = $this->bean->$name;
        } else {
            if (empty($this->bean->id) && !empty($this->default_value) && !empty($this->dateStrings[$this->default_value])) {
                $returnXTPL[strtoupper($this->name)] = $timedate->asUserDate($timedate->getNow(true)->modify($this->dateStrings[$this->default_value]), false);
            }
        }
        return $returnXTPL;
    }

    public function get_field_def()
    {
        $def = parent::get_field_def();
        if (!empty($def['default'])) {
            $def['display_default'] = $def['default'];
            $def['default'] = '';
        }
        return $def;
    }
}
