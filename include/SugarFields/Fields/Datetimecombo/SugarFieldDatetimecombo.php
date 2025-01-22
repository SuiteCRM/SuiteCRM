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

require_once('include/SugarFields/Fields/Base/SugarFieldBase.php');
#[\AllowDynamicProperties]
class SugarFieldDatetimecombo extends SugarFieldBase
{
    public function getEditViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex)
    {
        // Create Smarty variables for the Calendar picker widget
        if (!isset($displayParams['showMinutesDropdown'])) {
            $displayParams['showMinutesDropdown'] = false;
        }

        if (!isset($displayParams['showHoursDropdown'])) {
            $displayParams['showHoursDropdown'] = false;
        }

        if (!isset($displayParams['showNoneCheckbox'])) {
            $displayParams['showNoneCheckbox'] = false;
        }

        if (!isset($displayParams['showFormats'])) {
            $displayParams['showFormats'] = false;
        }

        global $timedate;
        $displayParams['dateFormat'] = $timedate->get_cal_date_format();

        $displayParams['timeFormat'] = $timedate->get_user_time_format();
        $this->setup($parentFieldArray, $vardef, $displayParams, $tabindex);
        return $this->fetch($this->findTemplate('EditView'));
    }

    public function getImportViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex)
    {
        $displayParams['showFormats'] = true;
        return $this->getEditViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex);
    }
    
    public function getSearchViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex)
    {
        if ($this->isRangeSearchView($vardef)) {
            $displayParams['showMinutesDropdown'] = false;
            $displayParams['showHoursDropdown'] = false;
            $displayParams['showNoneCheckbox'] = false;
            $displayParams['showFormats'] = false;
            global $timedate, $current_language;
            $displayParams['dateFormat'] = $timedate->get_cal_date_format();
            $displayParams['timeFormat'] = $timedate->get_user_time_format();

            $this->setup($parentFieldArray, $vardef, $displayParams, $tabindex);
            $id = isset($displayParams['idName']) ? $displayParams['idName'] : $vardef['name'];
            $this->ss->assign('original_id', (string)($id));
            $this->ss->assign('id_range', "range_{$id}");
            $this->ss->assign('id_range_start', "start_range_{$id}");
            $this->ss->assign('id_range_end', "end_range_{$id}");
            $this->ss->assign('id_range_choice', "{$id}_range_choice");
            if (file_exists('custom/include/SugarFields/Fields/Datetimecombo/RangeSearchForm.tpl')) {
                return $this->fetch('custom/include/SugarFields/Fields/Datetimecombo/RangeSearchForm.tpl');
            }
            return $this->fetch('include/SugarFields/Fields/Datetimecombo/RangeSearchForm.tpl');
        }

        // Create Smarty variables for the Calendar picker widget
        if (!isset($displayParams['showMinutesDropdown'])) {
            $displayParams['showMinutesDropdown'] = false;
        }

        if (!isset($displayParams['showHoursDropdown'])) {
            $displayParams['showHoursDropdown'] = false;
        }

        if (!isset($displayParams['showNoneCheckbox'])) {
            $displayParams['showNoneCheckbox'] = false;
        }

        if (!isset($displayParams['showFormats'])) {
            $displayParams['showFormats'] = false;
        }

        global $timedate;
        $displayParams['dateFormat'] = $timedate->get_cal_date_format();

        $displayParams['timeFormat'] = $timedate->get_user_time_format();
        $this->setup($parentFieldArray, $vardef, $displayParams, $tabindex);
        return $this->fetch($this->findTemplate('SearchView'));
    }


    public function getEmailTemplateValue($inputField, $vardef, $context = null, $tabindex = 0)
    {
        // This does not return a smarty section, instead it returns a direct value
        if (isset($context['notify_user'])) {
            $user = $context['notify_user'];
        } else {
            $user = $GLOBALS['current_user'];
        }
        return TimeDate::getInstance()->to_display_date_time($inputField, true, true, $user);
    }
    
    public function save(&$bean, $params, $field, $properties, $prefix = '')
    {
        global $timedate;
        if (!isset($params[$prefix.$field])) {
            //$bean->$field = '';
            return;
        }

        if (strpos($params[$prefix.$field], ' ') > 0) {
            if ($timedate->check_matching_format($params[$prefix.$field], TimeDate::DB_DATETIME_FORMAT)) {
                $bean->$field = $params[$prefix.$field];
            } else {
                $bean->$field = $timedate->to_db($params[$prefix.$field]);
            }
        } else {
            $GLOBALS['log']->error('Field ' . $prefix.$field . ' expecting datetime format, but got value: ' . $params[$prefix.$field]);
            //Default to assume date format value
            if ($timedate->check_matching_format($params[$prefix.$field], TimeDate::DB_DATE_FORMAT)) {
                $bean->$field = $params[$prefix.$field];
            } else {
                $bean->$field = $timedate->to_db_date($params[$prefix.$field]);
            }
        }
    }

    /**
     * @see SugarFieldBase::importSanitize()
     */
    public function importSanitize(
        $value,
        $vardef,
        $focus,
        ImportFieldSanitize $settings
        ) {
        global $timedate;

        $format = $timedate->merge_date_time($settings->dateformat, $settings->timeformat);

        if (!$timedate->check_matching_format($value, $format)) {
            $parts = $timedate->split_date_time($value);
            if (empty($parts[0])) {
                $datepart = $timedate->getNow()->format($settings->dateformat);
            } else {
                $datepart = $parts[0];
            }
            if (empty($parts[1])) {
                $timepart = $timedate->fromTimestamp(0)->format($settings->timeformat);
            } else {
                $timepart = $parts[1];
                // see if we can get by stripping the seconds
                if (strpos($settings->timeformat, 's') === false) {
                    $sep = $timedate->timeSeparatorFormat($settings->timeformat);
                    // We are assuming here seconds are the last component, which
                    // is kind of reasonable - no sane time format puts seconds first
                    $timeparts = explode($sep, $timepart);
                    if (!empty($timeparts[2])) {
                        $timepart = implode($sep, array($timeparts[0], $timeparts[1]));
                    }
                }
            }

            $value = $timedate->merge_date_time($datepart, $timepart);
            if (!$timedate->check_matching_format($value, $format)) {
                return false;
            }
        }

        try {
            $date = SugarDateTime::createFromFormat($format, $value, new DateTimeZone($settings->timezone));
        } catch (Exception $e) {
            return false;
        }
        return $date->asDb();
    }
}
