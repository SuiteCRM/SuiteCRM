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
 * Returns the display labels for a module path and field.
 * @param $modulePath
 * @param $field
 * @return array
 */
function getDisplayForField($modulePath, $field, $reportModule)
{
    global $app_list_strings;
    $modulePathDisplay = array();
    $currentBean = BeanFactory::getBean($reportModule);
    $modulePathDisplay[] = $currentBean->module_name;
    if (is_array($modulePath)) {
        $split = $modulePath;
    } else {
        $split = explode(':', $modulePath);
    }
    if ($split && $split[0] == $currentBean->module_dir) {
        array_shift($split);
    }
    foreach ($split as $relName) {
        if (empty($relName)) {
            continue;
        }
        if (!empty($currentBean->field_name_map[$relName]['vname'])) {
            $moduleLabel = trim(
                translate($currentBean->field_name_map[$relName]['vname'], $currentBean->module_dir),
                ':'
            );
        }
        $thisModule = getRelatedModule($currentBean->module_dir, $relName);
        $currentBean = BeanFactory::getBean($thisModule);

        if (!empty($moduleLabel)) {
            $modulePathDisplay[] = $moduleLabel;
        } else {
            $modulePathDisplay[] = $currentBean->module_name;
        }
    }
    $fieldType = $currentBean->field_name_map[$field]['type'];
    $fieldDisplay = $currentBean->field_name_map[$field]['vname'];
    $fieldDisplay = translate($fieldDisplay, $currentBean->module_dir);
    $fieldDisplay = trim($fieldDisplay, ':');
    foreach ($modulePathDisplay as &$module) {
        $module = isset($app_list_strings['aor_moduleList'][$module]) ? $app_list_strings['aor_moduleList'][$module] : (
            isset($app_list_strings['moduleList'][$module]) ? $app_list_strings['moduleList'][$module] : $module
        );
    }
    return array('field' => $fieldDisplay, 'type'=>$fieldType, 'module' => str_replace(' ', '&nbsp;', implode(' : ', $modulePathDisplay)));
}

function requestToUserParameters($reportBean = null)
{
    global $app_list_strings;
    $params = array();
    if (!empty($_REQUEST['parameter_id'])) {
        $dateCount = 0;
        foreach ($_REQUEST['parameter_id'] as $key => $parameterId) {
            if ($_REQUEST['parameter_type'][$key] === 'Multi') {
                $_REQUEST['parameter_value'][$key] = encodeMultienumValue(explode(
                    ',',
                    $_REQUEST['parameter_value'][$key]
                ));
            }

            $condition = BeanFactory::getBean('AOR_Conditions', $_REQUEST['parameter_id'][$key]);
            $value = $_REQUEST['parameter_value'][$key];
            if ($reportBean && $condition && !array_key_exists($value, $app_list_strings['date_time_period_list'])) {
                $value = fixUpFormatting($reportBean->report_module, $condition->field, $value);
            }

            $params[$parameterId] = array(
                'id' => $parameterId,
                'operator' => $_REQUEST['parameter_operator'][$key],
                'type' => $_REQUEST['parameter_type'][$key],
                'value' => $value,
            );

            // Fix for issue #1272 - AOR_Report module cannot update Date type parameter.
            if ($_REQUEST['parameter_type'][$key] === 'Date') {
                $values = array();
                $values[] = $_REQUEST['parameter_date_value'][$key];
                $values[] = $_REQUEST['parameter_date_sign'][$key];
                $values[] = $_REQUEST['parameter_date_number'][$key];
                $values[] = $_REQUEST['parameter_date_time'][$key];

                $params[$parameterId] = array(
                    'id' => $parameterId,
                    'operator' => $_REQUEST['parameter_operator'][$key],
                    'type' => $_REQUEST['parameter_type'][$key],
                    'value' => $values,
                );
                $dateCount++;
            }

            // determine if parameter is a date
            if ($_REQUEST['parameter_type'][$key] === 'Value') {
                $paramLength = strlen((string) $_REQUEST['parameter_value'][$key]);
                $paramValue = $_REQUEST['parameter_value'][$key];
                if ($paramLength === 10) {
                    if (strpos((string) $paramValue, '/') === 2 || strpos((string) $paramValue, '/') === 4) {
                        $params[$parameterId] = array(
                            'id' => $parameterId,
                            'operator' => $_REQUEST['parameter_operator'][$key],
                            'type' => $_REQUEST['parameter_type'][$key],
                            'value' => $value,
                        );
                    } elseif (strpos((string) $paramValue, '-') === 2 || strpos((string) $paramValue, '-') === 4) {
                        $params[$parameterId] = array(
                            'id' => $parameterId,
                            'operator' => $_REQUEST['parameter_operator'][$key],
                            'type' => $_REQUEST['parameter_type'][$key],
                            'value' => $value,
                        );
                    } elseif (strpos((string) $paramValue, '.') === 2 || strpos((string) $paramValue, '.') === 4) {
                        $params[$parameterId] = array(
                            'id' => $parameterId,
                            'operator' => $_REQUEST['parameter_operator'][$key],
                            'type' => $_REQUEST['parameter_type'][$key],
                            'value' => $value,
                        );
                    }
                }
            }
        }
    }

    return $params;
}

function getConditionsAsParameters($report, $override = array())
{
    if (empty($report)) {
        return array();
    }

    global $app_list_strings;
    $conditions = array();
    foreach ($report->get_linked_beans('aor_conditions', 'AOR_Conditions') as $key => $condition) {
        if (!$condition->parameter) {
            continue;
        }

        $path = unserialize(base64_decode($condition->module_path));
        $field_module = $report->report_module;
        if ($path[0] != $report->report_module) {
            foreach ($path as $rel) {
                if (empty($rel)) {
                    continue;
                }
                $field_module = getRelatedModule($field_module, $rel);
            }
        }

        $additionalConditions = unserialize(base64_decode($condition->value));


        $value = isset($override[$condition->id]['value']) ? $override[$condition->id]['value'] : $value = $condition->value;


        $field = getModuleField($field_module, $condition->field, "parameter_value[$key]", 'EditView', $value);
        $disp = getDisplayForField($path, $condition->field, $report->report_module);
        $conditions[] = array(
            'id' => $condition->id,
            'key' => $key,
            'operator' => $condition->operator,
            'operator_display' => $app_list_strings['aor_operator_list'][$condition->operator],
            'value_type' => $condition->value_type,
            'value' => $value,
            'field_display' => $disp['field'],
            'module_display' => $disp['module'],
            'field' => $field,
            'additionalConditions' => $additionalConditions
        );
    }

    return $conditions;
}

/**
 * getPeriodDate
 * @param $date_time_period_list_selected
 * @return DateTime
 */
function getPeriodDate($date_time_period_list_selected, $type = '')
{
    global $sugar_config, $timedate;
    $datetime_period = new DateTime();

    // Setup when year quarters start & end
    if (isset($sugar_config['aor']['quarters_begin'])) {
        $q = calculateQuarters($sugar_config['aor']['quarters_begin']);
    } else {
        $q = calculateQuarters();
    }

    if ($date_time_period_list_selected == 'today') {
        $datetime_period = new DateTime();
    } elseif ($date_time_period_list_selected == 'yesterday') {
        $datetime_period = $datetime_period->sub(new DateInterval("P1D"));
    } elseif ($date_time_period_list_selected == 'this_week') {
        $datetime_period = $datetime_period->setTimestamp(strtotime('this week'));
    } elseif ($date_time_period_list_selected == 'last_week') {
        $datetime_period = $datetime_period->setTimestamp(strtotime('last week'));
    } elseif ($date_time_period_list_selected == 'this_month') {
        $datetime_period = $datetime_period->setDate(
            $datetime_period->format('Y'),
            $datetime_period->format('m'),
            1
        );
    } elseif ($date_time_period_list_selected == 'last_month') {
        $datetime_period = $datetime_period->modify('first day of last month');
    } elseif ($date_time_period_list_selected == 'this_quarter') {
        $thisMonth = $datetime_period->setDate(
            $datetime_period->format('Y'),
            $datetime_period->format('m'),
            1
        );
        if ($thisMonth >= $q[1]['start'] && $thisMonth <= $q[1]['end']) {
            // quarter 1
            $datetime_period = $datetime_period->setDate(
                $q[1]['start']->format('Y'),
                $q[1]['start']->format('m'),
                $q[1]['start']->format('d')
            );
        } elseif ($thisMonth >= $q[2]['start'] && $thisMonth <= $q[2]['end']) {
            // quarter 2
            $datetime_period = $datetime_period->setDate(
                $q[2]['start']->format('Y'),
                $q[2]['start']->format('m'),
                $q[2]['start']->format('d')
            );
        } elseif ($thisMonth >= $q[3]['start'] && $thisMonth <= $q[3]['end']) {
            // quarter 3
            $datetime_period = $datetime_period->setDate(
                $q[3]['start']->format('Y'),
                $q[3]['start']->format('m'),
                $q[3]['start']->format('d')
            );
        } elseif ($thisMonth >= $q[4]['start'] && $thisMonth <= $q[4]['end']) {
            // quarter 4
            $datetime_period = $datetime_period->setDate(
                $q[4]['start']->format('Y'),
                $q[4]['start']->format('m'),
                $q[4]['start']->format('d')
            );
        }
    } elseif ($date_time_period_list_selected == 'last_quarter') {
        $thisMonth = $datetime_period->setDate(
            $datetime_period->format('Y'),
            $datetime_period->format('m'),
            1
        );
        if ($thisMonth >= $q[1]['start'] && $thisMonth <= $q[1]['end']) {
            // quarter 1 - 3 months
            $datetime_period = $q[1]['start']->sub(new DateInterval('P3M'));
        } elseif ($thisMonth >= $q[2]['start'] && $thisMonth <= $q[2]['end']) {
            // quarter 2 - 3 months
            $datetime_period = $q[2]['start']->sub(new DateInterval('P3M'));
        } elseif ($thisMonth >= $q[3]['start'] && $thisMonth <= $q[3]['end']) {
            // quarter 3 - 3 months
            $datetime_period = $q[3]['start']->sub(new DateInterval('P3M'));
        } elseif ($thisMonth >= $q[4]['start'] && $thisMonth <= $q[4]['end']) {
            // quarter 4 - 3 months
            $datetime_period = $q[3]['start']->sub(new DateInterval('P3M'));
        }
    } elseif ($date_time_period_list_selected == 'this_year') {
        $datetime_period = $datetime_period = $datetime_period->setDate(
            $datetime_period->format('Y'),
            1,
            1
        );
    } elseif ($date_time_period_list_selected == 'last_year') {
        $datetime_period = $datetime_period = $datetime_period->setDate(
            $datetime_period->format('Y') - 1,
            1,
            1
        );
    }
    // set time to 00:00:00
    $datetime_period = $datetime_period->setTime(0, 0, 0);

    if($type === 'datetime') {
        $datetime_period->sub(DateInterval::createFromDateString($timedate->getUserUTCOffset() . ' minutes'));
    }

    return $datetime_period;
}

/**
 * getPeriodDate
 * @param $date_time_period_list_selected
 * @return DateTime
 */
function getPeriodEndDate($dateTimePeriodListSelected, $type = '')
{
    global $timedate;

    $datetimePeriod = null;

    switch ($dateTimePeriodListSelected) {
        case 'today':
            $datetimePeriod = new DateTime();
            break;
        case 'yesterday':
            $datetimePeriod = new DateTime("yesterday");
            if($type === 'datetime') {
                $datetimePeriod->setTime(23, 59, 59);
            }
            break;
        case 'this_week':
            $datetimePeriod = new DateTime("this week sunday");
            if($type === 'datetime') {
                $datetimePeriod->setTime(23, 59, 59);
            }
            break;
        case 'last_week':
            $datetimePeriod = new DateTime("last week sunday");
            if($type === 'datetime') {
                $datetimePeriod->setTime(23, 59, 59);
            }
            break;
        case 'this_month':
            $datetimePeriod = new DateTime('last day of this month');
            if($type === 'datetime') {
                $datetimePeriod->setTime(23, 59, 59);
            }
            break;
        case 'last_month':
            $datetimePeriod = new DateTime("last day of last month");
            if($type === 'datetime') {
                $datetimePeriod->setTime(23, 59, 59);
            }
            break;
        case 'this_quarter':
            $thisMonth = new DateTime('first day of this month');
            $thisMonth = $thisMonth->format('n');
            if ($thisMonth < 4) {
                // quarter 1
                $datetimePeriod = new DateTime('last day of march');
                if($type === 'datetime') {
                    $datetimePeriod->setTime(23, 59, 59);
                }
            } elseif ($thisMonth > 3 && $thisMonth < 7) {
                // quarter 2
                $datetimePeriod = new DateTime('last day of june');
                if($type === 'datetime') {
                    $datetimePeriod->setTime(23, 59, 59);
                }
            } elseif ($thisMonth > 6 && $thisMonth < 10) {
                // quarter 3
                $datetimePeriod = new DateTime('last day of september');
                if($type === 'datetime') {
                    $datetimePeriod->setTime(23, 59, 59);
                }
            } elseif ($thisMonth > 9) {
                // quarter 4
                $datetimePeriod = new DateTime('this year last day of december');
                if($type === 'datetime') {
                    $datetimePeriod->setTime(23, 59, 59);
                }
            }
            break;
        case 'last_quarter':
            $thisMonth = new DateTime('first day of this month');
            $thisMonth = $thisMonth->format('n');
            if ($thisMonth < 4) {
                // previous quarter 1
                $datetimePeriod = new DateTime('this year last day of december');
                if($type === 'datetime') {
                    $datetimePeriod->setTime(23, 59, 59);
                }
            } elseif ($thisMonth > 3 && $thisMonth < 7) {
                // previous quarter 2
                $datetimePeriod = new DateTime('last day of march');
                if($type === 'datetime') {
                    $datetimePeriod->setTime(23, 59, 59);
                }
            } elseif ($thisMonth > 6 && $thisMonth < 10) {
                // previous quarter 3
                $datetimePeriod = new DateTime('last day of june');
                if($type === 'datetime') {
                    $datetimePeriod->setTime(23, 59, 59);
                }
            } elseif ($thisMonth > 9) {
                // previous quarter 4
                $datetimePeriod = new DateTime('last day of september');
                if($type === 'datetime') {
                    $datetimePeriod->setTime(23, 59, 59);
                }
            }
            break;
        case 'this_year':
            $datetimePeriod = new DateTime('this year last day of december');
            if($type === 'datetime') {
                $datetimePeriod->setTime(23, 59, 59);
            }
            break;
        case 'last_year':
            $datetimePeriod = new DateTime("last year last day of december");
            if($type === 'datetime') {
                $datetimePeriod->setTime(23, 59, 59);
            }
            break;
    }

    if($type === 'datetime') {
        $datetimePeriod->sub(DateInterval::createFromDateString($timedate->getUserUTCOffset() . ' minutes'));
    }

    return $datetimePeriod;
}

/**
 * @param int $offsetMonths - defines start of the year.
 * @return array - The each quarters boundary
 */
function calculateQuarters($offsetMonths = 0)
{
    // define quarters
    $q = array();
    $q['1'] = array();
    $q['2'] = array();
    $q['3'] = array();
    $q['4'] = array();

    // Get the start of this year
    $q1start = new DateTime();
    $q1start = $q1start->setTime(0, 0, 0);
    $q1start = $q1start->setDate($q1start->format('Y'), 1, 1);
    /*
     * $offsetMonths gets added to the current month. Therefor we need this variable to equal one less than the start
     * month. So Jan becomes 0. Feb => 1 and so on.
     */
    $offsetMonths -= 1;
    // Offset months
    if ($offsetMonths > 0) {
        $q1start->add(new DateInterval('P' . $offsetMonths . 'M'));
    }
    $q1end = DateTime::createFromFormat(DATE_ISO8601, $q1start->format(DATE_ISO8601));
    $q1end->add(new DateInterval('P2M'));

    $q2start = DateTime::createFromFormat(DATE_ISO8601, $q1start->format(DATE_ISO8601));
    $q2start->add(new DateInterval('P3M'));
    $q2end = DateTime::createFromFormat(DATE_ISO8601, $q2start->format(DATE_ISO8601));
    $q2end->add(new DateInterval('P2M'));

    $q3start = DateTime::createFromFormat(DATE_ISO8601, $q2start->format(DATE_ISO8601));
    $q3start->add(new DateInterval('P3M'));
    $q3end = DateTime::createFromFormat(DATE_ISO8601, $q3start->format(DATE_ISO8601));
    $q3end->add(new DateInterval('P2M'));

    $q4start = DateTime::createFromFormat(DATE_ISO8601, $q3start->format(DATE_ISO8601));
    $q4start->add(new DateInterval('P3M'));
    $q4end = DateTime::createFromFormat(DATE_ISO8601, $q4start->format(DATE_ISO8601));
    $q4end->add(new DateInterval('P2M'));

    // Assign quarter boundaries
    $q['1']['start'] = $q1start;
    $q['1']['end'] = $q1end;
    $q['2']['start'] = $q2start;
    $q['2']['end'] = $q2end;
    $q['3']['start'] = $q3start;
    $q['3']['end'] = $q3end;
    $q['4']['start'] = $q4start;
    $q['4']['end'] = $q4end;

    return $q;
}

/**
 * convertDateValue
 * @param string $value - date in any user format
 * @return DateTime $dateTime - converted string
 */
function convertToDateTime($value)
{

    global $current_user, $timedate;

    $user_dateformat = $current_user->getPreference('datef');

    // In some cases the date string already is in database format
    if ($timedate->check_matching_format($value, $timedate->get_db_date_format())) {
        $user_dateformat = $timedate->get_db_date_format();
    }

    $formattedValue = null;

    switch ($user_dateformat) {
        case 'Y-m-d':
            $formattedValue = date('Y-m-d', strtotime($value));
            break;
        case 'm-d-Y':
            $formattedValue = $value;
            $day = substr($formattedValue, 3, 2);
            $month = substr($formattedValue, 0, 2);
            $year = substr($formattedValue, 6, 4);
            $formattedValue = substr_replace($formattedValue, $day, 6, 4);
            $formattedValue = substr_replace($formattedValue, $month, 3, 2);
            $formattedValue = substr_replace($formattedValue, $year, 0, 2);
            $formattedValue = date('Y-m-d', strtotime($formattedValue));
            break;
        case 'd-m-Y':
            $formattedValue = $value;
            $day = substr($formattedValue, 0, 2);
            $month = substr($formattedValue, 3, 2);
            $year = substr($formattedValue, 6, 4);
            $formattedValue = substr_replace($formattedValue, $day, 6, 4);
            $formattedValue = substr_replace($formattedValue, $month, 3, 2);
            $formattedValue = substr_replace($formattedValue, $year, 0, 2);
            $formattedValue = date('Y-m-d', strtotime($formattedValue));
            break;
        case 'Y/m/d':
            $formattedValue = str_replace('/', '-', $value);
            break;
        case 'm/d/Y':
            $formattedValue = str_replace('/', '-', $value);
            $day = substr($formattedValue, 3, 2);
            $month = substr($formattedValue, 0, 2);
            $year = substr($formattedValue, 6, 4);
            $formattedValue = substr_replace($formattedValue, $day, 6, 4);
            $formattedValue = substr_replace($formattedValue, $month, 3, 2);
            $formattedValue = substr_replace($formattedValue, $year, 0, 2);
            $formattedValue = date('Y-m-d', strtotime($formattedValue));
            break;
        case 'd/m/Y':
            $formattedValue = str_replace('/', '-', $value);
            $day = substr($formattedValue, 0, 2);
            $month = substr($formattedValue, 3, 2);
            $year = substr($formattedValue, 6, 4);
            $formattedValue = substr_replace($formattedValue, $day, 6, 4);
            $formattedValue = substr_replace($formattedValue, $month, 3, 2);
            $formattedValue = substr_replace($formattedValue, $year, 0, 2);
            $formattedValue = date('Y-m-d', strtotime($formattedValue));
            break;
        case 'Y.m.d':
            $formattedValue = str_replace('.', '-', $value);
            break;
        case 'd.m.Y':
            $formattedValue = str_replace('.', '-', $value);
            $day = substr($formattedValue, 0, 2);
            $month = substr($formattedValue, 3, 2);
            $year = substr($formattedValue, 6, 4);
            $formattedValue = substr_replace($formattedValue, $day, 6, 4);
            $formattedValue = substr_replace($formattedValue, $month, 3, 2);
            $formattedValue = substr_replace($formattedValue, $year, 0, 2);
            $formattedValue = date('Y-m-d', strtotime($formattedValue));
            break;
        case 'm.d.Y':
            $formattedValue = str_replace('.', '-', $value);
            $day = substr($formattedValue, 3, 2);
            $month = substr($formattedValue, 0, 2);
            $year = substr($formattedValue, 6, 4);
            $formattedValue = substr_replace($formattedValue, $day, 6, 4);
            $formattedValue = substr_replace($formattedValue, $month, 3, 2);
            $formattedValue = substr_replace($formattedValue, $year, 0, 2);
            $formattedValue = date('Y-m-d', strtotime($formattedValue));
            break;
    }

    $formattedValue .= ' 00:00:00';
    $userTimezone = $current_user->getPreference('timezone');
    $utz = new DateTimeZone($userTimezone);
    $dateTime = DateTime::createFromFormat(
        'Y-m-d H:i:s',
        $formattedValue,
        $utz
    );
    $dateTime->setTimezone(new DateTimeZone('UTC'));

    return $dateTime;
}
