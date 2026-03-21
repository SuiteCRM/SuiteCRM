<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2011 - 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

function perform_aos_save($focus)
{
    $currency = fetch_aos_currency($focus);

    foreach ($focus->field_defs as $field) {
        $fieldName = $field['name'];
        $fieldNameDollar = $field['name'].'_usdollar';

        if (isset($focus->field_defs[$fieldNameDollar])) {
            $focus->$fieldNameDollar = '';
            if (!number_empty($focus->field_defs[$field['name']])) {
                if (!isset($focus->$fieldName)) {
                    LoggerManager::getLogger()->warn('Perform AOS Save error: Undefined field name of focus. Focus and field name were: ' . get_class($focus) . ', ' . $fieldName);
                }
                $amountToConvert = isset($focus->$fieldName) ? $focus->$fieldName : null;
                if (!amountToConvertIsDatabaseValue($focus, $fieldName)) {
                    if (!isset($focus->$fieldName)) {
                        LoggerManager::getLogger()->warn('Undefined field for AOS utils / perform aos save. Focus and field name were: [' . get_class($focus) . '], [' . $fieldName . ']');
                        $focusFieldValue = null;
                    } else {
                        $focusFieldValue = $focus->$fieldName;
                    }
                    $amountToConvert = unformat_number($focusFieldValue);
                }

                $focus->$fieldNameDollar = $currency->convertToDollar($amountToConvert);
            }
        }
    }
}

/**
 * @param $focus
 * @return bool|SugarBean
 */
function fetch_aos_currency($focus)
{
    $currency = BeanFactory::newBean('Currencies');
    if (!isset($focus->currency_id)) {
        LoggerManager::getLogger()->warn('Currency is not defined in focus');
        $currency->retrieve();
    } else {
        $currency->retrieve($focus->currency_id);
    }

    return $currency;
}

function amountToConvertIsDatabaseValue($focus, $fieldName)
{
    if (isset($focus->fetched_row)
        && isset($focus->fetched_row[$fieldName])
        && $focus->fetched_row[$fieldName] == $focus->$fieldName) {
        return true;
    }
    return false;
}
