<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
/**
 * Products, Quotations & Invoices modules.
 * Extensions to SugarCRM
 * @package Advanced OpenSales for SugarCRM
 * @subpackage Products
 * @copyright SalesAgility Ltd http://www.salesagility.com
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU AFFERO GENERAL PUBLIC LICENSE
 * along with this program; if not, see http://www.gnu.org/licenses
 * or write to the Free Software Foundation,Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301  USA
 *
 * @author SalesAgility Ltd <info@salesagility.com>
 */

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
