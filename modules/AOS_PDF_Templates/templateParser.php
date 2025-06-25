<?php

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
 * @author SalesAgility Ltd <support@salesagility.com>
 */

use SuiteCRM\Utility\SuiteValidator as SuiteValidator;

#[\AllowDynamicProperties]
class templateParser
{
    public static function parse_template($string, $bean_arr)
    {
        foreach ($bean_arr as $bean_name => $bean_id) {
            $focus = BeanFactory::getBean($bean_name, $bean_id);
            $string = templateParser::parse_template_bean($string, $focus->table_name, $focus);

            foreach ($focus->field_defs as $focus_name => $focus_arr) {
                if ($focus_arr['type'] == 'relate') {
                    if (isset($focus_arr['module']) && $focus_arr['module'] != '' && $focus_arr['module'] != 'EmailAddress') {
                        $idName = $focus_arr['id_name'];
                        $relate_focus = BeanFactory::getBean($focus_arr['module'], $focus->$idName);

                        $string = templateParser::parse_template_bean($string, $focus_arr['name'], $relate_focus);
                    }
                }
            }
        }
        return $string;
    }

    /**
     * @param $string
     * @param $key
     * @param $focus
     * @return mixed
     * @throws Exception
     */
    public static function parse_template_bean($string, $key, &$focus)
    {
        global $app_strings, $sugar_config, $locale, $current_user;
        $repl_arr = array();
        $isValidator = new SuiteValidator();

        foreach ($focus->field_defs as $field_def) {
            if (isset($field_def['name']) && $field_def['name'] != '') {
                $fieldName = $field_def['name'];

                // STIC Custom - JCH - 202210006 - Check if field is really empty
                // STIC#880
                // if (empty($focus->$fieldName)) {
                if (!isset($focus->$fieldName) || $focus->$fieldName == '' ) {
                // END STIC-Custom
                    $repl_arr[$key . '_' . $fieldName] = '';
                    continue;
                }

                if ($field_def['type'] == 'currency') {
                    // STIC-Custom 20250131 ART - Distinguish Type of Discount
                    // https://github.com/SinergiaTIC/SinergiaCRM/pull/575
                    // $repl_arr[$key . "_" . $fieldName] = currency_format_number($focus->$fieldName, $params = array('currency_symbol' => false));
                    // If it comes from aos_products_quotes and is product_discount
                    if ($key == 'aos_products_quotes' && ($field_def["name"] == 'product_discount' || $field_def["name"] ==  'service_discount')) { 
                        if ($focus->discount == 'Percentage') {
                            $repl_arr[$key . "_" . $fieldName] = currency_format_number($focus->$fieldName, $params = array('currency_symbol' => false)) . $app_strings['LBL_PERCENTAGE_SYMBOL'];
                        } else {
                            $repl_arr[$key . "_" . $fieldName] = currency_format_number($focus->$fieldName, $params = array('currency_symbol' => false)) . '€';
                        }
                    } else {
                        $repl_arr[$key . "_" . $fieldName] = currency_format_number($focus->$fieldName, $params = array('currency_symbol' => false));
                    }
                    // END STIC-Custom
                } elseif (($field_def['type'] == 'radioenum' || $field_def['type'] == 'enum' || $field_def['type'] == 'dynamicenum') && isset($field_def['options'])) {
                    $repl_arr[$key . "_" . $fieldName] = translate($field_def['options'], $focus->module_dir, $focus->$fieldName);
                } elseif ($field_def['type'] == 'multienum' && isset($field_def['options'])) {
                    $mVals = unencodeMultienum($focus->{$fieldName});
                    $translatedVals = array();

                    foreach ($mVals as $mVal) {
                        // STIC Custom 20250312 JBL - Avoid Warning: Array to string conversion
                        // https://github.com/SinergiaTIC/SinergiaCRM/pull/477
                        // $translatedVals[] = translate($field_def['options'], $focus->module_dir, $mVal);
                        $translated = translate($field_def['options'], $focus->module_dir, $mVal);
                        if (is_array($translated)) {
                            $translated = implode(", ", $translated);
                        }
                        $translatedVals[] = $translated;
                        // END STIC Custom
                    }

                    $repl_arr[$key . "_" . $fieldName] = implode(", ", $translatedVals);
                } //Fix for Windows Server as it needed to be converted to a string.
                elseif ($field_def['type'] == 'int') {
                    $repl_arr[$key . "_" . $fieldName] = (string)$focus->$fieldName;
                } elseif ($field_def['type'] == 'bool') {
                    if ($focus->{$fieldName} == "1") {
                        // STIC-Custom 20241125 ART - Translated checkbox values in PDF Templates
                        // https://github.com/SinergiaTIC/SinergiaCRM/pull/486
                        // $repl_arr[$key . "_" . $fieldName] = "true";
                        $repl_arr[$key . "_" . $fieldName] = translate('LBL_CHECKBOX_TRUE', 'AOS_PDF_Templates');
                    } else {
                        // STIC-Custom 20241125 ART - Translated checkbox values in PDF Templates - 
                        // https://github.com/SinergiaTIC/SinergiaCRM/pull/486
                        // $repl_arr[$key . "_" . $fieldName] = "false";
                        $repl_arr[$key . "_" . $fieldName] = translate('LBL_CHECKBOX_FALSE', 'AOS_PDF_Templates');
                        // END STIC-Custom
                    }
                } elseif ($field_def['type'] == 'image') {
                    $secureLink = $sugar_config['site_url'] . '/' . "public/" . $focus->id . '_' . $fieldName;
                    $file_location = $sugar_config['upload_dir'] . '/' . $focus->id . '_' . $fieldName;
                    // create a copy with correct extension by mime type
                    if (!file_exists('public')) {
                        sugar_mkdir('public', 0777);
                    }
                    if (!copy($file_location, "public/{$focus->id}".  '_' . $fieldName)) {
                        $secureLink = $sugar_config['site_url'] . '/'. $file_location;
                    }

                    if (empty($focus->{$fieldName})) {
                        $repl_arr[$key . "_" . $fieldName] = "";
                    } else {
                        $link = $secureLink;
                        $repl_arr[$key . "_" . $fieldName] = '<img src="' . $link . '" width="' . $field_def['width'] . '" height="' . $field_def['height'] . '"/>';
                    }
                } elseif ($field_def['type'] == 'wysiwyg') {
                    $repl_arr[$key . "_" . $field_def['name']] = html_entity_decode((string) $focus->$field_def['name'],
                        ENT_COMPAT, 'UTF-8');
                    $repl_arr[$key . "_" . $fieldName] = html_entity_decode((string) $focus->{$fieldName},
                        ENT_COMPAT, 'UTF-8');
                } elseif ($field_def['type'] == 'decimal' || $field_def['type'] == 'float') {
                    // STIC Custom 20250414 ART - SticUtils function for UserPreferences decimals formatting
                    // https://github.com/SinergiaTIC/SinergiaCRM/pull/315
                    require_once 'SticInclude/Utils.php';
                    // END STIC Custom

                    // STIC Custom 20250215 JBL - Remove Warning: Undefined array key access
                    // https://github.com/SinergiaTIC/SinergiaCRM/pull/477
                    // if ($_REQUEST['entryPoint'] == 'formLetter') {
                    if (!empty($_REQUEST['entryPoint']) && $_REQUEST['entryPoint'] == 'formLetter') {
                    // END STIC Custom
                    
                    // STIC Custom 20250414 ART - SticUtils function for UserPreferences decimals formatting
                    // https://github.com/SinergiaTIC/SinergiaCRM/pull/315
                    //     $value = formatDecimalInConfigSettings($focus->$fieldName, true);
                    // } else {
                    //     $value = formatDecimalInConfigSettings($focus->$fieldName, false);
                    // }
                        $value = SticUtils::formatDecimalInConfigSettings($focus->$fieldName, true);
                    } else {
                        $value = SticUtils::formatDecimalInConfigSettings($focus->$fieldName, false);
                    }
                    // END STIC Custom
                    $repl_arr[$key . "_" . $fieldName] = $value;
                // STIC Custom 20250424 JBL - Añadimos funcionalidad Addon campo de Firma
                // https://github.com/SinergiaTIC/SinergiaCRM/pull/315
                } elseif ($field_def['type'] == 'Signature') {
                    $repl_arr[$key . "_" . $fieldName] = '<img src="' . $focus->$fieldName . '" width="'.$field_def['width'].'" height="'.$field_def['height'].'">';
                // END STIC Custom
                // STIC-Custom 20221013 AAM - Parsing date/datetime fields when the bean is being modified
                // STIC#883
                } elseif ((isset($field_def['dbType']) && $field_def['dbType'] == 'date') || 
                          (isset($field_def['dbType']) && $field_def['dbType'] == 'datetime') || 
                          (!isset($field_def['dbType']) && isset($field_def['type']) &&  ($field_def['type'] == 'date' || $field_def['type'] == 'datetime'))) {                    
                    global $disable_date_format;
                    if($focus->$fieldName && ($focus->fetched_row || $disable_date_format)) {
                        $oldValueDisableDateFormat = $disable_date_format;
                        $disable_date_format = false;
                        $value = self::getUserDateDatetimeFormat($focus->$fieldName);
                        $repl_arr[$key . "_" . $fieldName] = $value; 
                        $disable_date_format = $oldValueDisableDateFormat;
                    } else {
                        $repl_arr[$key . "_" . $fieldName] = $focus->{$fieldName};
                    }
                // END STIC
                } else {
                    $repl_arr[$key . "_" . $fieldName] = $focus->{$fieldName};
                }
            }
        } // end foreach()

        krsort($repl_arr);
        reset($repl_arr);

        foreach ($repl_arr as $name => $value) {
            if ((strpos($name, 'product_discount') !== false || strpos($name, 'quotes_discount') !== false) && strpos($name, '_amount') === false) {
                if ($value !== '' && isset($repl_arr['aos_products_quotes_discount'])) {
                    if ($isValidator->isPercentageField($repl_arr['aos_products_quotes_discount'])) {
                        $sep = get_number_separators();
                        $value = rtrim(
                            rtrim(format_number($value), '0'),
                            $sep[1]
                        ) . $app_strings['LBL_PERCENTAGE_SYMBOL'];
                    }
                } else {
                    $value = '';
                }
            }

            if ($name === 'aos_products_product_image' && !empty($value)) {
                $value = '<img src="' . $value . '" class="img-responsive"/>';
            }

            if ($name === 'aos_products_quotes_product_qty') {
                $sep = get_number_separators();
                // STIC-Custom 20230623 AAM - Allowing decimals in the product_qty field
                // STIC#1144
                // $value = rtrim(rtrim(format_number($value), '0'), $sep[1]);
                // First, standarizing decimal separator
                $value = str_replace(',', '.', $value); 
                // Making sure there are two decimals in the value

                // STIC Custom 20250206 JBL - Avoid Uncaught TypeError in number_format
                // https://github.com/SinergiaTIC/SinergiaCRM/pull/477
                // $value = number_format($value, 2, $sep[1], $sep[0]);
                $value = number_format((float) $value, 2, $sep[1], $sep[0]);
                // End STIC Custom
                // END STIC-Custom
            }

            if ($isValidator->isPercentageField($name)) {
                $sep = get_number_separators();

                $precision = $locale->getPrecision($current_user);

                if ($precision === '0') {
                    $params = [
                        'percentage' => true,
                    ];
                    $value = format_number($value, $precision, $precision, $params);
                } else {
                    $value = rtrim(rtrim(format_number($value), '0'), $sep[1]) . $app_strings['LBL_PERCENTAGE_SYMBOL'];
                }
            }
            if (!empty($focus->field_defs[$name]['dbType'])
                && $focus->field_defs[$name]['dbType'] === 'datetime'
                && (strpos($name, 'date') > 0 || strpos($name, 'expiration') > 0)
            ) {
                if ($value != '') {
                    $dt = explode(' ', $value);
                    $value = $dt[0];
                    if (isset($dt[1]) && $dt[1] != '') {
                        if (strpos($dt[1], 'am') > 0 || strpos($dt[1], 'pm') > 0) {
                            $value = $dt[0] . ' ' . $dt[1];
                        }
                    }
                }
            }
            if ($value != '' && is_string($value)) {
                $string = str_replace("\$$name", $value, (string) $string);
            } elseif (strpos($name, 'address') > 0) {
                $string = str_replace("\$$name<br />", '', (string) $string);
                $string = str_replace("\$$name <br />", '', $string);
                $string = str_replace("\$$name", '', $string);
            } else {
                $string = str_replace("\$$name", '&nbsp;', (string) $string);
            }
        }

        return $string;
    }

    /**
     * STIC-Custom AAM 20221013 - Function use to format the date/datetime field into user format
     * STIC#883
     * Some date field definition have the wrong "type" property in their vardef. Such as the field last_rev_create_date from Documents.
     * Therefore we check the field format before formatting
     * STIC#916
     *
     * @param String $date
     * @return String
     */
    private static function getUserDateDatetimeFormat($date) {
        $formatDate = 'Y-m-d';
        $validDate = DateTime::createFromFormat($formatDate, $date);
        $formatDateTime = 'Y-m-d H:i:s';
        $validDateTime = DateTime::createFromFormat($formatDateTime, $date);
        if ($validDate && $validDate->format($formatDate) === $date) {
            // Date field
            global $current_user, $timedate;
            $date = $timedate->fromDbDate($date);
            return $timedate->asUserDate($date, true, $current_user);
        } elseif ($validDateTime && $validDateTime->format($formatDateTime) === $date) {
            // Datetime field
            global $current_user, $timedate;
            $date = $timedate->fromDB($date);
            return $timedate->asUser($date, $current_user);
        } else { 
            return $date;
        }
    }
    // END STIC
}
