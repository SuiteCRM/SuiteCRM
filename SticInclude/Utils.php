<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */
#[\AllowDynamicProperties]
class SticUtils
{

    /**
     * Dynamically updates an array of fields of the DetailView after a change in a subpanel.
     * This function must be invoked from module controller.php, inside the action_SubPanelViewer() function
     *
     * @param String $moduleName Example: stic_Events
     * @param String $subpanelName Example: stic_events_stic_registrations
     * @param string $fieldToUpdate It should contain an array of the fields that might be updated:
     *   $fieldsToUpdate = array(
     *       'total_attendances' => array('type' => 'integer'),
     *   );
     * If it is a multienum, the list should be specified:
     *   $fieldsToUpdate = array(
     *       'stic_relationship_type_c' => array (
     *           'type' => 'multienum',
     *           'list' => 'stic_contacts_relationships_types_list',
     *       ),
     *   );
     * @return void
     */
    public static function updateFieldOnSubpanelChange($moduleName, $subpanelName, $fieldsToUpdate)
    {

        require_once 'include/SubPanel/SubPanelViewer.php';

        $actionName = 'SubPanelViewer';

        if (
            array_key_exists('module', $_REQUEST) &&
            array_key_exists('subpanel', $_REQUEST) &&
            array_key_exists('action', $_REQUEST) &&
            ($_REQUEST['module'] == $moduleName) &&
            ($_REQUEST['subpanel'] == $subpanelName) &&
            ($_REQUEST['action'] == $actionName)
        ) {

            $subpanel = "#subpanel_" . $_REQUEST['subpanel'];
            $js = <<<EOQ
            <script>
                if (($("$subpanel").length > 0) && ($("$subpanel").hasClass('in'))) {
EOQ;
            $recordId = $_REQUEST['record'];
            $bean = BeanFactory::getBean($moduleName, $recordId);

            foreach ($fieldsToUpdate as $field => $fieldData) {
                switch ($fieldData['type']) {
                    case 'multienum':
                        $list = $fieldData['list'];
                        $encodedField = $bean->$field;
                        $js .= <<<EOQ
                        if (!$('#$field').val() && '$encodedField') {
                            $('[field=$field]').append('<input type="hidden" class="sugar_field" id="$field" value="">');
                        }
                        if ($('#$field').val() != '$encodedField') {
                            newArray = SUGAR.MultiEnumAutoComplete.getMultiSelectValuesFromKeys('$list', '$encodedField');
                            liTag = '</li><li style="margin-left:10px;">';
                            fieldString = '<li style="margin-left:10px;">';
                            fieldString += newArray.join(liTag);
                            fieldString += '</li>';
                            $('#$field').nextAll().remove();
                            if (!'$encodedField') {
                                $('#$field').remove();
                            } else {
                                $('#$field').val('$encodedField');
                                $('#$field').after(fieldString);
                            }
                            $('[field=$field]').fadeOut(500).fadeIn(1000);
                        }
EOQ;
                        break;
                    case 'decimal':
                        $fieldValue = formatDecimalInConfigSettings($bean->$field);
                        $js .= <<<EOQ
                        if ($('#$field').text() != '$fieldValue') {
                            $('#$field').text('$fieldValue').fadeOut(500).fadeIn(1000);
                        }
EOQ;
                        break;
                    default:
                        $fieldValue = $bean->$field;
                        $js .= <<<EOQ
                        if ($('#$field').text() != '$fieldValue') {
                            $('#$field').text('$fieldValue').fadeOut(500).fadeIn(1000);
                        }
EOQ;
                        break;
                }
            }
            $js .= <<<EOQ
            }
            </script>
EOQ;
            echo $js;
        }
    }

    /**
     * Get an related bean from the $bean module
     *
     *
     * @param Object $bean of the module from which we make the request
     * @param String $relationshipName Name of the relationships from which we want to get the $relatedBean
     * @return Object relatedModule Bean
     */
    public static function getRelatedBeanObject($bean, $relationshipName)
    {
        if (!$bean || !$bean->load_relationship($relationshipName)) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': : Failed retrieve contacts relationship data');
            return false;
        }
        $relatedBeans = $bean->$relationshipName->getBeans();
        $relatedBean = array_pop($relatedBeans);
        return !($relatedBean) ? false : $relatedBean;
    }

    /**
     * Clean a text string, removing unsupported characters
     *
     * @param String $text
     * @return String Clean $text
     */
    public static function cleanText($text)
    {
        // Store the input text
        $initText = $text;
        // Look for the php version of the instance to know what coding to apply
        $version = phpversion();
        if ($version == '5.3.29') {
            $text = htmlentities($text, ENT_QUOTES | ENT_XML1, "UTF-8");
        } else {
            $text = htmlentities($text);
        }
        // preg_replace changes everything that is not alphanumeric for its equivalent
        $text = preg_replace('/\&(.)[^;]*;/', '\\1', $text);

        // We control some characters that are not processed as we expect by preg_replace
        $text = str_replace(array("a#039;", "'"), ' ', $text);

        // We turn to uppercase so that in the following 'for' you do not have to search for lower case
        $text = strtoupper($text);

        // preg_replace does not treat the characters 'symbols' (Ex: ⧦𝑍𝑎𝑏𝑐𝛥𝛩𝛽𝛷𝛹𝛺𝜃𝜋𝜑𝜔 ♠ ︎ ♣ ︎ ♥ ︎ ♦ ︎), so we look for them and if there is,  we replace them with a '_'
        for ($i = 0; $i < strlen($text); $i++) {
            $permitidos = (',.-:()/ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ');
            $found = strpos($permitidos, $text[$i]);
            if ($found == false) {
                $text[$i] = ' ';
            }
        }

        // We send error log if the input string and the output string have different number of characters.
        mb_strlen($initText) != mb_strlen($text) ? $GLOBALS['log']->fatal('[Funcion $this->cleanText] String  ' . $initText . ' is converted to ' . $text . ' with the wrong number of characters') : '';

        return $text;
    }

    /**
     * Fill in the string indicated in '$text' with the character '$character' until you reach '$length' positions to the RIGTH. In case '$text' is longer than '$length', cut it to the positions.
     *
     * @param String $text The text to procces
     * @param String $character The character to fill
     * @param Int $length the final length of $text
     * @param Boolean $cut Default 'yes'. If cut text.
     * @return String
     */
    public static function fillRigth($text, $character, $length, $cut = true)
    {
        $text = self::cleanText($text);

        // If the text is longer than the one indicated in $cut we cut it
        if ($cut) {
            $text = substr($text, 0, $length);
        }

        // If the text is less than the length indicated in $length, we fill in the remaining space with the appropriate character
        for ($i = mb_strlen($text, 'UTF-8'); $i < $length; $i++) {
            $text = $text . $character;
        }

        return $text;
    }

    /**
     * Fill in the string indicated in '$text' with the character '$character' until you reach '$length' positions to the LEFT. In case '$text' is longer than '$length', cut it to the positions.
     *
     * @param String $text The text to procces
     * @param String $character The character to fill
     * @param Int $length the final length of $text
     * @param Boolean $cut Default 'yes'. If cut text.
     * @return String
     */
    public static function fillLeft($text, $character, $length, $cut = true)
    {
        $text = self::cleanText($text);

        // If the text is longer than the one indicated in $cut we cut it
        if ($cut) {
            $text = substr($text, 0, $length);
        }

        // If the text is less than the length indicated in $length, we fill in the remaining space with the appropriate character
        for ($i = mb_strlen($text, 'UTF-8'); $i < $length; $i++) {
            $text = $character . $text;
        }

        return $text;
    }

    /**
     * Main function to check if IBAN is correct
     *
     * @param String $iban The IBAN checked
     * @param Boolean $strictMode true requires that the string has no spaces, false supports strings with spaces and other signs
     * @return Boolean True if IBAN is correct, false if not
     */
    public static function checkIBAN($iban, $strictMode = true)
    {
        require_once 'modules/stic_Settings/Utils.php';

        // We ignore the validation of the account number if the variable GENERAL_IBAN_VALIDATION is set to 0
        if (stic_SettingsUtils::getSetting('GENERAL_IBAN_VALIDATION') == 0) {
            return true;
        } else {
            require_once 'SticInclude/vendor/php-iban/php-iban.php';
            return verify_iban($iban, $strictMode);
        }
    }

    /**
     * Validate if the string introduced is a valid Spanish nif or nie
     * Based in http://www.michublog.com/informatica/8-funciones-para-la-validacion-de-formularios-con-expresiones-regulares
     * STIC jch 20210609 - Added validation for special NIFS starting with K, L or M https://www.agenciatributaria.es/AEAT.internet/Inicio/La_Agencia_Tributaria/Campanas/_Campanas_/Fiscalidad_de_no_residentes/_Identificacion_/Preguntas_frecuentes_sobre_obtencion_de_NIF_de_no_Residentes/_Que_tipos_de_NIF_de_personas_fisicas_utiliza_la_normativa_tributaria_espanola_.shtml
     *
     * @param String $nif
     * @return boolean
     */
    public static function isValidNIForNIE($nif)
    {

        // Convert the string to uppercase and add leading zeros to complete the 8 digits.
        $nif = strtoupper(str_pad($nif, 9, "0", STR_PAD_LEFT));

        $nifRegEx = '/^[0-9]{8}[A-Z]$/i';
        $nieAndSpecialNifRegEx = '/^[KLMXYZ][0-9]{7}[A-Z]$/i';

        $letters = "TRWAGMYFPDXBNJZSQVHLCKE";

        if (preg_match($nifRegEx, $nif)) {
            return ($letters[(substr($nif, 0, 8) % 23)] == $nif[8]);
        } else if (preg_match($nieAndSpecialNifRegEx, $nif)) {
            if (in_array($nif[0], array('X', 'K', 'L', 'M'))) {
                $nif[0] = "0";
            } else if ($nif[0] == "Y") {
                $nif[0] = "1";
            } else if ($nif[0] == "Z") {
                $nif[0] = "2";
            }
            return ($letters[(substr($nif, 0, 8) % 23)] == $nif[8]);
        } else {
            return false;
        }
    }

    /**
     * Clean the string of invalid characters in NIF
     * @param String NIF
     * @return String clean NIF
     */
    public static function cleanNIF($nif)
    {
        return preg_replace('/[^TRWAGMYFPDXBNJZSQVHLCKE0-9]/', '', mb_strtoupper($nif));
    }

    /**
     * Clean a string of characters that are not valid for NIF or CIF
     * This function is less accurate than cleanNIF since there are characters that are valid for CIF, but not for NIF, such as the letter U
     * This function should only be used when it is not possible to determine if a CIF or NIF type identifier is being processed
     *
     * @param String NIF o CIF
     * @return String
     */
    public static function cleanNIForCIF($fiscalId)
    {
        return preg_replace('/[^A-Z0-9]/', '', mb_strtoupper($fiscalId));
    }

    /**
     * Check if a cif is valid http://www.michublog.com/informatica/8-funciones-para-la-validacion-de-formularios-con-expresiones-regulares
     *
     * @param String $cif
     * @return Boolean
     */
    public static function isValidCIF($cif)
    {
        $cif = strtoupper($cif);

        $cifRegEx1 = '/^[ABEH][0-9]{8}$/i';
        $cifRegEx2 = '/^[KPQS][0-9]{7}[A-J]$/i';
        $cifRegEx3 = '/^[CDFGJLMNRUVW][0-9]{7}[0-9A-J]$/i';

        if (preg_match($cifRegEx1, $cif) || preg_match($cifRegEx2, $cif) || preg_match($cifRegEx3, $cif)) {
            $control = $cif[strlen($cif) - 1];
            $sumA = 0;
            $sumB = 0;

            for ($i = 1; $i < 8; $i++) {
                if ($i % 2 == 0) {
                    $sumA += intval($cif[$i]);
                } else {
                    $t = (intval($cif[$i]) * 2);
                    $p = 0;

                    for ($j = 0; $j < strlen($t); $j++) {
                        $p += substr($t, $j, 1);
                    }
                    $sumB += $p;
                }
            }

            $sumC = (intval($sumA + $sumB)) . "";
            $sumD = (10 - intval($sumC[strlen($sumC) - 1])) % 10;

            $letters = "JABCDEFGHI";

            if ($control >= "0" && $control <= "9") {
                return ($control == $sumD);
            } else {
                return (strtoupper($control) == $letters[$sumD]);
            }
        } else {
            return false;
        }
    }

    /**
     * Validate a generic SugarCRM field
     *
     * @param String $field Field Name
     * @param Array $fieldDefs Field definition array (available from Bean itself)
     * @param Boolean $isEmptyValid Indicates if the empty value is interpreted as valid, by default the value of the field definition is taken into account
     * @return Boolean
     */
    public static function isValidField($field, $value, $fieldDefs, $isEmptyValid = null)
    {

        // If you have no definition of the field, return false
        if (empty($fieldDefs) || empty($fieldDefs[$field])) {
            return false;
        }

        // If it is an empty value, check if it is required or if it is accepted as valid
        if (empty($value)) {
            return (($isEmptyValid === null && $fieldDefs[$field]['required'] == 0) || $isEmptyValid);
        }

        // If it is not empty, check each field
        switch ($fieldDefs[$field]['type']) {
            case 'datetime':
                return self::isValidDateValue($field, $value, $fieldDefs);
            case 'date':
                return self::isValidDateValue($field, $value, $fieldDefs);
            case 'enum':
                return self::isValidSelectValue($field, $value, $fieldDefs);
            case 'int':
                return self::isValidIntValue($value, $fieldDefs[$field]['min'], $fieldDefs[$field]['max']);
            case 'currency':
            case 'float':
                return self::isValidFloatValue($value, $fieldDefs[$field]['min'], $fieldDefs[$field]['max']);
            default:
                return true;
        }
    }

    /**
     * Check that the data of an integer field is valid
     *
     * @param String $field Field Name
     * @param Integer minimum
     * @param Integer maximum
     * @return boolean
     */
    public static function isValidIntValue($value, $min = null, $max = null)
    {
        $wrongChars = preg_grep('/^[-+]*(\d+)$/', array($value), PREG_GREP_INVERT);
        return empty($wrongChars) &&
            ($min === null || $value > $min) &&
            ($max === null || $value < $max);
    }

    /**
     * Check that the data of a field of type float is valid
     *
     * @param String $field Field Name
     * @param Array $fieldDefs Field definition array (available from Bean itself)
     * @return Boolean
     */
    public static function isValidFloatValue($value, $min = null, $max = null)
    {
        $wrongChars = preg_grep('/^[-+]*[\d]+[.,]*[\d]+$/', array($value), PREG_GREP_INVERT);
        return empty($wrongChars) &&
            ($min === null || $value > $min) &&
            ($max === null || $value < $max);
    }

    /**
     * Check that the data of a field of type select is valid
     *
     * @param String $field Field Name
     * @param Array $fieldDefs Field definition array (available from Bean itself)
     * @return Boolean
     */
    protected static function isValidSelectValue($field, $value, $fieldDefs)
    {
        global $app_list_strings;
        $options_key = $fieldDefs[$field]['options'];
        $field_options = array_keys($app_list_strings[$options_key]);
        return in_array($value, $field_options);
    }

    /**
     * Check that the data of a date type field is valid
     *
     * @param String $field Field Name
     * @param Array $fieldDefs Field definition array (available from Bean itself)
     * @return Boolean
     */
    protected static function isValidDateValue($field, $value, $fieldDefs)
    {
        $time = TimeDate::getInstance();
        return $time->fromString($value) != null;
    }

    /**
     * Compare two dates with database date format
     *
     * @param String $value1 date (yyyy-mm-dd hh:mm:ss)
     * @param String $value2 date (yyyy-mm-dd hh:mm:ss)
     * @return Int -1 0 1
     */
    public static function compareDate($value1, $value2)
    {
        $time1 = TimeDate::getInstance();
        $time2 = TimeDate::getInstance();
        $time1 = $time1->fromString($value1);
        $time2 = $time2->fromString($value2);
        $ret = ($time1 > $time2 ? -1 : ($time1 == $time2 ? 0 : 1));
        $GLOBALS['log']->debug(__METHOD__ . ":Comparing dates [{$value1}] [{$value2}] [{$ret}]");
        return $ret;
    }

    /**
     * Clean the invalid characters of an IBAN
     *
     * @param String $iban Número de cuenta
     * @return String
     */
    public static function cleanIBAN($iban)
    {
        return preg_replace('/[^0-9a-zA-Z]/', '', $iban);
    }

    /**
     * Indicates whether a command is valid or not
     *
     * @param String $mandate
     * @param Boolean
     */
    public static function isValidMandate($mandate)
    {
        return (!empty($mandate) && mb_strlen($mandate) < 36);
    }

    /**
     * Create a valid mandate number
     *
     * @return Int
     */
    public static function createMandate()
    {
        return mt_rand(10000000, 99999999);
    }

    /**
     * Set value with appropriate separators according to user/system configuration
     *
     * @param Decimal $decimalValue
     * @param Boolean $userSetting. Indicates whether to choose user or system configuration
     * @return Decimal
     */
    public static function formatDecimalInConfigSettings($decimalValue, $userSetting = false)
    {
        global $current_user, $sugar_config;

        // Get the user preferences for the thousands and decimal separator and the number of decimal places 
        if ($userSetting) {
            // User decimal separator
            $user_dec_sep = (!empty($current_user->id) ? $current_user->getPreference('dec_sep') : null);
            // User thousands separator
            $user_grp_sep = (!empty($current_user->id) ? $current_user->getPreference('num_grp_sep') : null);
            // User number of decimal places
            $user_sig_digits = (!empty($current_user->id) ? $current_user->getPreference('default_currency_significant_digits') : null);
        }

        // Set the user preferences or the default preferences
        $dec_sep = empty($user_dec_sep) ? $sugar_config['default_decimal_seperator'] : $user_dec_sep;
        $grp_sep = empty($user_grp_sep) ? $sugar_config['default_number_grouping_seperator'] : $user_grp_sep;
        $sig_digits = empty($user_sig_digits) ? $sugar_config['default_currency_significant_digits'] : $user_sig_digits;

        // Format the number
        $value = number_format((float)$decimalValue, $sig_digits, $dec_sep, $grp_sep);
        return $value;
    }

    /**
     * Displays an error on the screen when refreshing the page and interrupts the execution
     * @param Object $obj The object that contains the BEAN
     * @param String $msg The message to show
     */
    public static function showErrorMessagesAndDie($obj, $msg)
    {
        $obj->bean->log = $msg;
        $obj->bean->save();
        SugarApplication::appendErrorMessage('<div class="msg-fatal-lock">' . $msg . '</div>');
        SugarApplication::redirect("index.php?module={$obj->bean->module_dir}&action=DetailView&record={$obj->bean->id}");

        die();
    }

    /**
     * Builds and returns an HTML anchor pointing to the record detail view
     * string $module Record's module name
     * string $id Record id
     * string $text Text to be inserted into the anchor
     *
     * @return void
     */

    public static function createLinkToDetailView($module, $id, $text)
    {
        global $sugar_config;
        $site_url = rtrim($sugar_config['site_url'], "/"); // Remove slash if exists, will be added later anyway
        return "<a href=\"{$site_url}/index.php?module={$module}&action=DetailView&record={$id}\">$text</a>";
    }

    /**
     * This function creates and save a duplicate bean of a given module record, applying
     * changes indicated in the $changes parameter.
     * This function has been created to duplicate the records related to a record that is duplicated by
     * massive duplication (Example: Survey questions in Surveys, Products in AOS_Quotes, etc.)
     *
     * @param String $module The name of the module to create a duplicate record from
     * @param String $id The ID of the record to duplicate
     * @param Array $changes An array of changes to apply to the duplicate record. The array should contain
     * key-value pairs, where the key is the name of the field to update and the value
     * is the new value for that field. If this array is empty, an exact copy of the
     * original record will be created.
     *
     * @return String The ID of the newly created duplicate record.

     */
    public static function duplicateBeanRecord($module, $id, $changes)
    {

        // Get the source bean
        $sourceBean = BeanFactory::getBean($module, $id, $changes);

        // Create a duplicate bean
        $duplicateBean = $sourceBean;

        // Remove the fetched row property
        unset($duplicateBean->fetched_row);

        // Generate a new ID
        $newId = create_guid();

        // Set new ID and mark it as new
        $duplicateBean->new_with_id = true;
        $duplicateBean->id = $newId;

        // Clear the dates
        $duplicateBean->date_entered = '';
        $duplicateBean->date_modified = '';

        // Ensure proper format in field types with decimal values
        $decimalFields = array_filter($duplicateBean->field_name_map, function ($k) {
            return in_array($k['type'], ['decimal', 'currency', 'float']);
        }, ARRAY_FILTER_USE_BOTH);
        foreach ($decimalFields as $key => $value) {
            $duplicateBean->$key = (float) number_format((float)$duplicateBean->$key, $value['precision'] ?? 2, '.', '');
        }

        // Apply any changes
        if (!empty($changes)) {
            foreach ($changes as $key => $value) {
                $duplicateBean->$key = $value;
            }
        }

        // Save the duplicate bean
        $duplicateBean->save();

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . " Mass duplicate related record {$id} from module {$module}. New record created {$newId}");

        // Return the new ID
        return $newId;
    }
    /**
     * unformatDecimal - Function to convert formatted decimal number to float value
     * @param mixed $number The formatted decimal number as string or float
     * @param int $precision The number of decimal places in the input number (default is 2)
     * @return float The float value of the input number after removing formatting
     */
    public static function unformatDecimal($number, $precision = 2)
    {
        if (is_float($number)) {
            // If the input number is already a float value, return it
            return $number;
        } elseif (is_string($number)) {
            // If the input number is a string, remove formatting and convert to float
            if (!ctype_punct(substr($number, -$precision - 1, 1))) {
                // If the last character before the decimal is not a punctuation mark, remove all non-numeric characters
                $number = floatval(preg_replace("/[^0-9]/", "", $number));
            } else {
                // If the last character before the decimal is a punctuation mark, remove all non-numeric characters and format the decimal places
                $number = preg_replace("/[^0-9]/", "", $number);
                $number = floatval(substr($number, 0, -2) . '.' . substr($number, -2));
            }
            return $number; // Return the float value of the input number after removing formatting
        }
    }

    /**
     * Formats date and datetime strings into the standard database format.
     * This function is based on the getDBFormat function found in modules/AOW_Actions/FormulaCalculator.php.
     * It ensures that both date and datetime strings are standardized for database storage, accommodating both
     * the database's default format and user-specific formats.
     *
     * @param string $date String representing a date. This can be in the format expected by the database or a SuiteCRM user-defined format.
     *                     It attempts to standardize dates (Y-m-d) and datetimes (Y-m-d H:i:s) for database insertion.
     * @return string|null Returns a formatted string suitable for database storage if the input is valid. If the input date
     *                     cannot be processed or is invalid, it returns null. 
     */
    public static function formatDateForDatabase($date)
    {
        $originalDate=$date;
        $formatDate = 'Y-m-d'; // Defines the standard date format for comparison and formatting.
        $validDate = DateTime::createFromFormat($formatDate, $date); // Attempts to create a DateTime object based on the standard date format.

        if (!$validDate) {
            // If the initial attempt fails, it tries to create a DateTime object with a datetime format.
            $validDate = DateTime::createFromFormat('Y-m-d H:i:s', $date);
            if ($validDate) {
                // If successful, formats the DateTime object to the standard date format.
                $date = $validDate->format('Y-m-d');
            }
        }

        // Checks if the string matches the date format without time, returning it unchanged if it does.
        if ($validDate && $validDate->format($formatDate) === $date) {
            return $date;
        } else {
            global $current_user, $timedate;
            // Determines if the string includes a time component by checking for a space character.
            if (strpos($date, " ") !== false) {
                $type = 'datetime';
            } else {
                $type = 'date';
            }
            // Converts the date from the user's format to the database format, leveraging the user's settings.
            $date = $timedate->fromUserType($date, $type, $current_user);
            if ($date) {
                // If conversion is successful, returns the date as a string in database format.
                return $date->asDbDate(false);
            }
            // Returns null if the date cannot be formatted to the database's expectations, indicating an invalid input.
            $GLOBALS['log']->fatal('Line '.__LINE__.': '.__METHOD__.': '."The date [$originalDate] is invalid or uses an unsupported format.");
            return null;
        }
    }

    /**
     * Parses a SuiteCRM email template by replacing variables
     * with values from a provided set of Beans and special variables such $sugarurl.
     *
     * @param string $templateId The ID of the EmailTemplate to parse.
     * @param array  $Beans      An array of SugarBean objects (e.g., a Contact, an Account, stic_Payments, etc.)
     * whose data will be used for replacements.
     *
     * @return array An associative array with keys 'subject', 'body', and 'body_html'
     * containing the parsed and cleaned content.
     */
    public static function parseEmailTemplate($templateId, $Beans)
    {

        // Load SuiteCRM global variables
        global $app_list_strings, $sugar_config, $current_user;

        // 1. Fetch the email template
        // Using getBean instead of retrieveBean as requested
        $template = BeanFactory::getBean('EmailTemplates', $templateId);

        // Initialize the response array
        $parsedTemplate = [
            'subject' => '',
            'body' => '',
            'body_html' => '',
        ];

        // Check if the template was loaded successfully
        if (!$template) {
            // Use SuiteCRM logger
            $GLOBALS['log']->error("parseEmailTemplate: Could not find EmailTemplate with ID: $templateId");
            return $parsedTemplate; // Return empty array if template not found
        }

        // Load the original content to be parsed
        $parsedTemplate['subject'] = $template->subject;
        $parsedTemplate['body'] = $template->body;
        $parsedTemplate['body_html'] = $template->body_html;

        // --- SUBSTITUTION COLLECTION ---

        // 2. Create collection arrays
        // We use one for plain text (subject, body) and one for HTML (body_html)
        // as the value formatting can differ (e.g., nl2br for textareas).
        $replacements_text = [];
        $replacements_html = [];

        // 3. Iterate over each provided Bean (Contact, Account, Quote, etc.)
        foreach ($Beans as $bean) {
            if (!$bean || !isset($bean->module_dir)) {
                continue; // Skip if it's not a valid bean
            }

            $moduleName = $bean->module_dir;
            $prefix = '';

            // 4. Determine the variable prefix based on the module
            // Variable syntax is $<prefix>_<field_name>
            switch ($moduleName) {
                case 'Contacts':
                    $prefix = '$contact_';
                    break;
                case 'Users':
                    // $contact_user_ refers to a User Bean (e.g., "Assigned To")
                    $prefix = '$contact_user_';
                    break;
                case 'Leads':
                    $prefix = '$contact_lead_';
                    break;
                case 'Accounts':
                    $prefix = '$contact_account_';
                    break;
                default:
                    // Standard module (e.g., Quotes -> $quotes_)
                    $prefix = '$' . strtolower($moduleName) . '_';
                    break;
            }

            // 5. Iterate over the Bean's fields and collect values
            foreach ($bean->field_defs as $fieldName => $def) {

                $placeholder = $prefix . $fieldName;
                $rawValue = $bean->$fieldName ?? '';
                $displayValue = $rawValue;
                $htmlValue = $rawValue;

                // 6. Handle dropdown fields (enum)
                // We want the display label (e.g., "In Progress") instead of the internal key (e.g., "in_progress")
                if (isset($def['type']) && ($def['type'] == 'enum' || $def['type'] == 'multienum' || $def['type'] == 'radioenum') && isset($def['options'])) {

                    $listName = $def['options']; // e.g., 'sales_stage_dom'

                    if (isset($app_list_strings[$listName]) && isset($app_list_strings[$listName][$rawValue])) {
                        // Found the corresponding label
                        $displayValue = $app_list_strings[$listName][$rawValue];
                        $htmlValue = $displayValue;
                    }
                }

                // 7. Handle HTML formatting (e.g., newlines in textareas)
                // 'text' type fields (textareas) store newlines as \n.
                // In HTML, these must be converted to <br>.
                if (isset($def['type']) && $def['type'] == 'text') {
                    $htmlValue = nl2br((string) $htmlValue);
                }

                // Ensure values are scalar (not null, array, or object) before replacing
                if (!is_scalar($displayValue)) {
                    $displayValue = '';
                }

                if (!is_scalar($htmlValue)) {
                    $htmlValue = '';
                }

                // 8. Add to the collection arrays (instead of replacing immediately)
                $replacements_text[$placeholder] = "$@@{$displayValue}";
                $replacements_html[$placeholder] = "$@@{$htmlValue}";
            }
        }

        // 9. Collect Special and Global Substitutions

        // 9.1. Static substitutions (like $sugarurl)
        $specialSubstitutions = [
            '$sugarurl' => $sugar_config['site_url'] ?? '',
            // You can add more here:
            // '$company_name' => $sugar_config['company_name'] ?? 'My Company',
        ];

        foreach ($specialSubstitutions as $placeholder => $value) {
            if (!is_scalar($value)) {
                $value = '';
            }

            $replacements_text[$placeholder] = $value;
            $replacements_html[$placeholder] = $value;
        }

        // --- SUBSTITUTION APPLICATION ---

        // 9. Sort the replacement arrays by key length (descending)
        // This is CRUCIAL. It ensures that "$contact_user_first_name" is replaced
        // BEFORE "$contact_user_", preventing partial, broken replacements.
        uksort($replacements_text, function ($a, $b) {return strlen($b) - strlen($a);});
        uksort($replacements_html, function ($a, $b) {return strlen($b) - strlen($a);});

        // 10. Perform the replacement (only once)
        // str_replace can take arrays of keys and values.
        $parsedTemplate['subject'] = str_replace(
            array_keys($replacements_text),
            array_values($replacements_text),
            $parsedTemplate['subject']
        );

        $parsedTemplate['body'] = str_replace(
            array_keys($replacements_text),
            array_values($replacements_text),
            $parsedTemplate['body']
        );

        $parsedTemplate['body_html'] = str_replace(
            array_keys($replacements_html),
            array_values($replacements_html),
            $parsedTemplate['body_html']
        );

        // 11. Final Cleanup: Remove any unreplaced variables
        // This regex looks for any variable starting with $
        // followed by letters/numbers/underscores (e.g., $contact_name, $quotes_total, $rare_variable)
        // and replaces it with an empty string.
        // We use [a-zA-Z_] at the start to avoid matching currency values like $500.

        $final_cleanup_pattern = '/\$[a-zA-Z_][a-zA-Z0-9_]*/';

        $parsedTemplate['subject'] = preg_replace($final_cleanup_pattern, '', $parsedTemplate['subject']);
        $parsedTemplate['body'] = preg_replace($final_cleanup_pattern, '', $parsedTemplate['body']);
        $parsedTemplate['body_html'] = preg_replace($final_cleanup_pattern, '', $parsedTemplate['body_html']);

        // Remove intermediate markup ($@@)
        $parsedTemplate['subject'] = str_replace('$@@', '', $parsedTemplate['subject']);
        $parsedTemplate['body'] = str_replace('$@@', '', $parsedTemplate['body']);
        $parsedTemplate['body_html'] = str_replace('$@@', '', $parsedTemplate['body_html']);

        // 12. Return the parsed and cleaned array
        return $parsedTemplate;
    }

    /**
     * Encodes an array (normally the current $_REQUEST) to be used as the value of the hidden
     * field "current_query_by_page" (mass updates, target lists, popups, bulk document
     * generation, ...).
     *
     * Before JSON-encoding the data, any HTML entities in string values are decoded, and double
     * quotes, single quotes, ampersands and tags are escaped as \u0022, \u0027, \u0026 and \u003C/\u003E
     * (JSON_HEX_* flags). This way the JSON structure always remains valid after any
     * html_entity_decode() / browser decoding round trip, which prevents broken payloads from
     * generating an empty WHERE clause and triggering fully uncontrolled mass updates when any
     * filter contains quotes or other special characters.
     *
     * The returned JSON string still needs to be transport encoded by the caller, depending on
     * where it will be placed: htmlentities() for HTML attributes (hidden inputs and hidden
     * fields built through JS templates) or base64_encode() when the consumer decodes it itself.
     *
     * @param array $requestArray Array to encode. Defaults to the current $_REQUEST.
     * @return string JSON string with special characters escaped as \u0022, \u0027, \u0026, \u003C/\u003E.
     */
    public static function encodeCurrentQueryByPage($requestArray = null)
    {
        if ($requestArray === null) {
            $requestArray = $_REQUEST;
        }

        $decodedArray = $requestArray;
        array_walk_recursive($decodedArray, function (&$value) {
            if (is_string($value)) {
                $value = html_entity_decode($value, ENT_QUOTES | ENT_HTML5, 'UTF-8');
            }
        });

        return json_encode($decodedArray, JSON_HEX_APOS | JSON_HEX_QUOT | JSON_HEX_AMP | JSON_HEX_TAG);
    }

    /**
     * Output a JSON response with proper HTTP headers and buffer cleanup.
     *
     * Ensures clean output by ending and clearing any existing output buffers
     * before sending the JSON response.
     *
     * @param string $status Status of the operation ('success' or 'error')
     * @param array  $errors Array of error messages
     * @param array  $infos  Array of information/success messages
     *
     * @return void
     */
    public static function outputJsonResponse($status, $errors, $infos) {
        @ob_end_clean();
        ob_start();
        ob_clean();
        header('Content-Type: application/json');
        echo json_encode([
            'status' => $status,
            'errors' => $errors,
            'infos' => $infos,
        ]);
        ob_flush();
    }

    /**
     * Refresh OPcache shared memory entries for regenerated PHP files
     *
     * On hosts where opcache.restrict_api blocks opcache_invalidate(), PHP files that are
     * regenerated by the application (e.g. custom/modules/<module>/logic_hooks.php) keep
     * being served by OPcache in their old compiled version until opcache.revalidate_freq
     * elapses, causing stale behaviour for every FPM worker during that window.
     *
     * This method removes that stale window:
     * 1. If the OPcache API is not path-restricted, it calls opcache_invalidate() directly
     *    on every file (invalidating never re-executes anything, so it is safe for any file).
     * 2. Otherwise it applies the "self-refresh" pattern for re-executable files: temporarily
     *    set opcache.validate_timestamps=1, opcache.revalidate_freq=0 and
     *    opcache.file_update_protection=0 for the current process and include() each file,
     *    which forces this worker to recompile the fresh content and update the shared
     *    OPcache entry, so all other workers immediately serve the new version.
     *    OPcache validates changes comparing mtimes with 1-second resolution, so each
     *    file's mtime is forced to time()+1 before including it: a file rewritten twice
     *    within the same second (e.g. remove+add flows) would otherwise validate as
     *    "unchanged" and the stale entry would be served indefinitely, not just for
     *    revalidate_freq.
     *
     * IMPORTANT: $files must only contain files that are safe to re-execute within the
     * current request (files that only set variables/arrays, like logic_hooks.php or
     * language files). Files declaring classes/functions MUST be passed through
     * $invalidateOnlyFiles: they are only refreshed via the native API (never
     * re-included), and on restricted hosts they cannot be refreshed (logged as error).
     *
     * Failures are logged (LoggerManager) and reflected in the return value, so callers
     * can detect hosts where OPcache entries cannot be refreshed from PHP at all
     * (php_admin_value locks, opcache.validate_timestamps=0 with restricted API, etc.).
     *
     * @param string|array $files Path(s) of regenerated PHP files safe to re-execute (variables/arrays only)
     * @param string|array $invalidateOnlyFiles Optional path(s) of regenerated PHP files that must NOT be re-executed (classes/functions)
     * @return bool True if every requested file was refreshed (or nothing was needed); false otherwise
     */
    public static function refreshPhpFilesInOpcache($files, $invalidateOnlyFiles = array())
    {
        if (!is_array($files)) {
            $files = array($files);
        }
        if (!is_array($invalidateOnlyFiles)) {
            $invalidateOnlyFiles = array($invalidateOnlyFiles);
        }

        // Resolve to real paths and drop nonexistent files
        $files = array_values(array_unique(array_filter(array_map('realpath', $files), 'is_file')));
        $invalidateOnlyFiles = array_values(array_unique(array_filter(array_map('realpath', $invalidateOnlyFiles), 'is_file')));
        if (empty($files) && empty($invalidateOnlyFiles)) {
            return true; // nothing to do
        }

        // Nothing can be done without Zend OPcache
        if (!extension_loaded('Zend OPcache') || !function_exists('opcache_invalidate')) {
            LoggerManager::getLogger()->error('SticUtils::refreshPhpFilesInOpcache: Zend OPcache is not loaded; regenerated PHP files may be served stale');
            return false;
        }

        // First try the native API, when it is not restricted by opcache.restrict_api.
        // Invalidating never re-executes anything, so it is safe for every file.
        $restrictApi = ini_get('opcache.restrict_api');
        $apiAvailable = !$restrictApi || strpos(__FILE__, $restrictApi) === 0;
        if ($apiAvailable) {
            $allInvalidated = true;
            foreach (array_merge($files, $invalidateOnlyFiles) as $file) {
                if (!@opcache_invalidate($file, true)) {
                    $allInvalidated = false;
                }
            }
            if ($allInvalidated) {
                return true;
            }
        }

        // Fallback: "self-refresh" pattern (include is not blocked by opcache.restrict_api).
        // Only valid for the re-executable files: class/function-declaring files cannot be
        // refreshed this way (re-including them would be a fatal "cannot redeclare").
        if (!empty($invalidateOnlyFiles)) {
            LoggerManager::getLogger()->error(
                'SticUtils::refreshPhpFilesInOpcache: cannot refresh class/function-declaring files when opcache.restrict_api blocks the API: '
                . implode(', ', $invalidateOnlyFiles)
            );
        }
        if (empty($files)) {
            return false;
        }

        // Without timestamp validation OPcache never re-checks files, so the fallback is
        // useless. NOTE: this directive cannot be enabled at runtime (ini_set changes the
        // ini value but not the behaviour, unlike revalidate_freq), verified empirically.
        if (!(bool)ini_get('opcache.validate_timestamps')) {
            LoggerManager::getLogger()->error('SticUtils::refreshPhpFilesInOpcache: opcache.validate_timestamps=0 and the OPcache API is blocked; entries can only be refreshed with opcache_reset/FPM restart or host configuration');
            return false;
        }
        $oldFreq = @ini_set('opcache.revalidate_freq', '0');
        if ($oldFreq === false) {
            LoggerManager::getLogger()->error("SticUtils::refreshPhpFilesInOpcache: ini_set('opcache.revalidate_freq') is blocked (php_admin_value?); OPcache entries cannot be refreshed from PHP");
            return false;
        }
        $oldProtection = @ini_set('opcache.file_update_protection', '0');
        try {
            foreach ($files as $file) {
                // Force a mtime different from the currently cached entry timestamp: OPcache
                // validates changes with 1-second mtime resolution, so a file rewritten within
                // the same second its previous version was cached would validate as "unchanged"
                // and the stale entry would be served indefinitely
                @touch($file, time() + 1);
                include $file;
            }
        } finally {
            @ini_set('opcache.revalidate_freq', $oldFreq);
            if ($oldProtection !== false) {
                @ini_set('opcache.file_update_protection', $oldProtection);
            }
        }
        return empty($invalidateOnlyFiles);
    }
}
