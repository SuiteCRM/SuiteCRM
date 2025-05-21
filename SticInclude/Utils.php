<?php
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

}
