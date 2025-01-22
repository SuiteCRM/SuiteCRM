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
 * Localization manager
 * @api
 */
class Localization
{
    public $availableCharsets = array(
        'BIG-5',        //Taiwan and Hong Kong
        /*'CP866'			  // ms-dos Cyrillic */
        /*'CP949'			  //Microsoft Korean */
        'CP1251',       //MS Cyrillic
        'CP1252',       //MS Western European & US
        'EUC-CN',       //Simplified Chinese GB2312
        'EUC-JP',       //Unix Japanese
        'EUC-KR',       //Korean
        'EUC-TW',       //Taiwanese
        'ISO-2022-JP',  //Japanese
        'ISO-2022-KR',  //Korean
        'ISO-8859-1',   //Western European and US
        'ISO-8859-2',   //Central and Eastern European
        'ISO-8859-3',   //Latin 3
        'ISO-8859-4',   //Latin 4
        'ISO-8859-5',   //Cyrillic
        'ISO-8859-6',   //Arabic
        'ISO-8859-7',   //Greek
        'ISO-8859-8',   //Hebrew
        'ISO-8859-9',   //Latin 5
        'ISO-8859-10',  //Latin 6
        'ISO-8859-13',  //Latin 7
        'ISO-8859-14',  //Latin 8
        'ISO-8859-15',  //Latin 9
        'KOI8-R',       //Cyrillic Russian
        'KOI8-U',       //Cyrillic Ukranian
        'SJIS',         //MS Japanese
        'UTF-8',        //UTF-8
        'UTF-16LE',     //UTF-16LE
    );
    public $localeNameFormat;
    public $localeNameFormatDefault;
    public $default_export_charset = 'UTF-8';
    public $default_email_charset = 'UTF-8';
    public $currencies = array(); // array loaded with current currencies
    public $invalidNameFormatUpgradeFilename = 'upgradeInvalidLocaleNameFormat.php';
    /* Charset mappings for iconv */
    public $iconvCharsetMap = array(
        'KS_C_5601-1987' => 'CP949',
        'ISO-8859-8-I' => 'ISO-8859-8'
    );

    /**
     * sole constructor
     */
    public function __construct()
    {
        global $sugar_config;
        $this->localeNameFormatDefault = empty($sugar_config['locale_name_format_default']) ? 's f l' : $sugar_config['default_name_format'];
        $this->loadCurrencies();
    }



    /**
     * returns an array of Sugar Config defaults that are determined by locale settings
     * @return array
     */
    public function getLocaleConfigDefaults()
    {
        $coreDefaults = array(
            'datef'                                    => 'm/d/Y',
            'timef'                                    => 'H:i',
            'default_currency_significant_digits'      => 2,
            'default_currency_symbol'                  => '$',
            'default_export_charset'                   => $this->default_export_charset,
            'default_locale_name_format'               => 's f l',
            'name_formats'                             => array(
                's f l'  => 's f l',
                'f l'    => 'f l',
                's l'    => 's l',
                'l, s f' => 'l, s f',
                'l, f'   => 'l, f',
                's l, f' => 's l, f',
                'l s f'  => 'l s f',
                'l f s'  => 'l f s'
            ),
            'default_number_grouping_seperator'        => ',',
            'default_decimal_seperator'                => '.',
            'export_delimiter'                         => ',',
            'default_email_charset'                    => $this->default_email_charset,
        );

        return $coreDefaults;
    }

    /**
     * abstraction of precedence
     * @param string prefName Name of preference to retrieve based on overrides
     * @param object user User in focus, default null (current_user)
     * @return string pref Most significant preference
     */
    public function getPrecedentPreference($prefName, $user = null, $sugarConfigPrefName = '')
    {
        global $current_user;
        global $sugar_config;

        $userPref = '';
        $coreDefaults = $this->getLocaleConfigDefaults();
        $pref = isset($coreDefaults[$prefName]) ? $coreDefaults[$prefName] : ''; // defaults, even before config.php

        if ($user != null) {
            $userPref = $user->getPreference($prefName);
        } elseif (!empty($current_user)) {
            $userPref = $current_user->getPreference($prefName);
        }
        // Bug 39171 - If we are asking for default_email_charset, check in emailSettings['defaultOutboundCharset'] as well
        if ($prefName == 'default_email_charset') {
            if ($user != null) {
                $emailSettings = $user->getPreference('emailSettings', 'Emails');
            } elseif (!empty($current_user)) {
                $emailSettings = $current_user->getPreference('emailSettings', 'Emails');
            }
            if (isset($emailSettings['defaultOutboundCharset'])) {
                $userPref = $emailSettings['defaultOutboundCharset'];
            }
        }

        // set fallback defaults defined in this class
        if (isset($this->$prefName)) {
            $pref = $this->$prefName;
        }
        //rrs: 33086 - give the ability to pass in the preference name as stored in $sugar_config.
        if (!empty($sugarConfigPrefName)) {
            $prefName = $sugarConfigPrefName;
        }
        // cn: 9549 empty() call on a value of 0 (0 significant digits) resulted in a false-positive.  changing to "isset()"
        $pref = (!isset($sugar_config[$prefName]) || (empty($sugar_config[$prefName]) && $sugar_config[$prefName] !== '0')) ? $pref : $sugar_config[$prefName];
        $pref = (empty($userPref) && $userPref !== '0') ? $pref : $userPref;
        return $pref;
    }

    ///////////////////////////////////////////////////////////////////////////
    ////	CURRENCY HANDLING
    /**
     * wrapper for whatever currency system we implement
     */
    public function loadCurrencies()
    {
        // doing it dirty here
        global $db;
        global $sugar_config;

        if (empty($db)) {
            return array();
        }

        $load = sugar_cache_retrieve('currency_list');
        if (!is_array($load)) {
            // load default from config.php
            $this->currencies['-99'] = array(
                'name' => $sugar_config['default_currency_name'],
                'symbol' => $sugar_config['default_currency_symbol'],
                'conversion_rate' => 1
            );

            $q = "SELECT id, name, symbol, conversion_rate FROM currencies WHERE status = 'Active' and deleted = 0";
            $r = $db->query($q);

            while ($a = $db->fetchByAssoc($r)) {
                $load = array();
                $load['name'] = $a['name'];
                $load['symbol'] = $a['symbol'];
                $load['conversion_rate'] = $a['conversion_rate'];

                $this->currencies[$a['id']] = $load;
            }
            sugar_cache_put('currency_list', $this->currencies);
        } else {
            $this->currencies = $load;
        }
    }

    /**
     * getter for currencies array
     * @return array $this->currencies returns array( id => array(name => X, etc
     */
    public function getCurrencies()
    {
        return $this->currencies;
    }

    /**
     * retrieves default OOTB currencies for sugar_config and installer.
     * @return array ret Array of default currencies keyed by ISO4217 code
     */
    public function getDefaultCurrencies()
    {
        $ret = array(
            'AUD' => array(
                'name'    => 'Australian Dollars',
                'iso4217' => 'AUD',
                'symbol'  => '$'
            ),
            'BRL' => array(
                'name'    => 'Brazilian Reais',
                'iso4217' => 'BRL',
                'symbol'  => 'R$'
            ),
            'GBP' => array(
                'name'    => 'British Pounds',
                'iso4217' => 'GBP',
                'symbol'  => '£'
            ),
            'CAD' => array(
                'name'    => 'Canadian Dollars',
                'iso4217' => 'CAD',
                'symbol'  => '$'
            ),
            'CNY' => array(
                'name'    => 'Chinese Yuan',
                'iso4217' => 'CNY',
                'symbol'  => '￥'
            ),
            'EUR' => array(
                'name'    => 'Euro',
                'iso4217' => 'EUR',
                'symbol'  => '€'
            ),
            'HKD' => array(
                'name'    => 'Hong Kong Dollars',
                'iso4217' => 'HKD',
                'symbol'  => '$'
            ),
            'INR' => array(
                'name'    => 'Indian Rupees',
                'iso4217' => 'INR',
                'symbol'  => '₨'
            ),
            'KRW' => array(
                'name'    => 'Korean Won',
                'iso4217' => 'KRW',
                'symbol'  => '₩'
            ),
            'YEN' => array(
                'name'    => 'Japanese Yen',
                'iso4217' => 'JPY',
                'symbol'  => '¥'
            ),
            'MXN' => array(
                'name'    => 'Mexican Pesos',
                'iso4217' => 'MXN',
                'symbol'  => '$'
            ),
            'SGD' => array(
                'name'    => 'Singaporean Dollars',
                'iso4217' => 'SGD',
                'symbol'  => '$'
            ),
            'CHF' => array(
                'name'    => 'Swiss Franc',
                'iso4217' => 'CHF',
                'symbol'  => 'SFr.'
            ),
            'THB' => array(
                'name'    => 'Thai Baht',
                'iso4217' => 'THB',
                'symbol'  => '฿'
            ),
            'USD' => array(
                'name'    => 'US Dollars',
                'iso4217' => 'USD',
                'symbol'  => '$'
            ),
        );

        return $ret;
    }
    ////	END CURRENCY HANDLING
    ///////////////////////////////////////////////////////////////////////////


    ///////////////////////////////////////////////////////////////////////////
    ////	CHARSET TRANSLATION
    /**
     * returns a mod|app_strings array in the target charset
     * @param array strings $mod_string, et.al.
     * @param string charset Target charset
     * @return array Translated string pack
     */
    public function translateStringPack($strings, $charset)
    {
        // handle recursive
        foreach ($strings as $k => $v) {
            if (is_array($v)) {
                $strings[$k] = $this->translateStringPack($v, $charset);
            } else {
                $strings[$k] = $this->translateCharset($v, 'UTF-8', $charset);
            }
        }
        ksort($strings);
        return $strings;
    }

    /**
     * translates the passed variable for email sending (export)
     * @param	mixed the var (array or string) to translate
     * @return	mixed the translated variable
     */
    public function translateForEmail($var)
    {
        if (is_array($var)) {
            foreach ($var as $k => $v) {
                $var[$k] = $this->translateForEmail($v);
            }
            return $var;
        } elseif (!empty($var)) {
            return $this->translateCharset($var, 'UTF-8', $this->getOutboundEmailCharset());
        }
    }

    /**
     * prepares a bean for export by translating any text fields into the export
     * character set
     * @param bean object A SugarBean
     * @return bean object The bean with translated strings
     */
    public function prepBeanForExport($bean)
    {
        foreach ($bean->field_defs as $k => $field) {
            if (is_string($bean->$k)) {
                // $bean->$k = $this->translateCharset($bean->$k, 'UTF-8', $this->getExportCharset());
            } else {
                $bean->$k = '';
            }
        }

        return $bean;
    }

    /**
     * translates a character set from one encoding to another encoding
     * @param string string the string to be translated
     * @param string fromCharset the charset the string is currently in
     * @param string toCharset the charset to translate into (defaults to UTF-8)
     * @param bool   forceIconv force using the iconv library instead of mb_string
     * @return string the translated string
     */
    public function translateCharset($string, $fromCharset, $toCharset = 'UTF-8', $forceIconv = false)
    {
        $GLOBALS['log']->debug("Localization: translating [{$string}] from {$fromCharset} into {$toCharset}");

        // Bug #35413 Function has to use iconv if $fromCharset is not in mb_list_encodings
        $isMb = function_exists('mb_convert_encoding') && !$forceIconv;
        $isIconv = function_exists('iconv');
        if ($isMb == true) {
            $fromCharset = strtoupper($fromCharset);
            $listEncodings = mb_list_encodings();
            $isFound = false;
            foreach ($listEncodings as $encoding) {
                if (strtoupper($encoding) == $fromCharset) {
                    $isFound = true;
                    break;
                }
            }
            $isMb = $isFound;
        }

        if ($isMb) {
            return mb_convert_encoding($string, $toCharset, $fromCharset);
        } elseif ($isIconv) {
            $newFromCharset = $fromCharset;
            if (isset($this->iconvCharsetMap[$fromCharset])) {
                $newFromCharset = $this->iconvCharsetMap[$fromCharset];
                $GLOBALS['log']->debug("Localization: iconv using charset {$newFromCharset} instead of {$fromCharset}");
            }
            $newToCharset = $toCharset;
            if (isset($this->iconvCharsetMap[$toCharset])) {
                $newToCharset = $this->iconvCharsetMap[$toCharset];
                $GLOBALS['log']->debug("Localization: iconv using charset {$newToCharset} instead of {$toCharset}");
            }
            return iconv($newFromCharset, $newToCharset, $string);
        }
        return $string;
    }

    /**
     * Prefixes the input with a BOM.
     *
     * @param string $string The string to add a BOM to
     * @param string $fromCharset The charset of the input string
     * @return string The input string including a BOM
     * @throws UnexpectedValueException in case the encoding isn't supported
     */
    public function addBOM($string, $fromCharset)
    {
        $charset  = $this->normalizeCharset($fromCharset);
        if ($charset === 'utf8') {
            return "\xef\xbb\xbf" . $string;
        } else if ($charset === 'utf16le') {
            return "\xff\xfe" . $string;
        } else if ($charset === 'utf16be') {
            return "\xfe\xff" . $string;
        }
        throw new UnexpectedValueException('Encoding not supported: ' . $fromCharset);
    }

    /**
     * translates a character set from one to another, and the into MIME-header friendly format
     */
    public function translateCharsetMIME($string, $fromCharset, $toCharset = 'UTF-8', $encoding = "Q")
    {
        $previousEncoding = mb_internal_encoding();
        mb_internal_encoding($fromCharset);
        $result = mb_encode_mimeheader($string, $toCharset, $encoding);
        mb_internal_encoding($previousEncoding);
        return $result;
    }

    public function normalizeCharset($charset)
    {
        $charset = strtolower(preg_replace("/[\-\_]*/", "", $charset));
        return $charset;
    }

    /**
     * returns an array of charsets with keys for available translations; appropriate for get_select_options_with_id()
     */
    public function getCharsetSelect()
    {
        //jc:12293 - the "labels" or "human-readable" representations of the various charsets
        //should be translatable
        $translated = array();
        foreach ($this->availableCharsets as $key) {
            //$translated[$key] = translate($value);
            $translated[$key] = translate($key);
        }

        return $translated;
        //end:12293
    }

    /**
     * returns the charset preferred in descending order: User, Sugar Config, DEFAULT
     * @param string charset to override ALL, pass a valid charset here
     * @return string charset the chosen character set
     */
    public function getExportCharset($charset = '', $user = null)
    {
        $charset = $this->getPrecedentPreference('default_export_charset', $user);
        return $charset;
    }

    /**
     * returns the charset preferred in descending order: User, Sugar Config, DEFAULT
     * @return string charset the chosen character set
     */
    public function getOutboundEmailCharset($user = null)
    {
        $charset = $this->getPrecedentPreference('default_email_charset', $user);
        return $charset;
    }
    ////	END CHARSET TRANSLATION
    ///////////////////////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////////////////////
    ////	NUMBER DISPLAY FORMATTING CODE
    public function getDecimalSeparator($user = null)
    {
        // Bug50887 this is purposefully misspelled as ..._separator to match the way it's defined throughout the app.
        $dec = $this->getPrecedentPreference('default_decimal_seperator', $user);
        return $dec;
    }

    public function getNumberGroupingSeparator($user = null)
    {
        $sep = $this->getPrecedentPreference('default_number_grouping_seperator', $user);
        return $sep;
    }

    public function getPrecision($user = null)
    {
        $precision = $this->getPrecedentPreference('default_currency_significant_digits', $user);
        return $precision;
    }

    public function getCurrencySymbol($user = null)
    {
        $dec = $this->getPrecedentPreference('default_currency_symbol', $user);
        return $dec;
    }

    /**
     * returns a number formatted by user preference or system default
     * @param string number Number to be formatted and returned
     * @param string currencySymbol Currency symbol if override is necessary
     * @param bool is_currency Flag to also return the currency symbol
     * @return string Formatted number
     */
    public function getLocaleFormattedNumber($number, $currencySymbol = '', $is_currency = true, $user = null)
    {
        $fnum = $number;
        $majorDigits = '';
        $minorDigits = '';
        $dec = $this->getDecimalSeparator($user);
        $thou = $this->getNumberGroupingSeparator($user);
        $precision = $this->getPrecision($user);
        $symbol = empty($currencySymbol) ? $this->getCurrencySymbol($user) : $currencySymbol;

        $exNum = explode($dec, $number);
        // handle grouping
        if (is_array($exNum) && count($exNum) > 0) {
            if (strlen($exNum[0]) > 3) {
                $offset = strlen($exNum[0]) % 3;
                if ($offset > 0) {
                    for ($i = 0; $i < $offset; $i++) {
                        $majorDigits .= $exNum[0][$i];
                    }
                }

                $tic = 0;
                for ($i = $offset; $i < strlen($exNum[0]); $i++) {
                    if ($tic % 3 == 0 && $i != 0) {
                        $majorDigits .= $thou; // add separator
                    }

                    $majorDigits .= $exNum[0][$i];
                    $tic++;
                }
            } else {
                $majorDigits = $exNum[0]; // no formatting needed
            }
            $fnum = $majorDigits;
        }

        // handle decimals
        if ($precision > 0) { // we toss the minor digits otherwise
            if (is_array($exNum) && isset($exNum[1])) { }
        }

        if ($is_currency) {
            $fnum = $symbol . $fnum;
        }
        return $fnum;
    }

    /**
     * returns Javascript to format numbers and currency for ***DISPLAY***
     */
    public function getNumberJs()
    {
        $out = <<<JAVASCRIPT

            var exampleDigits = '123456789.000000';

            // round parameter can be negative for decimal, precision has to be postive
            function formatNumber(n, sep, dec, precision) {
                var majorDigits;
                var minorDigits;
                var formattedMajor = '';
                var formattedMinor = '';

                var nArray = n.split('.');
                majorDigits = nArray[0];
                if (nArray.length < 2) {
                    minorDigits = 0;
                } else {
                    minorDigits = nArray[1];
                }

                // handle grouping
                if (sep.length > 0) {
                    var strlength = majorDigits.length;

                    if (strlength > 3) {
                        var offset = strlength % 3; // find how many to lead off by

                        for (j = 0; j < offset; j++) {
                            formattedMajor += majorDigits[j];
                        }

                        tic = 0;
                        for (i = offset; i < strlength; i++) {
                            if (tic % 3 == 0 && i != 0)
                                formattedMajor += sep;

                            formattedMajor += majorDigits.substr(i,1);
                            tic++;
                        }
                    }
                } else {
                    formattedMajor = majorDigits; // no grouping marker
                }

                // handle decimal precision
                if (precision > 0) {
                    for (i = 0; i < precision; i++) {
                        if (minorDigits[i] != undefined) {
                            formattedMinor += minorDigits[i];
                        } else {
                            formattedMinor += '0';
                        }
                    }
                } else {
                    // we're just returning the major digits, no decimal marker
                    dec = ''; // just in case
                }

                return formattedMajor + dec + formattedMinor;
            }

            function setSigDigits() {
                var sym = document.getElementById('symbol').value;
                var thou = document.getElementById('default_number_grouping_seperator').value;
                var dec = document.getElementById('default_decimal_seperator').value;
                var precision = document.getElementById('sigDigits').value;
                //umber(n, num_grp_sep, dec_sep, round, precision)
                var newNumber = sym + formatNumber(exampleDigits, thou, dec, precision, precision);
                document.getElementById('sigDigitsExample').value = newNumber;
            }
JAVASCRIPT;
        return $out;
    }

    ////	END NUMBER DISPLAY FORMATTING CODE
    ///////////////////////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////////////////////
    ////	NAME DISPLAY FORMATTING CODE
    /**
     * get's the Name format macro string, preferring $current_user
     * @return string format Name Format macro for locale
     */
    public function getLocaleFormatMacro($user = null)
    {
        $returnFormat = $this->getPrecedentPreference('default_locale_name_format', $user);
        return $returnFormat;
    }

    /**
     * returns formatted name according to $current_user's locale settings
     *
     * @param string firstName
     * @param string lastName
     * @param string salutation
     * @param string title
     * @param string format If a particular format is desired, then pass this optional parameter as a simple string.
     *   sfl is "Salutation FirstName LastName", "l, f s" is "LastName[comma][space]FirstName[space]Salutation"
     * @param object user object
     * @param bool returnEmptyStringIfEmpty true if we should return back an empty string rather than a single space
     * when the formatted name would be blank
     * @return string formattedName
     */
    public function getLocaleFormattedName($firstName, $lastName, $salutationKey = '', $title = '', $format = "", $user = null, $returnEmptyStringIfEmpty = false)
    {
        global $current_user;
        global $app_list_strings;

        if ($user == null) {
            $user = $current_user;
        }

        $salutation = $salutationKey;
        if (!empty($salutationKey) && !empty($app_list_strings['salutation_dom'][$salutationKey])) {
            $salutation = (!empty($app_list_strings['salutation_dom'][$salutationKey]) ? $app_list_strings['salutation_dom'][$salutationKey] : $salutationKey);
        }

        //check to see if passed in variables are set, if so, then populate array with value,
        //if not, then populate array with blank ''
        $names = array();
        $names['f'] = (empty($firstName) && $firstName != 0) ? '' : $firstName;
        $names['l'] = (empty($lastName) && $lastName != 0) ? '' : $lastName;
        $names['s'] = (empty($salutation) && $salutation != 0) ? '' : $salutation;
        $names['t'] = (empty($title) && $title != 0) ? '' : $title;

        //Bug: 39936 - if all of the inputs are empty, then don't try to format the name.
        $allEmpty = true;
        foreach ($names as $key => $val) {
            if (!empty($val)) {
                $allEmpty = false;
                break;
            }
        }
        if ($allEmpty) {
            return $returnEmptyStringIfEmpty ? '' : ' ';
        }
        //end Bug: 39936

        if (empty($format)) {
            $this->localeNameFormat = $this->getLocaleFormatMacro($user);
        } else {
            $this->localeNameFormat = $format;
        }

        // parse localeNameFormat
        $formattedName = '';
        for ($i = 0; $i < strlen($this->localeNameFormat); $i++) {
            $formattedName .= array_key_exists($this->localeNameFormat[$i], $names) ? $names[$this->localeNameFormat[$i]] : $this->localeNameFormat[$i];
        }

        $formattedName = trim($formattedName);
        if (strlen($formattedName) == 0) {
            return $returnEmptyStringIfEmpty ? '' : ' ';
        }

        if (strpos($formattedName, ',', strlen($formattedName) - 1)) { // remove trailing commas
            $formattedName = substr($formattedName, 0, strlen($formattedName) - 1);
        }
        return trim($formattedName);
    }

    /**
     * outputs some simple Javascript to show a preview of Name format in "My Account" and "Admin->Localization"
     * @param string first First Name, use app_strings default if not specified
     * @param string last Last Name, use app_strings default if not specified
     * @param string salutation Saluation, use app_strings default if not specified
     * @return string some Javascript
     */
    public function getNameJs($first = '', $last = '', $salutation = '', $title = '')
    {
        global $app_strings;

        $salutation = !empty($salutation) ? $salutation : $app_strings['LBL_LOCALE_NAME_EXAMPLE_SALUTATION'];
        $first      = !empty($first) ? $first : $app_strings['LBL_LOCALE_NAME_EXAMPLE_FIRST'];
        $last       = !empty($last) ? $last : $app_strings['LBL_LOCALE_NAME_EXAMPLE_LAST'];
        $title      = !empty($title) ? $title : $app_strings['LBL_LOCALE_NAME_EXAMPLE_TITLE'];

        $ret = <<<JAVASCRIPT
        function setPreview() {
            format = document.getElementById('default_locale_name_format').value;
            field = document.getElementById('nameTarget');

            stuff = new Object();

            stuff['s'] = '{$salutation}';
            stuff['f'] = '{$first}';
            stuff['l'] = '{$last}';
            stuff['t'] = '{$title}';

            var name = '';
            for (i = 0; i < format.length; i++) {
                if (stuff[format.substr(i,1)] != undefined) {
                    name += stuff[format.substr(i,1)];
                } else {
                    name += format.substr(i,1);
                }
            }

            field.value = name;
        }
JAVASCRIPT;

        return $ret;
    }

    /**
     * Checks to see that the characters in $name_format are allowed:  s, f, l, space/tab or punctuation
     * @param $name_format
     * @return bool
     */
    public function isAllowedNameFormat($name_format)
    {
        // will result in a match as soon as a disallowed char is hit in $name_format
        $match = preg_match('/[^sfl[:punct:][:^alnum:]\s]/', $name_format);
        if ($match !== false && $match === 0) {
            return true;
        }
        return false;
    }

    /**
     * Checks to see if there was an invalid Name Format encountered during the upgrade
     * @return bool true if there was an invalid name, false if all went well.
     */
    public function invalidLocaleNameFormatUpgrade()
    {
        return file_exists($this->invalidNameFormatUpgradeFilename);
    }

    /**
     * Creates the file that is created when there is an invalid name format during an upgrade
     */
    public function createInvalidLocaleNameFormatUpgradeNotice()
    {
        $fh = fopen($this->invalidNameFormatUpgradeFilename, 'wb');
        fclose($fh);
    }

    /**
     * Removes the file that is created when there is an invalid name format during an upgrade
     */
    public function removeInvalidLocaleNameFormatUpgradeNotice()
    {
        if ($this->invalidLocaleNameFormatUpgrade()) {
            unlink($this->invalidNameFormatUpgradeFilename);
        }
    }

    /**
     * Creates dropdown items that have localized example names while filtering out invalid formats
     *
     * @param array un-prettied dropdown list
     * @return array array of dropdown options
     */
    public function getUsableLocaleNameOptions($options)
    {
        global $app_strings;

        $examples = array(
            's' => $app_strings['LBL_LOCALE_NAME_EXAMPLE_SALUTATION'],
            'f' => $app_strings['LBL_LOCALE_NAME_EXAMPLE_FIRST'],
            'l' => $app_strings['LBL_LOCALE_NAME_EXAMPLE_LAST']
        );
        $newOpts = array();
        foreach ($options as $key => $val) {
            if ($this->isAllowedNameFormat($key) && $this->isAllowedNameFormat($val)) {
                $newVal = '';
                $pieces = str_split($val);
                foreach ($pieces as $piece) {
                    if (isset($examples[$piece])) {
                        $newVal .= $examples[$piece];
                    } else {
                        $newVal .= $piece;
                    }
                }
                $newOpts[$key] = $newVal;
            }
        }
        return $newOpts;
    }
    ////	END NAME DISPLAY FORMATTING CODE
    ///////////////////////////////////////////////////////////////////////////

    /**
     * Attempts to detect the charset used in the string
     *
     * @param  $str string
     * @param $strict bool default false (use strict encoding?)
     * @return string
     */
    public function detectCharset($str, $strict = false)
    {
        if (function_exists('mb_convert_encoding')) {
            return mb_detect_encoding($str, 'ASCII,JIS,UTF-8,EUC-JP,SJIS,ISO-8859-1', $strict);
        }

        return false;
    }
}
