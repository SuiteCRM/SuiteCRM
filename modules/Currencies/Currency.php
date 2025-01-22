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

/*********************************************************************************

 ********************************************************************************/




/**
 * Currency.php
 * This class encapsulates the handling of currency conversions and
 * formatting in the SugarCRM application.
 *
 */
#[\AllowDynamicProperties]
class Currency extends SugarBean
{
    // Stored fields
    public $id;
    public $iso4217;
    public $name;
    public $status;
    public $conversion_rate;
    public $deleted;
    public $date_entered;
    public $date_modified;
    public $symbol;
    public $hide = '';
    public $unhide = '';
    public $field_name_map;

    public $table_name = "currencies";
    public $object_name = "Currency";
    public $module_dir = "Currencies";
    public $new_schema = true;

    public $disable_num_format = true;


    public function __construct()
    {
        parent::__construct();
        global $app_strings, $current_user, $sugar_config, $locale;
        $this->field_defs['hide'] = array('name'=>'hide', 'source'=>'non-db', 'type'=>'varchar','len'=>25);
        $this->field_defs['unhide'] = array('name'=>'unhide', 'source'=>'non-db', 'type'=>'varchar','len'=>25);
        $this->disable_row_level_security =true;
    }

    /**
     * convertToDollar
     * This method accepts a currency amount and converts it to the US Dollar amount
     *
     * @param $amount string|float The currency amount to convert to US Dollars
     * @param $precision int The rounding precision scale
     * @return float currency value in US Dollars from conversion
     */
    public function convertToDollar($amount, $precision = 6)
    {
        $amount = is_string($amount) ? (float)$amount : $amount;

        return $this->conversion_rate ? round(($amount / $this->conversion_rate), $precision) : 0;
    }

    /**
     * convertFromCollar
     * This method accepts a US Dollar amount and returns a currency amount
     * with the conversion rate applied to it.
     *
     * @param $amount string|float The currency amount in US Dollars
     * @param $precision int The rounding precision scale
     * @return float currency value from US Dollar conversion
     */
    public function convertFromDollar($amount, $precision = 6)
    {
        $amount = is_string($amount) ? (float)$amount : $amount;

        return round(($amount * $this->conversion_rate), $precision);
    }

    /**
     * getDefaultCurrencyName
     *
     * Returns the default currency name as defined in application
     * @return String value of default currency name
     */
    public function getDefaultCurrencyName()
    {
        global $sugar_config;
        return $sugar_config['default_currency_name'];
    }

    /**
     * getDefaultCurrencySymbol
     *
     * Returns the default currency symobol in application
     * @return String value of default currency symbol(e.g. $)
     */
    public function getDefaultCurrencySymbol()
    {
        global $sugar_config;
        return $sugar_config['default_currency_symbol'];
    }

    /**
     * getDefaultISO4217
     *
     * Returns the default ISO 4217 standard currency code value
     * @return String value for the ISO 4217 standard code(e.g. EUR)
     */
    public function getDefaultISO4217()
    {
        global $sugar_config;
        return $sugar_config['default_currency_iso4217'];
    }

    /**
     * retrieveIDBySmbol
     *
     * Returns the id value for given currency symbol in Currencies table
     * and currency entry for symbol is not set to deleted.
     *
     * @param $symbol Symbol value
     * @return String id value for symbol defined in Currencies table, blank String value
     *         if none found
     */
    public function retrieveIDBySymbol($symbol)
    {
        $query = "SELECT id FROM currencies WHERE symbol='$symbol' AND deleted=0;";
        $result = $this->db->query($query);
        if ($result) {
            $row = $this->db->fetchByAssoc($result);
            if ($row) {
                return $row['id'];
            }
        }

        return '';
    }

    public function list_view_parse_additional_sections(&$list_form)
    {
        global $isMerge;

        if (isset($isMerge) && $isMerge && $this->id != '-99') {
            $list_form->assign('PREROW', '<input name="mergecur[]" type="checkbox" value="'.$this->id.'">');
        }
        return $list_form;
    }

    public function retrieve_id_by_name($name)
    {
        $nameQuoted = $this->db->quote($name);
        $query = "select id from currencies where name='$nameQuoted' and deleted=0;";
        $result = $this->db->query($query);
        if ($result) {
            $row = $this->db->fetchByAssoc($result);
            if ($row) {
                return $row['id'];
            }
        }
        return '';
    }

    public function retrieve($id = -99, $encode = true, $deleted = true)
    {
        if ($id == '-99') {
            $this->name = 	$this->getDefaultCurrencyName();
            $this->symbol = $this->getDefaultCurrencySymbol();
            $this->id = '-99';
            $this->conversion_rate = 1;
            $this->iso4217 = $this->getDefaultISO4217();
            $this->deleted = 0;
            $this->status = 'Active';
            $this->hide = '<!--';
            $this->unhide = '-->';
        } else {
            parent::retrieve($id, $encode, $deleted);
        }
        if (!isset($this->name) || $this->deleted == 1) {
            $this->name = 	$this->getDefaultCurrencyName();
            $this->symbol = $this->getDefaultCurrencySymbol();
            $this->conversion_rate = 1;
            $this->iso4217 = $this->getDefaultISO4217();
            $this->id = '-99';
            $this->deleted = 0;
            $this->status = 'Active';
            $this->hide = '<!--';
            $this->unhide = '-->';
        }
        return $this;
    }

    /**
     * Method for returning the currency symbol, must return chr(2) for the € symbol
     * to display correctly in pdfs
     * Parameters:
     * 	none
     * Returns:
     * 	$symbol otherwise chr(2) for euro symbol
     */
    public function getPdfCurrencySymbol()
    {
        if ($this->symbol == '&#8364;' || $this->symbol == '€') {
            return chr(2);
        }
        return $this->symbol;
    }
    public function get_list_view_data()
    {
        $this->conversion_rate = format_number($this->conversion_rate, 10, 10);
        $data = parent::get_list_view_data();
        return $data;
    }
    public function save($check_notify = false)
    {
        sugar_cache_clear('currency_list');
        return parent::save($check_notify);
    }
} // end currency class

/**
 * currency_format_number
 *
 * This method is a wrapper designed exclusively for formatting currency values
 * with the assumption that the method caller wants a currency formatted value
 * matching their user preferences(if set) or the system configuration defaults
 *(if user preferences are not defined).
 *
 * @param $amount The amount to be formatted
 * @param $params Optional parameters(see @format_number)
 * @return String representation of amount with formatting applied
 */
function currency_format_number($amount, $params = array())
{
    global $locale;
    if (isset($params['round']) && is_int($params['round'])) {
        $real_round = $params['round'];
    } else {
        $real_round = $locale->getPrecedentPreference('default_currency_significant_digits');
    }
    if (isset($params['decimals']) && is_int($params['decimals'])) {
        $real_decimals = $params['decimals'];
    } else {
        $real_decimals = $locale->getPrecedentPreference('default_currency_significant_digits');
    }
    $real_round = $real_round == '' ? 0 : $real_round;
    $real_decimals = $real_decimals == '' ? 0 : $real_decimals;

    $showCurrencySymbol = $locale->getPrecedentPreference('default_currency_symbol') != '' ? true : false;
    if ($showCurrencySymbol && !isset($params['currency_symbol'])) {
        $params["currency_symbol"] = true;
    }
    return format_number($amount, $real_round, $real_decimals, $params);
}

/**
 * format_number(deprecated)
 *
 * This method accepts an amount and formats it given the user's preferences.
 * Should the values set in the user preferences be invalid then it will
 * apply the system wide Sugar configuration values.  Calls to
 * getPrecendentPreference() method in Localization.php are made that
 * handle this logic.
 *
 * Going forward with Sugar 4.5.0e+ implementations, users of this class should
 * simple call this function with $amount parameter and leave it to the
 * class to locate and apply the appropriate formatting.
 *
 * One of the problems is that there is considerable legacy code that is using
 * this method for non currency formatting.  In other words, the format_number
 * method may be called to just display a number like 1,000 formatted appropriately.
 *
 * Also, issues about responsibilities arise.  Currently the callers of this function
 * are responsible for passing in the appropriate decimal and number rounding digits
 * as well as parameters to control displaying the currency symbol or not.
 *
 * @param $amount float|string The currency amount to apply formatting to
 * @param $round Integer value for number of places to round to
 * @param $decimals Integer value for number of decimals to round to
 * @param $params array of additional parameter values
 *
 *
 * The following are passed in as an array of params:
 *        boolean $params['currency_symbol'] - true to display currency symbol
 *        boolean $params['convert'] - true to convert from USD dollar
 *        boolean $params['percentage'] - true to display % sign
 *        boolean $params['symbol_space'] - true to have space between currency symbol and amount
 *        String  $params['symbol_override'] - string to over default currency symbol
 *        String  $params['type'] - pass in 'pdf' for pdf currency symbol conversion
 *        String  $params['currency_id'] - currency_id to retreive, defaults to current user
 *        String  $params['human'] - formatting that truncates the first thousands and appends "k"
 * @return String formatted currency value
 * @see include/Localization/Localization.php
 */
function format_number($amount, $round = null, $decimals = null, $params = array())
{
    global $app_strings, $current_user, $sugar_config, $locale;
    static $current_users_currency = null;
    static $last_override_currency = null;
    static $override_currency_id = null;
    static $currency;

    $amount = is_string($amount) ? (float)$amount : $amount;

    $seps = get_number_separators();
    $num_grp_sep = $seps[0];
    $dec_sep = $seps[1];

    // cn: bug 8522 - sig digits not honored in pdfs
    if (is_null($decimals)) {
        $decimals = $locale->getPrecision();
    }
    if (is_null($round)) {
        $round = $locale->getPrecision();
    }

    // only create a currency object if we need it
    if ((!empty($params['currency_symbol']) && $params['currency_symbol']) ||
      (!empty($params['convert']) && $params['convert']) ||
      (!empty($params['currency_id']))) {
        // if we have an override currency_id
        if (!empty($params['currency_id'])) {
            if ($override_currency_id != $params['currency_id']) {
                $override_currency_id = $params['currency_id'];
                $currency = BeanFactory::newBean('Currencies');
                $currency->retrieve($override_currency_id);
                $last_override_currency = $currency;
            } else {
                $currency = $last_override_currency;
            }
        } elseif (!isset($current_users_currency)) { // else use current user's
            $current_users_currency = BeanFactory::newBean('Currencies');
            if ($current_user->getPreference('currency')) {
                $current_users_currency->retrieve($current_user->getPreference('currency'));
            } else {
                $current_users_currency->retrieve('-99');
            } // use default if none set
            $currency = $current_users_currency;
        }
    }
    if (!empty($params['convert']) && $params['convert']) {
        $amount = $currency->convertFromDollar($amount, 6);
    }

    if (!empty($params['currency_symbol']) && $params['currency_symbol']) {
        if (!empty($params['symbol_override'])) {
            $symbol = $params['symbol_override'];
        } elseif (!empty($params['type']) && $params['type'] == 'pdf') {
            $symbol = $currency->getPdfCurrencySymbol();
            $symbol_space = false;
        } else {
            if (empty($currency->symbol)) {
                $symbol = $currency->getDefaultCurrencySymbol();
            } else {
                $symbol = $currency->symbol;
            }
            $symbol_space = true;
        }
    } else {
        $symbol = '';
    }

    if (isset($params['charset_convert'])) {
        $symbol = $locale->translateCharset($symbol, 'UTF-8', $locale->getExportCharset());
    }

    $checkAmount = 0;

    if (empty($params['human'])) {
        $amount = number_format(round($amount, $round), $decimals, $dec_sep, $num_grp_sep);
        $amount = format_place_symbol($amount, $symbol, (empty($params['symbol_space']) ? false : true));
    } else {
        // If amount is more greater than a thousand(positive or negative)
        if (strpos((string) $amount, '.') > 0) {
            $checkAmount = strlen(substr((string) $amount, 0, strpos((string) $amount, '.')));
        }

        if ($checkAmount >= 1000 || $checkAmount <= -1000) {
            $amount = round(($amount / 1000), 0);
            $amount = number_format($amount, 0, $dec_sep, $num_grp_sep); // add for SI bug 52498
            $amount .= 'k';
            $amount = format_place_symbol($amount, $symbol, (empty($params['symbol_space']) ? false : true));
        } else {
            $amount = format_place_symbol($amount, $symbol, (empty($params['symbol_space']) ? false : true));
        }
    }

    if (!empty($params['percentage']) && $params['percentage']) {
        $amount .= $app_strings['LBL_PERCENTAGE_SYMBOL'];
    }
    return $amount;
} //end function format_number



function format_place_symbol($amount, $symbol, $symbol_space)
{
    if ($symbol != '') {
        if ($symbol_space == true) {
            $amount = $symbol . '&nbsp;' . $amount;
        } else {
            $amount = $symbol . $amount;
        }
    }
    return $amount;
}

function unformat_number($string)
{
    // Just in case someone passes an already unformatted number through.
    if (!is_string($string)) {
        return $string;
    }

    static $currency = null;
    if (!isset($currency)) {
        global $current_user;
        $currency = BeanFactory::newBean('Currencies');
        if (!empty($current_user->id)) {
            if ($current_user->getPreference('currency')) {
                $currency->retrieve($current_user->getPreference('currency'));
            } else {
                $currency->retrieve('-99'); // use default if none set
            }
        } else {
            $currency->retrieve('-99'); // use default if none set
        }
    }

    $seps = get_number_separators();
    // remove num_grp_sep and replace decimal separator with decimal
    $string = trim(str_replace(array($seps[0], $seps[1], $currency->symbol), array('', '.', ''), $string));
    if (preg_match('/^[+-]?\d(\.\d+)?[Ee]([+-]?\d+)?$/', $string)) {
        $string = sprintf("%.0f", $string);
    }//for scientific number format. After round(), we may get this number type.
    preg_match('/[\-\+]?[0-9\.]*/', $string, $string);

    $out_number = trim($string[0]);
    if ($out_number == '') {
        return '';
    }
    return (float)$out_number;
}

// deprecated use format_number() above
function format_money($amount, $for_display = true)
{
    // This function formats an amount for display.
    // Later on, this should be converted to use proper thousand and decimal separators
    // Currently, it stays closer to the existing format, and just rounds to two decimal points
    if (isset($amount)) {
        if ($for_display) {
            return sprintf("%0.02f", $amount);
        }
        // If it's an editable field, don't use a thousand separator.
        // Or perhaps we will want to, but it doesn't matter right now.
        return sprintf("%0.02f", $amount);
    }
    return;
}

/**
 * @deprecated
 * @param bool $reset_sep
 */
function get_number_seperators($reset_sep = false)
{
    get_number_separators($reset_sep);
}

/**
 * Returns user/system preference for number grouping separator character(default ",") and the decimal separator
 *(default ".").  Special case: when num_grp_sep is ".", it will return NULL as the num_grp_sep.
 * @return array Two element array, first item is num_grp_sep, 2nd item is dec_sep
 */
function get_number_separators($reset_sep = false)
{
    global $current_user, $sugar_config;

    static $dec_sep = null;
    static $num_grp_sep = null;

    // This is typically only used during unit-tests
    // TODO: refactor this. unit tests should not have static dependencies
    if ($reset_sep) {
        $dec_sep = $num_grp_sep = null;
    }

    if ($dec_sep == null) {
        $dec_sep = $sugar_config['default_decimal_seperator'];
        if (!empty($current_user->id)) {
            $user_dec_sep = $current_user->getPreference('dec_sep');
            $dec_sep = (empty($user_dec_sep) ? $sugar_config['default_decimal_seperator'] : $user_dec_sep);
        }
    }

    if ($num_grp_sep == null) {
        $num_grp_sep = $sugar_config['default_number_grouping_seperator'];
        if (!empty($current_user->id)) {
            $user_num_grp_sep = $current_user->getPreference('num_grp_sep');
            $num_grp_sep = (empty($user_num_grp_sep)
                ? $sugar_config['default_number_grouping_seperator'] : $user_num_grp_sep);
        }
    }

    return array($num_grp_sep, $dec_sep);
}

/**
 * toString
 *
 * Utility function to print out some information about Currency instance.
 * @deprecated since version 7.10.2
 */
function toString($echo = true)
{
    LoggerManager::getLogger()->fatal('Wrong or incomplete implementation for currency to string convertation.');


    $s = "\$m_currency_round=" . (isset($m_currency_round) ? $m_currency_round : null) . " \n" .
     "\$m_currency_decimal=" . (isset($m_currency_decimal) ? $m_currency_decimal : null) . " \n" .
     "\$m_currency_symbol=" . (isset($m_currency_symbol) ? $m_currency_symbol : null) . " \n" .
     "\$m_currency_iso=" . (isset($m_currency_iso) ? $m_currency_iso : null) . " \n" .
     "\$m_currency_name=" . (isset($m_currency_name) ? $m_currency_name : null) . " \n";

    if ($echo) {
        echo $s;
    }
    return $s;
}

function getCurrencyDropDown($focus, $field='currency_id', $value='', $view='DetailView')
{
    $view = ucfirst($view);
    if ($view == 'EditView' || $view == 'MassUpdate' || $view == 'QuickCreate' || $view == 'ConvertLead') {
        if (isset($_REQUEST[$field]) && !empty($_REQUEST[$field])) {
            $value = $_REQUEST[$field];
        } elseif (empty($focus->id)) {
            $value = $GLOBALS['current_user']->getPreference('currency');
            if (empty($value)) {
                // -99 is the system default currency
                $value = -99;
            }
        }
        require_once('modules/Currencies/ListCurrency.php');
        $currency_fields = array();
        //Bug 18276 - Fix for php 5.1.6

        if (!isset($focus)) {
            LoggerManager::getLogger()->warn('Currency Dorp-down error: Focus not defined.');
            $defs = null;
        } elseif (!isset($focus->field_defs)) {
            LoggerManager::getLogger()->warn('Currency Dorp-down error: Undefined field definition for focus. Focus was: ' . get_class($focus));
            $defs = null;
        } elseif (!is_object($focus)) {
            LoggerManager::getLogger()->warn('Currency Dorp-down error: Focus is not an object. Given type of focus was: ' . gettype($focus));
            $defs = null;
        } else {
            $defs = isset($focus->field_defs) ? $focus->field_defs : null;
        }

        //
        foreach ((array)$defs as $name=>$key) {
            if ($key['type'] == 'currency') {
                $currency_fields[]= $name;
            }
        }
        $currency = new ListCurrency();
        $selectCurrency = $currency->getSelectOptions($value);

        $currency->setCurrencyFields($currency_fields);
        $html = '<select name="';
        // If it's a lead conversion (ConvertLead view), add the module_name before the $field
        if ($view == "ConvertLead") {
            $html .= $focus->module_name;
        }
        $html .= $field. '" id="' . $field  . '_select" ';
        if ($view != 'MassUpdate') {
            $html .= 'onchange="CurrencyConvertAll(this.form);"';
        }
        $html .= '>'. $selectCurrency . '</select>';
        if ($view != 'MassUpdate') {
            $html .= $currency->getJavascript();
        }
        return $html;
    }
    $currency = BeanFactory::newBean('Currencies');
    $currency->retrieve($value);
    return $currency->name;
}

function getCurrencyNameDropDown($focus, $field='currency_name', $value='', $view='DetailView')
{
    if ($view == 'EditView' || $view == 'MassUpdate' || $view == 'QuickCreate') {
        require_once('modules/Currencies/ListCurrency.php');
        $currency_fields = array();
        //Bug 18276 - Fix for php 5.1.6

        if (!isset($focus)) {
            LoggerManager::getLogger()->warn('Currency Dorp-down error: Focus not defined.');
            $defs = null;
        } elseif (!isset($focus->field_defs)) {
            LoggerManager::getLogger()->warn('Currency Dorp-down error: Undefined field definition for focus. Focus was: ' . get_class($focus));
            $defs = null;
        } elseif (!is_object($focus)) {
            LoggerManager::getLogger()->warn('Currency Dorp-down error: Focus is not an object. Given type of focus was: ' . gettype($focus));
            $defs = null;
        } else {
            $defs = isset($focus->field_defs) ? $focus->field_defs : null;
        }

        //
        foreach ((array)$defs as $name=>$key) {
            if ($key['type'] == 'currency') {
                $currency_fields[]= $name;
            }
        }
        $currency = new ListCurrency();
        $currency->lookupCurrencies();
        $listitems = array();
        foreach ($currency->list as $item) {
            $listitems[$item->name] = $item->name;
        }
        return '<select name="'.$field.'" id="'.$field.'" />'.
            get_select_options_with_id($listitems, $value).'</select>';
    }
    $currency = BeanFactory::newBean('Currencies');
    if (isset($focus->currency_id)) {
        $currency_id = $focus->currency_id;
    } else {
        $currency_id = -99;
    }
    $currency->retrieve($currency_id);
    return $currency->name;
}

function getCurrencySymbolDropDown($focus, $field='currency_name', $value='', $view='DetailView')
{
    if ($view == 'EditView' || $view == 'MassUpdate' || $view == 'QuickCreate') {
        require_once('modules/Currencies/ListCurrency.php');
        $currency_fields = array();
        //Bug 18276 - Fix for php 5.1.6

        if (!isset($focus)) {
            LoggerManager::getLogger()->warn('Currency Dorp-down error: Focus not defined.');
            $defs = null;
        } elseif (!isset($focus->field_defs)) {
            LoggerManager::getLogger()->warn('Currency Dorp-down error: Undefined field definition for focus. Focus was: ' . get_class($focus));
            $defs = null;
        } elseif (!is_object($focus)) {
            LoggerManager::getLogger()->warn('Currency Dorp-down error: Focus is not an object. Given type of focus was: ' . gettype($focus));
            $defs = null;
        } else {
            $defs = isset($focus->field_defs) ? $focus->field_defs : null;
        }

        //
        foreach ((array)$defs as $name=>$key) {
            if ($key['type'] == 'currency') {
                $currency_fields[]= $name;
            }
        }
        $currency = new ListCurrency();
        $currency->lookupCurrencies();
        $listitems = array();
        foreach ($currency->list as $item) {
            $listitems[$item->symbol] = $item->symbol;
        }
        return '<select name="'.$field.'" id="'.$field.'" />'.
            get_select_options_with_id($listitems, $value).'</select>';
    }
    $currency = BeanFactory::newBean('Currencies');
    if (isset($focus->currency_id)) {
        $currency_id = $focus->currency_id;
    } else {
        $currency_id = -99;
    }
    $currency->retrieve($currency_id);
    return $currency->name;
}
