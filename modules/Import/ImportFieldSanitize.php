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

/**

 * Description: class for sanitizing field values
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 */
require_once('modules/Import/sources/ImportFile.php');

#[\AllowDynamicProperties]
class ImportFieldSanitize
{
    /**
     * properties set to handle locale formatting
     */
    public $dateformat;
    public $timeformat;
    public $timezone;
    public $currency_symbol;
    public $default_currency_significant_digits;
    public $num_grp_sep;
    public $dec_sep;
    public $default_locale_name_format;

    /**
     * array of modules/users_last_import ids pairs that are created in this class
     * needs to be reset after the row is imported
     */
    public static $createdBeans = array();

    /**
     * true if we will create related beans during the sanitize process
     */
    public $addRelatedBean = false;
    
    /**
     * Checks the SugarField defintion for an available santization method.
     *
     * @param  $value  string
     * @param  $vardef array
     * @param  $focus  object bean of the module we're importing into
     * @return string sanitized and validated value on success, bool false on failure
     */
    public function __call(
        $name,
        $params
        ) {
        static $sfh;
        
        if (!isset($sfh)) {
            require_once('include/SugarFields/SugarFieldHandler.php');
            $sfh = new SugarFieldHandler();
        }
        $value = $params[0];
        $vardef = $params[1];
        if (isset($params[2])) {
            $focus = $params[2];
        } else {
            $focus = null;
        }
        if ($name == 'relate' && !empty($params[3])) {
            $this->addRelatedBean = true;
        } else {
            $this->addRelatedBean = false;
        }
        
        $field = $sfh::getSugarField(ucfirst($name));
        if ($field instanceof SugarFieldBase) {
            $value = $field->importSanitize($value, $vardef, $focus, $this);
        }
        
        return $value;
    }

    /**
     * Validate date fields
     *
     * @param  $value  string
     * @param  $vardef array
     * @param  $focus  object bean of the module we're importing into
     * @return string sanitized and validated value on success, bool false on failure
     */
    public function date(
        $value,
        $vardef,
        &$focus
        ) {
        global $timedate;

        $format = $this->dateformat;

        if (!$timedate->check_matching_format($value, $format)) {
            return false;
        }

        if (!$this->isValidTimeDate($value, $format)) {
            return false;
        }

        $value = $timedate->swap_formats(
            $value,
            $format,
            $timedate->get_date_format()
        );

        return $value;
    }

    /**
     * Validate email fields
     *
     * @param  $value  string
     * @param  $vardef array
     * @param  $focus  object bean of the module we're importing into
     * @return string sanitized and validated value on success, bool false on failure
     */
    public function email(
        $value,
        $vardef
        ) {
        // cache $sea instance
        static $sea;
        
        if (!($sea instanceof SugarEmailAddress)) {
            $sea = new SugarEmailAddress;
        }
        
        if (!empty($value) && !preg_match($sea->regex, (string) $value)) {
            return false;
        }

        return $value;
    }

    /**
     * Validate sync_to_outlook field
     *
     * @param  $value     string
     * @param  $vardef    array
     * @param  $bad_names array used to return list of bad users/teams in $value
     * @return string sanitized and validated value on success, bool false on failure
     */
    public function synctooutlook(
        $value,
        $vardef,
        &$bad_names
        ) {
        static $focus_user;

        // cache this object since we'll be reusing it a bunch
        if (!($focus_user instanceof User)) {
            $focus_user = BeanFactory::newBean('Users');
        }


        if (!empty($value) && strtolower($value) != "all") {
            $theList   = explode(",", $value);
            $isValid   = true;
            $bad_names = array();
            foreach ($theList as $eachItem) {
                if ($focus_user->retrieve_user_id($eachItem)
                        || $focus_user->retrieve($eachItem)
                ) {
                    // all good
                } else {
                    $isValid     = false;
                    $bad_names[] = $eachItem;
                    continue;
                }
            }
            if (!$isValid) {
                return false;
            }
        }

        return $value;
    }

    /**
     * Validate time fields
     *
     * @param  $value    string
     * @param  $vardef   array
     * @param  $focus  object bean of the module we're importing into
     * @return string sanitized and validated value on success, bool false on failure
     */
    public function time(
        $value,
        $vardef,
        $focus
        ) {
        global $timedate;

        $format = $this->timeformat;

        if (!$timedate->check_matching_format($value, $format)) {
            return false;
        }

        if (!$this->isValidTimeDate($value, $format)) {
            return false;
        }

        $value = $timedate->swap_formats(
            $value,
            $format,
            $timedate->get_time_format()
        );
        $value = $timedate->handle_offset(
            $value,
            $timedate->get_time_format(),
            false,
            $GLOBALS['current_user'],
            $this->timezone
        );
        $value = $timedate->handle_offset(
            $value,
            $timedate->get_time_format(),
            true
        );

        return $value;
    }

    /**
     * Added to handle Bug 24104, to make sure the date/time value is correct ( i.e. 20/20/2008 doesn't work )
     *
     * @param  $value  string
     * @param  $format string
     * @return string sanitized and validated value on success, bool false on failure
     */
    public function isValidTimeDate(
        $value,
        $format
        ) {
        global $timedate;

        $dateparts = array();
        $reg = $timedate->get_regular_expression($format);
        preg_match('@'.$reg['format'].'@', (string) $value, $dateparts);

        if (empty($dateparts)) {
            return false;
        }
        if (isset($reg['positions']['a'])
                && !in_array($dateparts[$reg['positions']['a']], array('am','pm'))) {
            return false;
        }
        if (isset($reg['positions']['A'])
                && !in_array($dateparts[$reg['positions']['A']], array('AM','PM'))) {
            return false;
        }
        if (isset($reg['positions']['h']) && (
            !is_numeric($dateparts[$reg['positions']['h']])
                || $dateparts[$reg['positions']['h']] < 1
                || $dateparts[$reg['positions']['h']] > 12
        )) {
            return false;
        }
        if (isset($reg['positions']['H']) && (
            !is_numeric($dateparts[$reg['positions']['H']])
                || $dateparts[$reg['positions']['H']] < 0
                || $dateparts[$reg['positions']['H']] > 23
        )) {
            return false;
        }
        if (isset($reg['positions']['i']) && (
            !is_numeric($dateparts[$reg['positions']['i']])
                || $dateparts[$reg['positions']['i']] < 0
                || $dateparts[$reg['positions']['i']] > 59
        )) {
            return false;
        }
        if (isset($reg['positions']['s']) && (
            !is_numeric($dateparts[$reg['positions']['s']])
                || $dateparts[$reg['positions']['s']] < 0
                || $dateparts[$reg['positions']['s']] > 59
        )) {
            return false;
        }
        if (isset($reg['positions']['d']) && (
            !is_numeric($dateparts[$reg['positions']['d']])
                || $dateparts[$reg['positions']['d']] < 1
                || $dateparts[$reg['positions']['d']] > 31
        )) {
            return false;
        }
        if (isset($reg['positions']['m']) && (
            !is_numeric($dateparts[$reg['positions']['m']])
                || $dateparts[$reg['positions']['m']] < 1
                || $dateparts[$reg['positions']['m']] > 12
        )) {
            return false;
        }
        if (isset($reg['positions']['Y']) &&
                !is_numeric($dateparts[$reg['positions']['Y']])) {
            return false;
        }

        return true;
    }
}
