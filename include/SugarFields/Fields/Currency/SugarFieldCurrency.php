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


require_once('include/SugarFields/Fields/Float/SugarFieldFloat.php');

class SugarFieldCurrency extends SugarFieldFloat
{
    public function getListViewSmarty($parentFieldArray, $vardef, $displayParams, $col)
    {
        global $locale, $current_user;
        $tabindex = 1;
        $this->setup($parentFieldArray, $vardef, $displayParams, $tabindex, false);

        $currencyUSD = '-99';

        $amount = $parentFieldArray[strtoupper($vardef['name'])];
        $currencyId = isset($parentFieldArray['CURRENCY_ID']) ? $parentFieldArray['CURRENCY_ID'] : "";
        $currencySymbol = isset($parentFieldArray['CURRENCY_SYMBOL']) ? $parentFieldArray['CURRENCY_SYMBOL'] : "";

        if (empty($currencyId)) {
            $currencyId = $locale->getPrecedentPreference('currency');
        }

        if (empty($currencySymbol)) {
            $currencySymbol = $locale->getPrecedentPreference('default_currency_symbol');
        }
        
        if (stripos($vardef['name'], '_USD')) {
            $userCurrencyId = $current_user->getPreference('currency');
            if (!empty($userCurrencyId) && $currencyUSD !== $userCurrencyId) {
                $userCurrency = BeanFactory::getBean('Currencies', $userCurrencyId);
                $currencyId = $userCurrency->id;
                $currencySymbol = $userCurrency->symbol;
                $amount = $userCurrency->convertFromDollar($amount, 6);
            } else {
                $currencyId = $currencyUSD;
                $currencySymbol = $locale->getPrecedentPreference('default_currency_symbol');
            }
        }

        $this->ss->assign('currency_id', $currencyId);
        $this->ss->assign('currency_symbol', $currencySymbol);
        $this->ss->assign('amount', $amount);

        return $this->fetch($this->findTemplate('ListView'));
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
        $value = str_replace($settings->currency_symbol, "", $value);
        
        $value = str_replace($settings->num_grp_sep, "", $value);       

        if (isset($vardef['len'])) {
            // check for field length
            $length = explode(',', $vardef['len']);
            $value = sugar_substr($value, $length[0]);
        }

        return $value;
    }

    /**
     * format the currency field based on system locale values for currency
     * Note that this may be different from the precision specified in the vardefs.
     * @param string $rawfield value of the field
     * @param string $somewhere vardef for the field being processed
     * @return number formatted according to currency settings
     */
    public function formatField($rawField, $vardef)
    {
        // for currency fields, use the user or system precision, not the precision in the vardef
        //this is achived by passing in $precision as null
        $precision = null;
        if ($rawField === '' || $rawField === null) {
            return '';
        }
        return format_number($rawField, $precision, $precision);
    }
}
