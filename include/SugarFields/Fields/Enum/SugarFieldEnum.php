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

require_once('include/SugarFields/Fields/Base/SugarFieldBase.php');
#[\AllowDynamicProperties]
class SugarFieldEnum extends SugarFieldBase
{
    public function getDetailViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex)
    {
        if (!empty($vardef['function']['returns']) && $vardef['function']['returns']== 'html') {
            $this->setup($parentFieldArray, $vardef, $displayParams, $tabindex);
            return "<span id='{$vardef['name']}'>" . $this->fetch($this->findTemplate('DetailViewFunction')) . "</span>";
        } else {
            return parent::getDetailViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex);
        }
    }
    
    public function getEditViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex)
    {
        if (empty($displayParams['size'])) {
            $displayParams['size'] = 6;
        }
        
        if (isset($vardef['function']) && !empty($vardef['function']['returns']) && $vardef['function']['returns']== 'html') {
            $this->setup($parentFieldArray, $vardef, $displayParams, $tabindex);
            return $this->fetch($this->findTemplate('EditViewFunction'));
        } else {
            return parent::getEditViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex);
        }
    }
    
    
    
    public function getSearchViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex)
    {
        if (empty($displayParams['size'])) {
            $displayParams['size'] = 6;
        }
        
        if (!empty($vardef['function']['returns']) && $vardef['function']['returns']== 'html') {
            $this->setup($parentFieldArray, $vardef, $displayParams, $tabindex);
            return $this->fetch($this->findTemplate('EditViewFunction'));
        } else {
            $this->setup($parentFieldArray, $vardef, $displayParams, $tabindex);
            return $this->fetch($this->findTemplate('SearchView'));
        }
    }
    

    public function displayFromFunc($displayType, $parentFieldArray, $vardef, $displayParams, $tabindex = 0)
    {
        if (isset($vardef['function']['returns']) && $vardef['function']['returns'] == 'html') {
            return parent::displayFromFunc($displayType, $parentFieldArray, $vardef, $displayParams, $tabindex);
        }

        $displayTypeFunc = 'get'.$displayType.'Smarty';
        return $this->$displayTypeFunc($parentFieldArray, $vardef, $displayParams, $tabindex);
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
        global $app_list_strings;
        
        // Bug 27467 - Trim the value given
        $value = trim($value);
        
        if (isset($app_list_strings[$vardef['options']])
                && !isset($app_list_strings[$vardef['options']][$value])) {
            // Bug 23485/23198 - Check to see if the value passed matches the display value
            if (in_array($value, $app_list_strings[$vardef['options']])) {
                $value = array_search($value, $app_list_strings[$vardef['options']]);
            }
            // Bug 33328 - Check for a matching key in a different case
            elseif (in_array(strtolower($value), array_keys(array_change_key_case($app_list_strings[$vardef['options']])))) {
                foreach ($app_list_strings[$vardef['options']] as $optionkey => $optionvalue) {
                    if (strtolower($value) == strtolower($optionkey)) {
                        $value = $optionkey;
                    }
                }
            }
            // Bug 33328 - Check for a matching value in a different case
            elseif (in_array(strtolower($value), array_map('strtolower', $app_list_strings[$vardef['options']]))) {
                foreach ($app_list_strings[$vardef['options']] as $optionkey => $optionvalue) {
                    if (strtolower($value) == strtolower($optionvalue)) {
                        $value = $optionkey;
                    }
                }
            } else {
                return false;
            }
        }
        
        return $value;
    }
    
    public function formatField($rawField, $vardef)
    {
        global $app_list_strings;
        
        if (!empty($vardef['options'])) {
            $option_array_name = $vardef['options'];
            
            if (!empty($app_list_strings[$option_array_name][$rawField])) {
                return $app_list_strings[$option_array_name][$rawField];
            } else {
                return $rawField;
            }
        } else {
            return $rawField;
        }
    }
}
