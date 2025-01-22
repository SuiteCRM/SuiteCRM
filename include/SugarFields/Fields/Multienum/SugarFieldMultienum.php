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

require_once('include/SugarFields/Fields/Enum/SugarFieldEnum.php');

class SugarFieldMultienum extends SugarFieldEnum
{
    public function setup($parentFieldArray, $vardef, $displayParams, $tabindex, $twopass=true)
    {
        if (!isset($vardef['options_list']) && isset($vardef['options']) && !is_array($vardef['options'])) {
            $vardef['options_list'] = $GLOBALS['app_list_strings'][$vardef['options']];
        }

        if ($_REQUEST['module'] === 'AOW_WorkFlow') {
            $displayParams['isWorkFlowCall'] = true;
        }
        return parent::setup($parentFieldArray, $vardef, $displayParams, $tabindex, $twopass);
    }

    public function getSearchViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex)
    {
        if (!empty($vardef['function']['returns']) && $vardef['function']['returns']== 'html') {
            $this->setup($parentFieldArray, $vardef, $displayParams, $tabindex);
            return $this->fetch($this->findTemplate('EditViewFunction'));
        } else {
            $this->setup($parentFieldArray, $vardef, $displayParams, $tabindex);
            return $this->fetch($this->findTemplate('SearchView'));
        }
    }

    /**
     * @param $displayType
     * @param $parentFieldArray
     * @param $vardef
     * @param $displayParams
     * @param int $tabindex
     * @return string
     */
    public function displayFromFunc($displayType, $parentFieldArray, $vardef, $displayParams, $tabindex = 0)
    {
        if (isset($vardef['function']['returns']) && $vardef['function']['returns'] == 'html') {
            return parent::displayFromFunc($displayType, $parentFieldArray, $vardef, $displayParams, $tabindex);
        }

        $displayTypeFunc = 'get'.$displayType.'Smarty';
        return $this->$displayTypeFunc($parentFieldArray, $vardef, $displayParams, $tabindex);
    }

    public function save(&$bean, $params, $field, $properties, $prefix = '')
    {
        if (isset($params[$prefix.$field])) {
            if ($params[$prefix.$field][0] === '' && !empty($params[$prefix.$field][1])) {
                unset($params[$prefix.$field][0]);
            }

            $bean->$field = encodeMultienumValue($params[$prefix.$field]);
        } else {
            if (isset($params[$prefix.$field.'_multiselect']) && $params[$prefix.$field.'_multiselect']==true) {
                // if the value in db is not empty and
                // if the data is not set in params (means the user has deselected everything) and
                // if the corresponding multiselect flag is true
                // then set field to ''
                if (!empty($bean->$field)) {
                    $bean->$field = '';
                }
            }
        }
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
        if (!empty($value) && is_array($value)) {
            $enum_list = $value;
        } else {
            // If someone was using the old style multienum import technique
            $value = str_replace("^", "", $value);

            // We will need to break it apart to put test it.
            $enum_list = explode(",", $value);
        }
        // parse to see if all the values given are valid
        foreach ($enum_list as $key => $enum_value) {
            $enum_list[$key] = $enum_value = trim($enum_value);
            $sanitizedValue = parent::importSanitize($enum_value, $vardef, $focus, $settings);
            if ($sanitizedValue  === false) {
                return false;
            } else {
                $enum_list[$key] = $sanitizedValue;
            }
        }
        $value = encodeMultienumValue($enum_list);

        return $value;
    }
}
