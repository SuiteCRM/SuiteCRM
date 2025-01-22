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

/**
 * Handle Sugar fields
 * @api
 */
class SugarFieldHandler
{
    public function __construct()
    {
    }




    public static function fixupFieldType($field)
    {
        switch ($field) {
               case 'double':
               case 'decimal':
                    $field = 'float';
                    break;
               case 'uint':
               case 'ulong':
               case 'long':
               case 'short':
               case 'tinyint':
                    $field = 'int';
                    break;
               case 'date':
                    $field = 'datetime';
                    break;
               case 'url':
                       $field = 'link';
                       break;
               case 'varchar':
                    $field = 'base';
                    break;
            }

        return ucfirst($field);
    }

    /**
     * return the singleton of the SugarField
     * @param $field
     * @param bool $returnNullIfBase
     * @return mixed
     */
    public static function getSugarField($field, $returnNullIfBase=false)
    {
        static $sugarFieldObjects = array();

        $field = self::fixupFieldType($field);
        $field = ucfirst($field);

        if (!isset($sugarFieldObjects[$field])) {
            //check custom directory
            if (file_exists('custom/include/SugarFields/Fields/' . $field . '/SugarField' . $field. '.php')) {
                $file = 'custom/include/SugarFields/Fields/' . $field . '/SugarField' . $field. '.php';
                $type = $field;
            //else check the fields directory
            } elseif (file_exists('include/SugarFields/Fields/' . $field . '/SugarField' . $field. '.php')) {
                $file = 'include/SugarFields/Fields/' . $field . '/SugarField' . $field. '.php';
                $type = $field;
            } else {
                // No direct class, check the directories to see if they are defined
                if ($returnNullIfBase &&
                    !is_dir('custom/include/SugarFields/Fields/'.$field) &&
                    !is_dir('include/SugarFields/Fields/'.$field)) {
                    return null;
                }
                $file = get_custom_file_if_exists('include/SugarFields/Fields/Base/SugarFieldBase.php');
                $type = 'Base';
            }
            require_once($file);

            $class = 'SugarField' . $type;
            //could be a custom class check it
            $customClass = 'Custom' . $class;
            if (class_exists($customClass)) {
                $sugarFieldObjects[$field] = new $customClass($field);
            } else {
                $sugarFieldObjects[$field] = new $class($field);
            }
        }
        return $sugarFieldObjects[$field];
    }

    /**
     * Returns the smarty code to be used in a template built by TemplateHandler
     * The SugarField class is choosen dependant on the vardef's type field.
     *
     * @param parentFieldArray string name of the variable in the parent template for the bean's data
     * @param vardef vardef field defintion
     * @param displayType string the display type for the field (eg DetailView, EditView, etc)
     * @param displayParam parameters for displayin
     *      available paramters are:
     *      * labelSpan - column span for the label
     *      * fieldSpan - column span for the field
     */
    public static function displaySmarty($parentFieldArray, $vardef, $displayType, $displayParams = array(), $tabindex = 1)
    {
        $string = '';
        $displayTypeFunc = 'get' . $displayType . 'Smarty'; // getDetailViewSmarty, getEditViewSmarty, etc...

        // This will handle custom type fields.
        // The incoming $vardef Array may have custom_type set.
        // If so, set $vardef['type'] to the $vardef['custom_type'] value
        if (isset($vardef['custom_type'])) {
            $vardef['type'] = $vardef['custom_type'];
        }
        if (empty($vardef['type'])) {
            $vardef['type'] = 'varchar';
        }

        $field = self::getSugarField($vardef['type']);
        if (!empty($vardef['function'])) {
            $string = $field->displayFromFunc($displayType, $parentFieldArray, $vardef, $displayParams, $tabindex);
        } else {
            $string = $field->$displayTypeFunc($parentFieldArray, $vardef, $displayParams, $tabindex);
        }

        return $string;
    }
}
