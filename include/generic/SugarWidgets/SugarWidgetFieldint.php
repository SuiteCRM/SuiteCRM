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



#[\AllowDynamicProperties]
class SugarWidgetFieldInt extends SugarWidgetReportField
{
    public function displayList(&$layout_def)
    {
        return $this->displayListPlain($layout_def);
    }

    public function queryFilterEquals(&$layout_def)
    {
        return $this->_get_column_select($layout_def)."= '".DBManagerFactory::getInstance()->quote($layout_def['input_name0'])."'\n";
    }

    public function queryFilterNot_Equals(&$layout_def)
    {
        return $this->_get_column_select($layout_def)."!='".DBManagerFactory::getInstance()->quote($layout_def['input_name0'])."'\n";
    }

    public function queryFilterGreater(&$layout_def)
    {
        return $this->_get_column_select($layout_def)." > '".DBManagerFactory::getInstance()->quote($layout_def['input_name0'])."'\n";
    }

    public function queryFilterLess(&$layout_def)
    {
        return $this->_get_column_select($layout_def)." < '".DBManagerFactory::getInstance()->quote($layout_def['input_name0'])."'\n";
    }

    public function queryFilterBetween(&$layout_def)
    {
        return $this->_get_column_select($layout_def)." BETWEEN '".DBManagerFactory::getInstance()->quote($layout_def['input_name0']). "' AND '" . DBManagerFactory::getInstance()->quote($layout_def['input_name1']) . "'\n";
    }

    public function queryFilterStarts_With(&$layout_def)
    {
        return $this->queryFilterEquals($layout_def);
    }

    public function displayInput($layout_def)
    {
        return '<input type="text" size="20" value="' . $layout_def['input_name0'] . '" name="' . $layout_def['name'] . '">';
    }
 
    public function display($layout_def)
    {
        $obj = null;
        //Bug40995
        if (isset($obj->layout_manager->defs['reporter']->focus->field_name_map[$layout_def['name']]['precision'])) {
            $precision=$obj->layout_manager->defs['reporter']->focus->field_name_map[$layout_def['name']]['precision'];
            $layout_def['precision']=$precision;
        }
        //Bug40995
        return parent::display($layout_def);
    }
}
