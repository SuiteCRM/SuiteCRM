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

#[\AllowDynamicProperties]
class FieldViewer
{
    public function __construct()
    {
        $this->ss = new Sugar_Smarty();
    }



    public function getLayout($vardef)
    {
        if (empty($vardef['type'])) {
            $vardef['type'] = 'varchar';
        }
        $mod = return_module_language($GLOBALS['current_language'], 'DynamicFields');
        $this->ss->assign('vardef', $vardef);
        $this->ss->assign('MOD', $mod);
        $this->ss->assign('APP', $GLOBALS['app_strings']);
        //Only display range search option if in Studio, not ModuleBuilder
        $this->ss->assign('range_search_option_enabled', empty($_REQUEST['view_package']));

        $GLOBALS['log']->debug('FieldViewer.php->getLayout() = '.$vardef['type']);
        switch ($vardef['type']) {
            case 'address':
                return $this->ss->fetch('modules/DynamicFields/templates/Fields/Forms/address.tpl');
            case 'bool':
                return $this->ss->fetch('modules/DynamicFields/templates/Fields/Forms/bool.tpl');
            case 'int':
                return $this->ss->fetch('modules/DynamicFields/templates/Fields/Forms/int.tpl');
            case 'float':
                return $this->ss->fetch('modules/DynamicFields/templates/Fields/Forms/float.tpl');
            case 'decimal':
                return $this->ss->fetch('modules/DynamicFields/templates/Fields/Forms/float.tpl');
            case 'date':
                require_once('modules/DynamicFields/templates/Fields/Forms/date.php');
                return get_body($this->ss, $vardef);
            case 'datetimecombo':
            case 'datetime':
                require_once('modules/DynamicFields/templates/Fields/Forms/datetimecombo.php');
                return get_body($this->ss, $vardef);
            case 'enum':
                require_once('modules/DynamicFields/templates/Fields/Forms/enum2.php');
                return get_body($this->ss, $vardef);
            case 'multienum':
                require_once('modules/DynamicFields/templates/Fields/Forms/multienum.php');
                return get_body($this->ss, $vardef);
            case 'radioenum':
                require_once('modules/DynamicFields/templates/Fields/Forms/radioenum.php');
                return get_body($this->ss, $vardef);
            case 'html':
                require_once('modules/DynamicFields/templates/Fields/Forms/html.php');
                return get_body($this->ss, $vardef);
            case 'currency':
                return $this->ss->fetch('modules/DynamicFields/templates/Fields/Forms/currency.tpl');
            case 'relate':
                require_once('modules/DynamicFields/templates/Fields/Forms/relate.php');
                return get_body($this->ss, $vardef);
            case 'parent':
                require_once('modules/DynamicFields/templates/Fields/Forms/parent.php');
                return get_body($this->ss, $vardef);
            case 'text':
                return $this->ss->fetch('modules/DynamicFields/templates/Fields/Forms/text.tpl');
            case 'encrypt':
                require_once('modules/DynamicFields/templates/Fields/Forms/encrypt.php');
                return get_body($this->ss, $vardef);
            case 'iframe':
                require_once('modules/DynamicFields/templates/Fields/Forms/iframe.php');
                return get_body($this->ss, $vardef);
            case 'url':
                require_once('modules/DynamicFields/templates/Fields/Forms/url.php');
                return get_body($this->ss, $vardef);
            case 'phone:':
                require_once('modules/DynamicFields/templates/Fields/Forms/phone.php');
                return get_body($this->ss, $vardef);
            default:
                $file = false;
                if (file_exists('custom/modules/DynamicFields/templates/Fields/Forms/' . $vardef['type'] . '.php')) {
                    $file = 'custom/modules/DynamicFields/templates/Fields/Forms/' . $vardef['type'] . '.php';
                } elseif (file_exists('modules/DynamicFields/templates/Fields/Forms/' . $vardef['type'] . '.php')) {
                    $file = 'modules/DynamicFields/templates/Fields/Forms/' . $vardef['type'] . '.php';
                }
                if (!empty($file)) {
                    require_once($file);
                    return get_body($this->ss, $vardef);
                } else {
                    return $this->ss->fetch('modules/DynamicFields/templates/Fields/Forms/varchar.tpl');
                }
        }
    }
}
