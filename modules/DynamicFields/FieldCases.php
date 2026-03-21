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



require_once('modules/DynamicFields/templates/Fields/TemplateTextArea.php');
require_once('modules/DynamicFields/templates/Fields/TemplateFloat.php');
require_once('modules/DynamicFields/templates/Fields/TemplateInt.php');
require_once('modules/DynamicFields/templates/Fields/TemplateDate.php');
require_once('modules/DynamicFields/templates/Fields/TemplateDatetimecombo.php');
require_once('modules/DynamicFields/templates/Fields/TemplateBoolean.php');
require_once('modules/DynamicFields/templates/Fields/TemplateEnum.php');
require_once('modules/DynamicFields/templates/Fields/TemplateMultiEnum.php');
require_once('modules/DynamicFields/templates/Fields/TemplateRadioEnum.php');
require_once('modules/DynamicFields/templates/Fields/TemplateEmail.php');
require_once('modules/DynamicFields/templates/Fields/TemplateRelatedTextField.php');

require_once('modules/DynamicFields/templates/Fields/TemplateURL.php');
require_once('modules/DynamicFields/templates/Fields/TemplateIFrame.php');
require_once('modules/DynamicFields/templates/Fields/TemplateHTML.php');
require_once('modules/DynamicFields/templates/Fields/TemplatePhone.php');
require_once('modules/DynamicFields/templates/Fields/TemplateCurrency.php');
require_once('modules/DynamicFields/templates/Fields/TemplateParent.php');
require_once('modules/DynamicFields/templates/Fields/TemplateCurrencyId.php');
require_once('modules/DynamicFields/templates/Fields/TemplateAddress.php');
require_once('modules/DynamicFields/templates/Fields/TemplateParentType.php');
require_once('modules/DynamicFields/templates/Fields/TemplateEncrypt.php');
require_once('modules/DynamicFields/templates/Fields/TemplateId.php');
require_once('modules/DynamicFields/templates/Fields/TemplateImage.php');
require_once('modules/DynamicFields/templates/Fields/TemplateDecimal.php');
function get_widget($type)
{
    $type = (string) $type;
    $local_temp = null;
    switch (strtolower($type)) {
            case 'char':
            case 'varchar':
            case 'varchar2':
                        $local_temp = new TemplateText(); break;
            case 'text':
            case 'textarea':
                        $local_temp = new TemplateTextArea(); break;
            case 'double':

            case 'float':
                        $local_temp = new TemplateFloat(); break;
            case 'decimal':
                        $local_temp = new TemplateDecimal(); break;
            case 'int':
                        $local_temp = new TemplateInt(); break;
            case 'date':
                        $local_temp = new TemplateDate(); break;
            case 'bool':
                        $local_temp = new TemplateBoolean(); break;
            case 'relate':
                        $local_temp = new TemplateRelatedTextField(); break;
            case 'enum':
                        $local_temp = new TemplateEnum(); break;
            case 'multienum':
                        $local_temp = new TemplateMultiEnum(); break;
            case 'radioenum':
                        $local_temp = new TemplateRadioEnum(); break;
            case 'email':
                        $local_temp = new TemplateEmail(); break;
            case 'url':
                        $local_temp = new TemplateURL(); break;
            case 'iframe':
                        $local_temp = new TemplateIFrame(); break;
            case 'html':
                        $local_temp = new TemplateHTML(); break;
            case 'phone':
                        $local_temp = new TemplatePhone(); break;
            case 'currency':
                        $local_temp = new TemplateCurrency(); break;
            case 'parent':
                        $local_temp = new TemplateParent(); break;
            case 'parent_type':
                        $local_temp = new TemplateParentType(); break;
            case 'currency_id':
                        $local_temp = new TemplateCurrencyId(); break;
            case 'address':
                        $local_temp = new TemplateAddress(); break;
            case 'encrypt':
                        $local_temp = new TemplateEncrypt(); break;
            case 'id':
                        $local_temp = new TemplateId(); break;
            case 'datetimecombo':
            case 'datetime':
                        $local_temp = new TemplateDatetimecombo(); break;
            case 'image':
                        $local_temp = new TemplateImage(); break;
            default:
                        $file = false;
                        if (file_exists('custom/modules/DynamicFields/templates/Fields/Template'. ucfirst($type) . '.php')) {
                            $file  =	'custom/modules/DynamicFields/templates/Fields/Template'. ucfirst($type) . '.php';
                        } else {
                            if (file_exists('modules/DynamicFields/templates/Fields/Template'. ucfirst($type) . '.php')) {
                                $file  =	'modules/DynamicFields/templates/Fields/Template'. ucfirst($type) . '.php';
                            }
                        }
                        if (!empty($file)) {
                            require_once($file);
                            $class  = 'Template' . ucfirst($type) ;
                            $customClass = 'Custom' . $class;
                            if (class_exists($customClass)) {
                                $local_temp = new $customClass();
                            } else {
                                $local_temp = new $class();
                            }
                            break;
                        } else {
                            $local_temp = new TemplateText();
                            break;
                        }
    }

    return $local_temp;
}
