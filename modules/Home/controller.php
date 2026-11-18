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

include_once get_custom_file_if_exists('include/InlineEditing/InlineEditing.php');

#[\AllowDynamicProperties]
class HomeController extends SugarController
{
    public function action_saveHTMLField()
    {
        if ($_REQUEST['field'] && $_REQUEST['id'] && $_REQUEST['current_module']) {
            echo saveField($_REQUEST['field'], $_REQUEST['id'], $_REQUEST['current_module'], $_REQUEST['value'], $_REQUEST['view']);
        }
    }

    public function action_getDisplayValue()
    {
        if ($_REQUEST['field'] && $_REQUEST['id'] && $_REQUEST['current_module']) {
            $bean = BeanFactory::getBean($_REQUEST['current_module'], $_REQUEST['id']);

            if (is_object($bean) && $bean->id != "") {
                echo getDisplayValue($bean, $_REQUEST['field'], "close");
            } else {
                echo "Could not find value.";
            }
        }
    }

        // for inline editing performance improvement:
    public function action_getInlineEditFieldInfo()
    {
        if ($_REQUEST['field'] && $_REQUEST['id'] && $_REQUEST['current_module']) {
            $validation = getValidationRules($_REQUEST['current_module'], $_REQUEST['field'], $_REQUEST['id']);
            $html = getEditFieldHTML($_REQUEST['current_module'], $_REQUEST['field'], $_REQUEST['field'], 'EditView', $_REQUEST['id']);
            $relateJS = '';
            if ($_REQUEST['type'] === 'relate' || $_REQUEST['type'] === 'parent') {
                $relateJS = getRelateFieldJS($_REQUEST['current_module'], $_REQUEST['field']);
            }
            $this->view = '';  // see SugarController.php --> execute() function
            echo json_encode(
                [ 'validationRules' => $validation,
                  'editFieldHTML'   => $html,
                  'relateFieldJS'   => $relateJS ]
            );
        }
    }

}
