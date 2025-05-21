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
class ViewEditFields extends ViewAjax
{
    public function __construct()
    {
        $rel = $this->rel = $_REQUEST['rel'];
        $this->id = $_REQUEST['id'];
        $moduleName = $this->module = $_REQUEST['rel_module'];

        global $beanList;
        require_once("data/Link.php");

        $beanName = $beanList [ $moduleName ];
        $link = new Link($this->rel, new $beanName(), array());
        $this->fields = $link->_get_link_table_definition($rel, 'fields');
    }

    public function display()
    {

        //echo "<pre>".print_r($this->fields, true)."</pre>";
        echo "<form name='edit_rel_fields'>" .
             '<input type="submit" class="button primary" value="Save">' .
             '<input type="button" class="button" onclick="editRelPanel.hide()" value="Cancel">' .
             '<input type="hidden" name="module" value="Relationships">' .
             '<input type="hidden" name="action" value="saverelfields">' .
             '<input type="hidden" name="rel" value="' . $this->rel .'">' .
             '<input type="hidden" name="id"  value="' . $this->id  .'">' .
             '<input type="hidden" name="rel_module" value="' . $this->module .'">' .
             "<table class='edit view'><tr>";
        $count = 0;
        foreach ($this->fields as $def) {
            if (!empty($def['relationship_field'])) {
                $label = !empty($def['vname']) ? $def['vname'] : $def['name'];
                echo "<td>" . translate($label, $this->module) . ":</td>"
                   . "<td><input id='{$def['name']}' name='{$def['name']}'>"  ;

                if ($count%1) {
                    echo "</tr><tr>";
                }
                $count++;
            }
        }
        echo "</tr></table></form>";
    }
}
