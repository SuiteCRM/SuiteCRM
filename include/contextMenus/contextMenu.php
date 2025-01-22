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
class contextMenu
{
    public $menuItems;
    public $objectName;

    public function __construct()
    {
        $this->menuItems = array();
    }




    public function getScript()
    {
        $json = getJSONobj();
        return "SUGAR.contextMenu.registerObjectType('{$this->objectName}', " . $json->encode($this->menuItems) . ");\n";
    }

    /**
     * adds a menu item to the current contextMenu
     *
     * @param string $text text of the item
     * @param string $action function or pointer to the javascript function to call
     * @param array $params other parameters includes:
     *      url - The URL for the MenuItem's anchor's "href" attribute.
     *      target - The value to be used for the MenuItem's anchor's "target" attribute.
     *      helptext - Additional instructional text to accompany the text for a MenuItem. Example: If the text is
     *                 "Copy" you might want to add the help text "Ctrl + C" to inform the user there is a keyboard
     *                 shortcut for the item.
     *      emphasis - If set to true the text for the MenuItem will be rendered with emphasis (using <em>).
     *      strongemphasis - If set to true the text for the MenuItem will be rendered with strong emphasis (using <strong>).
     *      disabled - If set to true the MenuItem will be dimmed and will not respond to user input or fire events.
     *      selected - If set to true the MenuItem will be highlighted.
     *      submenu - Appends / removes a menu (and it's associated DOM elements) to / from the MenuItem.
     *      checked - If set to true the MenuItem will be rendered with a checkmark.
     */
    public function addMenuItem($text, $action, $module = null, $aclAction = null, $params = null)
    {
        // check ACLs if module and aclAction set otherwise no ACL check
        if (((!empty($module) && !empty($aclAction)) && ACLController::checkAccess($module, $aclAction)) || (empty($module) || empty($aclAction))) {
            $item = array('text' => translate($text),
                          'action' => $action);
            foreach (array('url', 'target', 'helptext', 'emphasis', 'strongemphasis', 'disabled', 'selected', 'submenu', 'checked') as $param) {
                if (!empty($params[$param])) {
                    $item[$param] = $params[$param];
                }
            }
            array_push($this->menuItems, $item);
        }
    }

    /**
     * Loads up menu items from files located in include/contextMenus/menuDefs
     * @param string $name name of the object
     */
    public function loadFromFile($name)
    {
        global $menuDef;
        clean_string($name, 'FILE');
        require_once('include/contextMenus/menuDefs/' . $name . '.php');
        $this->loadFromDef($name, $menuDef[$name]);
    }

    /**
     * Loads up menu items from def
     * @param string $name name of the object type
     * @param array $defs menu item definitions
     */
    public function loadFromDef($name, $defs)
    {
        $this->objectName = $name;
        foreach ($defs as $def) {
            $this->addMenuItem(
                $def['text'],
                $def['action'],
                (empty($def['module']) ? null : $def['module']),
                (empty($def['aclAction']) ? null : $def['aclAction']),
                (empty($def['params']) ? null : $def['params'])
            );
        }
    }
}
