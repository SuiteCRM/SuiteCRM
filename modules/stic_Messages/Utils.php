<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

// global $messageableModules;

// This array can be extended in custom folder to add new modules or change default fields


class stic_MessagesUtils {
    /* List of modules from which messages can be sent.
     * If new modules must be included, this list can be modified from a custom file
     */
    public static $messageableModules = array(
        'Contacts' => array('phoneField' => 'phone_mobile', 'name' => "concat(first_name, ' ', last_name)", 'dbTable' => 'contacts', 'nameFields' => array('first_name', 'last_name') ),
        'Accounts' => array('phoneField' => 'phone_office', 'name' => 'name', 'dbTable' => 'accounts', 'nameFields' => array('name')),
        'Leads' => array('phoneField' => 'phone_mobile', 'name' => "concat(first_name, ' ', last_name)", 'dbTable' => 'leads', 'nameFields' => array('first_name', 'last_name')),
        'Employees' => array('phoneField' => 'phone_mobile', 'name' => "concat(first_name, ' ', last_name)", 'dbTable' => 'users', 'nameFields' => array('first_name', 'last_name')),
        'Users' => array('phoneField' => 'phone_mobile', 'name' => "concat(first_name, ' ', last_name)", 'dbTable' => 'users', 'nameFields' => array('first_name', 'last_name')),
    );

    /** 
     * Returns the list of modules from which messages can be sent
     * @return array Modules enabled
     */
    public static function getMessageableModules() {
        $modules = array_keys(self::$messageableModules);
        asort($modules);
        return $modules;
    }

    /**
     * Generates the list of possible fields used on the sendMessage action for the module indicated
     * @param string Module name 
     * @return array List of fields to be shown on the action
     */
    public static function getRelatedMessageableFields($module) {
        global $beanList, $app_list_strings;
        $relPhoneFields = array();
        $checked_link = array();
        $msgModules = self::getMessageableModules();
        if ($module != '') {
            if (isset($beanList[$module]) && $beanList[$module]) {
                $mod = new $beanList[$module]();

                foreach ($mod->get_related_fields() as $field) {
                    if (isset($field['link'])) {
                        $checked_link[] = $field['link'];
                    }
                    if (!isset($field['module']) || !in_array($field['module'], $msgModules) || (isset($field['dbType']) && $field['dbType'] == "id")) {
                        continue;
                    }
                    $relPhoneFields[$field['link'] ? $field['link'] : $field['name']] = translate($field['module']) . ": "
                        . trim(translate($field['vname'], $mod->module_name), ":");
                }

                foreach ($mod->get_linked_fields() as $field) {
                    if (!in_array($field['name'], $checked_link) && !in_array($field['relationship'], $checked_link)) {
                        if (isset($field['module']) && $field['module'] != '') {
                            $rel_module = $field['module'];
                        } elseif ($mod->load_relationship($field['name'])) {
                            $relField = $field['name'];
                            $rel_module = $mod->$relField->getRelatedModuleName();
                        }

                        if (in_array($rel_module, $msgModules)) {
                            if (isset($field['vname']) && $field['vname'] != '') {
                                $relPhoneFields[$field['name']] = $app_list_strings['moduleList'][$rel_module] . ' : ' . translate($field['vname'], $mod->module_dir);
                            } else {
                                $relPhoneFields[$field['name']] = $app_list_strings['moduleList'][$rel_module] . ' : ' . $field['name'];
                            }
                        }
                    }
                }

                array_multisort($relPhoneFields, SORT_ASC, $relPhoneFields);
            }
        }
        return $relPhoneFields;
    }

    /**
     * Return the default phone to be used to send messages to bean received
     * @param object The bean
     * @return string The phone number
     */
    public static function getPhoneForMessage($bean) {
    
        $fieldName = self::$messageableModules[$bean->module_name]['phoneField'];
        if ($fieldName !== null){
            return $bean->$fieldName;
        }
        return '';
    }

    /**
     * Return the default phone field to be used to send messages to the module indicated
     * @param string Module name
     * @return string The field name
     */
    public static function getPhoneFieldNameForMessage($moduleName) {
    
        $fieldName = self::$messageableModules[$moduleName]['phoneField'];

        return $fieldName;
    }

    /**
     * Return the default phone field to be used to send messages to the module indicated
     * @param string Module name
     * @return string The field name
     */
    public static function getPhoneFieldForSql($moduleName, $prefix= '') {
        $db = DBManagerFactory::getInstance();

        $prefix = empty($prefix) ? self::$messageableModules[$moduleName]['dbTable'] : $prefix;
        // $fieldName = self::$messageableModules[$moduleName]['phoneField'];
        $fieldName = $db->concat($prefix, array(self::$messageableModules[$moduleName]['phoneField']), '&nbsp;');

        return $fieldName;
    }

    /** 
     * Gets the field to be used on a SQL query to retrieve the name, depending on the module 
     * @param string Module name
     * @return string The filed or function to be used in a SQL query
     */
    public static function getNameFieldNameForMessage($moduleName) {
    
        $fieldName = self::$messageableModules[$moduleName]['name'];

        return $fieldName;
    }
    /** 
     * Gets the field to be used on a SQL query to retrieve the name, depending on the module 
     * @param string Module name
     * @return string The filed or function to be used in a SQL query
     */
    public static function getNameFieldForSql($moduleName, $prefix='') {
        $db = DBManagerFactory::getInstance();

        $prefix = empty($prefix) ? self::$messageableModules[$moduleName]['dbTable'] : $prefix;
        // $fieldName = self::$messageableModules[$moduleName]['phoneField'];
        $fieldName = $db->concat($prefix, self::$messageableModules[$moduleName]['nameFields'], '&nbsp;');

        return $fieldName;


    
        $fieldName = self::$messageableModules[$moduleName]['name'];

        return $fieldName;
    }
        /** 
     * Gets the table name to be used on a SQL query to retrieve data from the module
     * @param string Module name
     * @return string The table name
     */
    public static function getTableNameForMessage($moduleName) {
    
        $tableName = self::$messageableModules[$moduleName]['dbTable'];

        return $tableName;
    }

    /** 
     * Function used to retrieve the messages subpanel data related to the bean being displayed.
     * @param array $params An array of parameters used to generate the query.
     * @return array|string The SQL query as an array if 'return_as_array' is true, or as a string otherwise.
     *
     */
    public static function get_stic_messages($type = null) {
        $beanId = $_REQUEST['record'];
        $statusList = $type['status']??'';
        $statusCond = empty($statusList)? '' : " and stic_messages.status IN ({$statusList})";
        $return_array['select'] = 'SELECT stic_messages.id ';
        $return_array['from'] = ' FROM stic_messages ';
        $return_array['where'] = " WHERE stic_messages.parent_id = '{$beanId}' {$statusCond} AND stic_messages.deleted = 0 ";
        $return_array['join'] = " ";
    
        if (isset($type) && ! empty($type['return_as_array'])) {
            return $return_array;
        }
    
        return $return_array['select'] . $return_array['from'] . $return_array['where'];
    }
    
    /**
     * Adds a JS function to the output which indicates if the module stic_Messages is active.
     *
     */
    public static function echoIsMessagesModuleActive() {
        require_once('modules/stic_Settings/Utils.php');
        require_once 'modules/MySettings/TabController.php';
        $controller = new TabController();
        $currentTabs = $controller->get_system_tabs();
        $active = 'false';
        if (!($currentTabs['stic_Messages'] ?? false)){
        }
        else {
            if (ACLController::checkAccess('stic_Messages', 'edit', true)) {
                $active = 'true';
            }
        }

        $messagesLimit = stic_SettingsUtils::getSetting('MESSAGES_LIMIT');

        echo "<script type='text/javascript'>function getMessagesActive() {return {$active};} function getMessagesLimit() {return {$messagesLimit};} </script>";
        echo getVersionedScript("modules/stic_Messages/stic_Messages.js");
    }

    public static function getWhatsAppWindowState(string $parentId, string $parentType): array
    {
        if (empty($parentId) || empty($parentType)) {
            return array('windowOpen' => false, 'hoursLeft' => 0, 'minutesLeft' => 0);
        }

        $db = DBManagerFactory::getInstance();
        $parentIdSafe = $db->quote($parentId);

        $sql = "SELECT sent_date
                FROM stic_messages
                WHERE parent_id = '{$parentIdSafe}'
                AND deleted = 0
                AND type = 'whatsapp'
                AND (
                    (direction = 'inbound' AND status = 'received')
                    OR (direction = 'outbound' AND template_id IS NOT NULL AND template_id != '' AND status = 'sent')
                )
                ORDER BY sent_date DESC
                LIMIT 1";

        $result = $db->query($sql);
        $lastMessage = $db->fetchByAssoc($result);

        $windowOpen = false;
        $hoursLeft = 0;
        $minutesLeft = 0;

        if ($lastMessage && !empty($lastMessage['sent_date'])) {
            $eventTs = (new DateTime($lastMessage['sent_date'], new DateTimeZone('UTC')))->getTimestamp();
            $nowTs = (new DateTime('now', new DateTimeZone('UTC')))->getTimestamp();
            $diffSeconds = $nowTs - $eventTs;
            $diffH = $diffSeconds / 3600;

            if ($diffH < 24) {
                $windowOpen = true;
                $secondsLeft = (24 * 3600) - $diffSeconds;
                $hoursLeft = floor($secondsLeft / 3600);
                $minutesLeft = floor(($secondsLeft % 3600) / 60);
            }
        }

        return array('windowOpen' => $windowOpen, 'hoursLeft' => $hoursLeft, 'minutesLeft' => $minutesLeft);
    }

    public static function fillDynamicListMessageTemplate($type = null)
    {
        $emailTemplatesFocus = BeanFactory::newBean('EmailTemplates');

        if ($type === null) {
            $typeRequest = $_REQUEST['type'] ?? 'sms';
        } else {
            $typeRequest = strtolower($type);
        }
        $type = $typeRequest;
        $emailTemplates = $emailTemplatesFocus->get_list("name", "email_templates.type='$type'", 0, -99, -99);

        $dynamic_email_template_list = array("" => translate("LBL_NONE", "app_strings"));

        foreach ($emailTemplates['list'] as $emailTemplate) {
            $dynamic_email_template_list[$emailTemplate->id] = $emailTemplate->name;
        }

        $GLOBALS['app_list_strings']['dynamic_message_template_list'] = $dynamic_email_template_list;
    }

    /**
     * Returns UI configuration for all registered message helpers.
     * Used by JavaScript to determine field locking and behavior.
     * 
     * @return array Associative array keyed by helper class name
     */
    public static function getHelpersUIConfig(): array {
        $helpers = self::getAvailableHelpers();
        $config = [];

        foreach ($helpers as $className) {
            $helper = self::instantiateHelper($className);
            if ($helper !== null) {
                $config[$helper->getHelperType()] = $helper->getUIConfig();
            }
        }

        return $config;
    }

    /**
     * Returns list of available helper class names.
     * 
     * @return array
     */
    public static function getAvailableHelpers(): array {
        $helpers = [];
        $paths = [
            'custom/modules/stic_Messages/Helpers/',
            'modules/stic_Messages/Helpers/',
        ];

        foreach ($paths as $path) {
            if (!is_dir($path)) {
                continue;
            }
            $files = glob($path . '*Helper.php');
            foreach ($files as $file) {
                $className = basename($file, '.php');
                // Skip the abstract base class
                if ($className === 'stic_MessagesHelper') {
                    continue;
                }
                if (!in_array($className, $helpers)) {
                    $helpers[] = $className;
                }
            }
        }

        return $helpers;
    }

    /**
     * Instantiates a helper class by name.
     * 
     * @param string $className
     * @return stic_MessagesHelper|null
     */
    public static function instantiateHelper(string $className): ?stic_MessagesHelper {
        $paths = [
            'custom/modules/stic_Messages/Helpers/',
            'modules/stic_Messages/Helpers/',
        ];

        foreach ($paths as $path) {
            $file = $path . $className . '.php';
            if (file_exists($file)) {
                require_once($file);
                if (class_exists($className)) {
                    $instance = new $className();
                    if ($instance instanceof stic_MessagesHelper) {
                        return $instance;
                    }
                }
            }
        }

        return null;
    }

    /**
     * Returns list of message types that cannot be retried.
     * Used by controller to build SQL WHERE clause.
     * 
     * @return array List of type names that are not retryable
     */
    public static function getNonRetryableTypes(): array {
        $helpers = self::getAvailableHelpers();
        $nonRetryable = [];

        foreach ($helpers as $className) {
            $helper = self::instantiateHelper($className);
            if ($helper !== null && !$helper->isRetryable()) {
                $nonRetryable[] = $helper->getHelperType();
            }
        }

        return $nonRetryable;
    }

    /**
     * Finds the helper class name for a given channel type.
     * Uses direct class name mapping from config, overridable in config_override.php.
     * 
     * @param string $type The channel type (e.g., 'whatsapp', 'sms', 'whatsapp_web', 'private_area')
     * @return string|null The helper class name or null if not found
     */
    public static function getHelperClassForType(string $type): ?string {
        global $sugar_config;

        $defaultProviders = [
            'whatsapp' => 'TwilioWhatsAppHelper',
            'sms' => 'SevenSmsHelper',
            'whatsapp_web' => 'WhatsAppWebHelper',
            'private_area' => 'PrivateAreaHelper',
        ];
        $providers = array_merge(
            $defaultProviders,
            $sugar_config['stic_message_providers'] ?? []
        );
        $className = $providers[$type] ?? null;

        if ($className !== null && self::instantiateHelper($className) !== null) {
            return $className;
        }

        return null;
    }

    /**
     * Instantiates a helper by its channel type.
     * Convenience method that combines getHelperClassForType() + instantiateHelper().
     * 
     * @param string $type The channel type (e.g., 'whatsapp', 'sms', 'whatsapp_web', 'private_area')
     * @return stic_MessagesHelper|null
     */
    public static function instantiateHelperByType(string $type): ?stic_MessagesHelper {
        $className = self::getHelperClassForType($type);
        if ($className === null) {
            return null;
        }
        return self::instantiateHelper($className);
    }
}
