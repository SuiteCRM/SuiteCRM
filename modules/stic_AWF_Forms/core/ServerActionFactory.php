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
// Prevents directly accessing this file from a web browser
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

class ServerActionFactory {

    public function createAction(FormAction $actionConfig): ServerActionDefinition {
        
        $actionName = $actionConfig->name;    // Ex: 'SaveRecordAction' (file and class name without extension)
        $className = $actionName;             // Ex: 'SaveRecordAction' (class name)

        // Define search paths with priority: custom/Extension > custom > core
        // For each level, search Hook before Deferred
        $searchPaths = [
            'custom/Extension/modules/stic_AWF_Forms/actions/Hook/' . $actionName . '.php',
            'custom/Extension/modules/stic_AWF_Forms/actions/Deferred/' . $actionName . '.php',
            'custom/modules/stic_AWF_Forms/actions/Hook/' . $actionName . '.php',
            'custom/modules/stic_AWF_Forms/actions/Deferred/' . $actionName . '.php',
            'modules/stic_AWF_Forms/actions/Hook/' . $actionName . '.php',
            'modules/stic_AWF_Forms/actions/Deferred/' . $actionName . '.php',
        ];

        $filePath = null;

        // Search the file in the defined paths
        foreach ($searchPaths as $path) {
            if (file_exists($path)) { 
                $filePath = $path;
                break;
            }
        }

        if (!$filePath) {
            throw new \InvalidArgumentException("There is no file with definition for Action with Name: {$className}");
        }

        require_once($filePath);
        if (!class_exists($className)) {
            throw new \RuntimeException("There is a file '{$filePath}', but do not has definition for Action with Name: '{$className}'");
        }
        
        // check that the class is a subclass of ServerActionDefinition
        $reflection = new \ReflectionClass($className);
        if (!$reflection->isSubclassOf(ServerActionDefinition::class)) {
            throw new \RuntimeException("The class '{$className}' is not a subclass of 'ServerActionDefinition'.");
        }

        /** @var ServerActionDefinition $action */
        $action = new $className();
        
        return $action;
    }
}