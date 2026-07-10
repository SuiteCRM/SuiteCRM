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

/**
 * Service class responsible for discovering available actions by scanning predefined directories.
 * It handles the loading of action definitions, ensuring that custom overrides are respected.
 */
class ActionDiscoveryService {
    /** @var string[] Paths to scan for action definitions */
    private static array $basePaths = [
        'modules/stic_AWF_Forms/actions/',
        'custom/modules/stic_AWF_Forms/actions/',
    ];

    /**
     * Scans configured paths to discover actions, handling overrides in custom paths.
     * @param ActionType[] $availableTypes Action types to discover (empty for all)
     * @param ?string $formType Optional form type to filter actions by (null = no filtering)
     * @return ActionDefinition[] Discovered actions
     */
    public static function discoverActions(array $availableTypes = [], ?string $formType = null): array {
        $discoveredFiles = [];
        if (empty($availableTypes)) {
            $availableTypes = ActionType::cases();
        }

        // Iterate over base directories: core -> custom - > custom/Extension
        foreach (self::$basePaths as $basePath) {
            if (!is_dir($basePath)) {
                continue;
            }
            // Iterate dynamically over action types
            foreach ($availableTypes as $actionType) {
                $typePath = $basePath . $actionType->value . '/';
                if (is_dir($typePath)) {
                    $searchPath = $typePath.'*.php';
                    foreach (glob($searchPath) as $filePath) {
                        $actionName = pathinfo($filePath, PATHINFO_FILENAME); // Ex: 'SaveRecordAction'
                        $discoveredFiles[$actionName] = $filePath; // The last found file (higher priority) overwrites previous ones
                    }
                }
            }
        }

        // Instantiate the discovered action definitions
        $discoveredActions = [];
        foreach ($discoveredFiles as $actionName => $filePath) {
            try {
                // If there is a syntax error, a ParseError will be thrown, caught by catch (\Throwable $t)
                require_once($filePath);
                if (!class_exists($actionName)) {
                    throw new \RuntimeException("Line ".__LINE__.": ".__METHOD__.": File {$filePath} exists, but class {$actionName} is not defined.");
                }
                $reflection = new \ReflectionClass($actionName);
                if ($reflection->isAbstract() || !$reflection->isSubclassOf(ActionDefinition::class)) { 
                    $GLOBALS['log']->warn("Line ".__LINE__.": ".__METHOD__.": Class {$actionName} is abstract or not a subclass of ActionDefinition.");
                    continue; 
                }
                /** @var ActionDefinition $actionInstance */
                $actionInstance = new $actionName();
                if ($actionInstance->isActive) {
                    $discoveredActions[$actionName] = $actionInstance;
                }

            } catch (\Throwable $t) {
                $GLOBALS['log']->error("Line ".__LINE__.": ".__METHOD__.": Error discovering the action {$actionName}: " . $t->getMessage());
            }
        }

        // Filter by form type if specified
        if ($formType !== null) {
            $discoveredActions = array_filter($discoveredActions, function (ActionDefinition $action) use ($formType) {
                // If supportedFormTypes is empty, the action supports all form types
                return empty($action->supportedFormTypes) || in_array($formType, $action->supportedFormTypes);
            });
        }

        return array_values($discoveredActions);
    }

}