<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * Factory responsible for creating calendar provider instances.
 * Separates instance creation concerns from the CalendarProviderType value object.
 */
class CalendarProviderInstanceFactory
{

    /**
     * Creates an instance of a calendar provider if the provided type is enabled.
     *
     * @param CalendarProviderType $type The type of the calendar provider to create.
     * @param CalendarAccount $connection The connection details for the calendar account.
     * @return AbstractCalendarProvider|null Returns an instance of the calendar provider or null if the type is not enabled or an error occurs.
     */
    public function createInstance(CalendarProviderType $type, CalendarAccount $connection): ?AbstractCalendarProvider
    {
        if (!$type->isEnabled()) {
            $GLOBALS['log']->debug("[CalendarProviderInstanceFactory][createInstance] Provider type '{$type->getName()}' is disabled");
            return null;
        }

        try {
            $instance = $this->instantiateProvider($type);
            $instance->setConnection($connection);
            return $instance;
        } catch (Throwable $e) {
            $GLOBALS['log']->error("[CalendarProviderInstanceFactory][createInstance] Failed to create provider instance for '{$type->getName()}': " . $e->getMessage());
            return null;
        }
    }

    /**
     * Instantiates a calendar provider based on the specified type.
     *
     * @param CalendarProviderType $type The type of the calendar provider to instantiate, including its file path and class name.
     * @return AbstractCalendarProvider Returns an instance of the calendar provider.
     * @throws InvalidArgumentException If the provider file does not exist, the provider class is not found, or the provider class does not extend AbstractCalendarProvider.
     */
    protected function instantiateProvider(CalendarProviderType $type): AbstractCalendarProvider
    {
        $fullPath = $type->getFile();

        if (!file_exists($fullPath)) {
            throw new InvalidArgumentException("Provider file not found: $fullPath");
        }

        require_once $fullPath;

        $className = $type->getClass();
        if (!class_exists($className)) {
            throw new InvalidArgumentException("Provider class not found: $className");
        }

        $instance = new $className();

        if (!$instance instanceof AbstractCalendarProvider) {
            throw new InvalidArgumentException("Provider class must extend AbstractCalendarProvider: $className");
        }

        return $instance;
    }

}