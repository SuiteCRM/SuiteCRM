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

require_once 'include/CalendarSync/domain/CalendarProviderType.php';
require_once 'include/CalendarSync/infrastructure/providers/SuiteCRMInternalCalendarProvider.php';
require_once 'include/CalendarSync/domain/services/factories/CalendarProviderTypeFactory.php';
require_once 'include/CalendarSync/domain/services/factories/CalendarProviderInstanceFactory.php';

/**
 * Class CalendarProviderRegistry
 *
 * Registry for managing calendar provider types.
 * Defines the contract for retrieving and managing provider type data.
 *
 * Manages calendar provider definitions and their associated types.
 * This registry is responsible for discovering, loading, and processing
 * calendar providers from predefined paths, as well as testing connections
 * and retrieving providers for specific calendar accounts.
 */
class CalendarProviderRegistry
{

    protected const EXTENSION_BASE_PATH = 'include/CalendarSync/Extension/CalendarProviders';
    protected const LANGUAGE_EXTENSION_PATH = 'custom/Extension/application/Ext/Language';
    /** @var array<string, CalendarProviderType>|null */
    protected static array|null $cachedProviders = null;
    /** @var array<string, CalendarProviderType> */
    protected array $providers = [];

    public function __construct(
        protected readonly CalendarProviderTypeFactory $calendarProviderTypeFactory = new CalendarProviderTypeFactory(),
        protected readonly CalendarProviderInstanceFactory $providerInstanceFactory = new CalendarProviderInstanceFactory()
    ) {
        $this->loadProviders();
        try {
            $this->writeCalendarSourceTypesToExtension();
        } catch (Throwable $e) {
            $GLOBALS['log']->error("[CalendarProviderRegistry][__construct] Failed to write calendar source types to extension: " . $e->getMessage());
        }
    }

    /**
     * Loads calendar provider definitions into the registry.
     * If cached providers are available, uses the cached data.
     * Otherwise, it discovers and registers the providers.
     *
     * @return void
     */
    protected function loadProviders(): void
    {
        if (self::$cachedProviders !== null) {
            $this->providers = self::$cachedProviders;
            return;
        }

        $this->discoverProviders();
        $GLOBALS['log']->debug("[CalendarProviderRegistry][loadProviders] Loaded " . count($this->providers) . " providers");
        self::$cachedProviders = $this->providers;
    }

    /**
     * Discovers and registers calendar providers by loading configuration files
     * from specified directory paths. Validates and processes the structure of
     * each provider before registering.
     *
     * @return void
     */
    protected function discoverProviders(): void
    {
        $GLOBALS['log']->debug("[CalendarProviderRegistry][discoverProviders] Starting provider discovery");
        $paths = [
            self::EXTENSION_BASE_PATH,
            'custom/' . self::EXTENSION_BASE_PATH,
        ];

        foreach ($paths as $path) {
            $realPath = realpath($path);
            if (!$realPath || !is_dir($realPath)) {
                continue;
            }

            $files = glob($realPath . '/*.php');
            if ($files === false) {
                return;
            }

            foreach ($files as $file) {
                $realFile = realpath($file);
                if (!$realFile) {
                    continue;
                }

                try {
                    $calendarProviders = [];

                    $result = include $realFile;

                    if ($result === false) {
                        $GLOBALS['log']->error("[CalendarProviderRegistry][discoverProviders] Failed to include calendar provider: " . $realFile);
                        continue;
                    }

                    if (empty($calendarProviders) || !is_array($calendarProviders)) {
                        continue;
                    }

                    foreach ($calendarProviders as $key => $providerArray) {
                        if (!is_string($key) || !is_array($providerArray)) {
                            continue;
                        }

                        try {
                            $provider = $this->calendarProviderTypeFactory->fromArray($providerArray);
                        } catch (Throwable $e) {
                            $GLOBALS['log']->error("[CalendarProviderRegistry][discoverProviders] Invalid provider structure in file: " . $realFile);
                            continue;
                        }

                        if (!$provider->isEnabled()) {
                            $GLOBALS['log']->debug("[CalendarProviderRegistry][discoverProviders] Skipping disabled provider: $key");
                            continue;
                        }

                        $GLOBALS['log']->debug("[CalendarProviderRegistry][discoverProviders] Registered provider: $key");
                        $this->providers[$key] = $provider;
                    }

                } catch (ParseError $e) {
                    $GLOBALS['log']->error("[CalendarProviderRegistry][discoverProviders] Parse error in calendar provider file $realFile: " . $e->getMessage());
                } catch (Error $e) {
                    $GLOBALS['log']->error("[CalendarProviderRegistry][discoverProviders] Fatal error in calendar provider file $realFile: " . $e->getMessage());
                } catch (Throwable $e) {
                    $GLOBALS['log']->error("[CalendarProviderRegistry][discoverProviders] Exception in calendar provider file $realFile: " . $e->getMessage());
                }
            }
        }
    }

    /**
     * Writes calendar source types to the dedicated language extension file and triggers
     * a language rebuild so the consolidated file is updated safely.
     * Only writes when the source types have actually changed (new providers, modifications, or removals).
     *
     * @return void
     */
    protected function writeCalendarSourceTypesToExtension(): void {
        require_once 'include/utils.php';
        require_once 'include/utils/file_utils.php';

        $language = get_current_language();
        $calendarSourceTypes = $this->getCalendarSourceTypes();

        if (empty($calendarSourceTypes)) {
            return;
        }

        if ($this->languageExtensionMatchesSourceTypes($language, $calendarSourceTypes)) {
            return;
        }

        $this->writeLanguageExtension($language, $calendarSourceTypes);
        $this->rebuildLanguageCache($language);
    }

    /**
     * Returns the path to the dedicated language extension file for the calendar registry.
     *
     * @param string $language The language code (e.g. 'en_us')
     *
     * @return string
     */
    protected function getLanguageExtensionPath(string $language): string {
        return self::LANGUAGE_EXTENSION_PATH . "/$language.registry_calendar.php";
    }

    /**
     * Checks whether the existing language extension file already contains the given source types.
     *
     * @param string                $language            The language code
     * @param array<string, string> $calendarSourceTypes The current source types from the registry
     *
     * @return bool
     */
    protected function languageExtensionMatchesSourceTypes(string $language, array $calendarSourceTypes): bool {
        $path = $this->getLanguageExtensionPath($language);

        if (!file_exists($path)) {
            return false;
        }

        $app_list_strings = [];
        include $path;

        $existingTypes = $app_list_strings['calendar_source_types'] ?? [];

        ksort($existingTypes);
        ksort($calendarSourceTypes);

        return $existingTypes === $calendarSourceTypes;
    }

    /**
     * Writes calendar source types to the dedicated language extension file.
     *
     * @param string                $language            The language code
     * @param array<string, string> $calendarSourceTypes The source types to write
     *
     * @return void
     */
    protected function writeLanguageExtension(string $language, array $calendarSourceTypes): void {
        global $log;
        $path = $this->getLanguageExtensionPath($language);

        if (!is_dir(dirname($path))
            && !mkdir($concurrentDirectory = dirname($path), 0755, true)
            && !is_dir($concurrentDirectory)) {
            throw new RuntimeException(sprintf('Directory "%s" was not created', $concurrentDirectory));
        }

        $theName = "app_list_strings['calendar_source_types']";
        write_override_label_to_file($theName, $calendarSourceTypes, $path);

        $log->debug("[CalendarProviderRegistry][writeLanguageExtension] Updated language extension for $language");
    }

    /**
     * Rebuilds the consolidated language file and clears the cache.
     *
     * @param string $language The language code to rebuild
     *
     * @return void
     */
    protected function rebuildLanguageCache(string $language): void {
        global $log;
        require_once 'ModuleInstall/ModuleInstaller.php';

        $mi = new ModuleInstaller();
        $mi->silent = true;
        $mi->rebuild_languages([$language => $language]);

        sugar_cache_clear('app_list_strings.' . $language);

        $log->debug("[CalendarProviderRegistry][rebuildLanguageCache] Rebuilt language cache for $language");
    }

    /**
     * Get available calendar source types as key-value pairs.
     * Used for UI dropdowns and form elements.
     *
     * @return array<string, string> Array with source keys and display names
     */
    public function getCalendarSourceTypes(): array
    {
        return array_map(
            static fn($provider) => $provider->getName(), $this->findEnabled()
        );
    }

    /**
     * Retrieve only enabled provider types.
     *
     * @return array<string, CalendarProviderType> Array of enabled provider types indexed by source
     */
    public function findEnabled(): array
    {
        return array_filter(
            $this->findAll(),
            static fn(CalendarProviderType $provider) => $provider->isEnabled()
        );
    }

    /**
     * Retrieve all registered provider types.
     *
     * @return array<string, CalendarProviderType> Array of provider types indexed by source
     */
    public function findAll(): array
    {
        return $this->providers;
    }

    /**
     * Retrieves the appropriate calendar provider for the given account.
     * Determines the provider type based on the account's source and
     * returns an instance of the corresponding provider if available.
     *
     * @param CalendarAccount $account The calendar account for which the provider is being retrieved.
     * @return AbstractCalendarProvider|null The calendar provider instance if found, or null if no suitable provider is available.
     */
    public function getProviderForAccount(CalendarAccount $account): ?AbstractCalendarProvider
    {
        $providerType = $this->findBySource($account->source);
        if (!$providerType) {
            return null;
        }

        return $this->providerInstanceFactory->createInstance($providerType, $account);
    }

    /**
     * Find a provider type by its source identifier.
     *
     * @param string $source The source identifier to search for
     * @return CalendarProviderType|null The provider type if found, null otherwise
     */
    public function findBySource(string $source): ?CalendarProviderType
    {
        if (!$this->exists($source)) {
            return null;
        }

        $providerOfSource = $this->providers[$source];

        return $providerOfSource->isEnabled() ? $providerOfSource : null;

    }

    /**
     * Check if a provider type exists for the given source.
     *
     * @param string $source The source identifier to check
     * @return bool True if the provider type exists, false otherwise
     */
    public function exists(string $source): bool
    {
        return isset($this->providers[$source]);
    }

    /**
     * Returns an internal calendar provider configured for the given calendar account.
     *
     * @param CalendarAccount $account The calendar account to establish the connection for the provider.
     * @return AbstractCalendarProvider The configured internal calendar provider instance.
     */
    public function getInternalProviderForAccount(CalendarAccount $account): AbstractCalendarProvider
    {
        $internalProvider = new SuiteCRMInternalCalendarProvider();
        $internalProvider->setConnection($account);
        return $internalProvider;
    }

    /**
     * Get the authentication method for a specific source.
     *
     * @param string $source The source identifier
     * @return string|null The authentication method or null if not found
     */
    public function getAuthMethodForSource(string $source): ?string
    {
        return $this->findBySource($source)?->getAuthMethod();
    }

}
