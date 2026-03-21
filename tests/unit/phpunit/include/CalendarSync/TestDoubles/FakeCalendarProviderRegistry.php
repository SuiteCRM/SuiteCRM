<?php
/**
 * In-memory stub for CalendarProviderRegistry.
 *
 * Provides canned responses for provider lookups without loading actual providers.
 * Replaces production registry which loads and manages calendar provider instances.
 *
 * Key differences from production:
 * - Pre-configured with outlook and google providers
 * - Returns auth methods but not provider instances
 * - No actual provider class loading or instantiation
 * - Supports dynamic provider registration via addProvider()
 *
 * Pre-configured providers:
 * - outlook: oauth2
 * - google: oauth2
 *
 * Limitations:
 * - getProviderForAccount() always returns null
 * - No connection testing capability
 * - No actual calendar API integration
 *
 * Use this test double when testing code that queries provider information
 * but does not need actual provider instances.
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/CalendarSync/infrastructure/registry/CalendarProviderRegistry.php';

class FakeCalendarProviderRegistry extends CalendarProviderRegistry
{
    private array $configuredProviders = [
        'outlook' => 'oauth2',
        'google' => 'oauth2',
    ];

    public function addProvider(string $source, string $authMethod): void
    {
        if (empty($source)) {
            throw new InvalidArgumentException('Source cannot be empty');
        }

        if (empty($authMethod)) {
            throw new InvalidArgumentException('Auth method cannot be empty');
        }

        $this->configuredProviders[$source] = $authMethod;
    }

    public function getAuthMethodForSource(string $source): ?string
    {
        if (empty($source)) {
            return null;
        }

        return $this->configuredProviders[$source] ?? null;
    }

    public function getProviderForAccount(CalendarAccount $account): ?AbstractCalendarProvider
    {
        if (empty($account->source)) {
            return null;
        }

        if (!isset($this->configuredProviders[$account->source])) {
            return null;
        }

        return null;
    }

    public function getCalendarSourceTypes(): array
    {
        return array_map(
            fn($source) => ['key' => $source, 'value' => ucfirst($source)],
            array_keys($this->configuredProviders)
        );
    }

    public function clear(): void
    {
        $this->configuredProviders = [
            'outlook' => 'oauth2',
            'google' => 'oauth2',
        ];
    }
}
