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

require_once __DIR__.'/stic_AWF_PaymentStrategy.php';
require_once __DIR__.'/stic_AWF_OfflineStrategy.php';

class stic_AWF_PaymentStrategyFactory
{
    // Map payment prefixes to special treatment
    // From stic_payments_methods_list: Payment platform Strategies
    protected static $strategyMap = array(
        'bizum' => 'stic_AWF_RedsysStrategy',    // RedSys Bizum
        'stripe' => 'stic_AWF_StripeStrategy',   // Stripe
        'card' => 'stic_AWF_RedsysStrategy',     // RedSys Card
        'ceca_card' => 'stic_AWF_CecaStrategy',  // CECA Card
        'paypal' => 'stic_AWF_PaypalStrategy',   // PayPal
    );

    /**
     * Creates and configures the strategy instance based on the method value.
     *  @param string $methodValue Ex: 'card_futbol', 'transfer', 'cash'
     *  @return stic_AWF_PaymentStrategy
     *  @throws Exception If the class does not exist
     */

    public static function createFromMethodValue(string $methodValue): stic_AWF_PaymentStrategy
    {
        $strategyClass = 'stic_AWF_OfflineStrategy'; // Default
        $suffix = null;

        // Detect Prefix (to get strategy) and Suffix
        foreach (self::$strategyMap as $prefix => $class) {
            if ($methodValue === $prefix || strpos($methodValue, $prefix . '_') === 0) {
                $strategyClass = $class;
                
                // If it is longer, extract the suffix (ex: 'card_football' -> 'FOOTBALL')
                if (strlen($methodValue) > strlen($prefix)) {
                    $part = substr($methodValue, strlen($prefix) + 1);
                    if ($part !== false && $part !== '') {
                        $suffix = strtoupper($part);
                    }
                }
                break; 
            }
        }

        // Instantiation
        if (file_exists(__DIR__."/{$strategyClass}.php")) {
            require_once __DIR__."/{$strategyClass}.php";
        }
        if (!class_exists($strategyClass)) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": stic_AWF_PaymentStrategyFactory: Class $strategyClass not found. Fallback to Offline.");
            $strategyClass = 'stic_AWF_OfflineStrategy';
        }

        /** @var stic_AWF_PaymentStrategy $strategy */
        $strategy = new $strategyClass();

        // Configure Strategy with suffix
        if ($suffix) {
            $strategy->setSuffix($suffix);
        }

        return $strategy;
    }
    
    /**
     * Recreate the strategy from the saved data (for the Webhook or Terminal phase).
     */
    public static function createFromStoredData(array $data): stic_AWF_PaymentStrategy
    {
        $strategyClass = $data['strategy_class'] ?? null;
        $suffix = $data['strategy_suffix'] ?? null;

        // Instantiation
        if (!$strategyClass) {
            throw new Exception("Payment strategy class not defined in stored data.");
        }
        
        // Handle full class name: "stic_AWF_RedsysStrategy" -> "stic_AWF_RedsysStrategy.php"
        $filePath = __DIR__ . '/' . $strategyClass . '.php';
        
        if (file_exists($filePath)) {
            require_once $filePath;
        } elseif (!class_exists($strategyClass)) {
            throw new Exception("Unknown payment strategy class in stored data: " . $strategyClass);
        }

        /** @var stic_AWF_PaymentStrategy $strategy */
        $strategy = new $strategyClass();

        // Configure Strategy with suffix
        if ($suffix) {
            $strategy->setSuffix($suffix);
        }

        return $strategy;
    }

    /**
     * Creates a strategy instance from a webhook source identifier.
     * Used by WebhookHandler when no ticket is found (e.g. Stripe recurring events).
     *
     * @param string $source The source identifier from the webhook URL (e.g. 'stripe', 'redsys')
     * @return stic_AWF_PaymentStrategy The strategy instance, or null if source is unknown
     */
    public static function createFromSource(string $source): ?stic_AWF_PaymentStrategy
    {
        // Try each known strategy class to find one matching the source
        $strategyClasses = [
            'stic_AWF_RedsysStrategy',
            'stic_AWF_CecaStrategy',
            'stic_AWF_StripeStrategy',
            'stic_AWF_PaypalStrategy',
        ];

        foreach ($strategyClasses as $strategyClass) {
            $filePath = __DIR__ . '/' . $strategyClass . '.php';
            if (file_exists($filePath)) {
                require_once $filePath;
            }
            if (class_exists($strategyClass) && $strategyClass::getSourceName() === $source) {
                return new $strategyClass();
            }
        }

        return null;
    }

    /**
     * Extracts the external transaction ID using the strategy that matches the given source.
     *
     * @param string $source The source identifier from the webhook URL
     * @param array $rawData POST data array
     * @param string $rawBody Raw request body
     * @param array $headers the headers received
     * 
     * @return string|null The external transaction ID or null
     */
    public static function extractExternalIdBySource(string $source, array $rawData, string $rawBody, array $headers): ?string
    {
        $strategy = self::createFromSource($source);
        if ($strategy !== null) {
            return $strategy::extractExternalId($rawData, $rawBody, $headers);
        }
        return null;
    }
}