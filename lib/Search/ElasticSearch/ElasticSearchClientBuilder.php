<?php
/**
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

namespace SuiteCRM\Search\ElasticSearch;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use Elasticsearch\ClientBuilder;

/**
 * Class ElasticSearchClientBuilder generates a configured Elasticsearch client.
 */
#[\AllowDynamicProperties]
class ElasticSearchClientBuilder
{
    private static $hosts;

    /**
     * Returns a preconfigured elasticsearch client.
     *
     * @return \Elasticsearch\Client
     */
    public static function getClient()
    {
        if (empty(self::$hosts)) {
            self::$hosts = self::loadFromSugarConfig();
        }

        $client = ClientBuilder::create()->setHosts(self::$hosts)->build();

        return $client;
    }

    /** @noinspection PhpUnusedPrivateMethodInspection */

    /**
     * Perform a series of standardizations and checks to make sure that the host is valid.
     *
     * Especially useful when reading user inputs.
     *
     * @param array|string $host
     *
     * @return array
     */
    public static function sanitizeHost($host)
    {
        if (is_string($host)) {
            $host = ['host' => $host];
        }

        if (!isset($host['host'])) {
            throw new \InvalidArgumentException('Host URL cannot be empty');
        }

        $hostURL = $host['host'];

        if (!is_string($hostURL)) {
            throw new \InvalidArgumentException('Host URL must be a string');
        }

        $hostURL = self::addHttp($hostURL, isset($host['scheme']) ? $host['scheme'] : 'http');

        $parsedHost = parse_url($hostURL);

        if (!is_array($parsedHost)) {
            throw new \InvalidArgumentException("Failed to parse Host URL '$hostURL'");
        }

        $merged = array_merge($host, $parsedHost);

        if (isset($merged['scheme']) && $merged['scheme'] === 'http') {
            unset($merged['scheme']);
        }

        unset($merged['path'], $merged['query']);

        return $merged;
    }

    /** @noinspection PhpUnusedPrivateMethodInspection */
    /**
     * Loads config from a json file.
     *
     * @param $file
     *
     * @return array
     */
    private static function loadFromFile($file)
    {
        if (!file_exists($file)) {
            return self::loadDefaultConfig();
        }

        $results = file_get_contents($file);

        if ($results === false) {
            return self::loadDefaultConfig();
        }

        $hosts = json_decode($results, true);

        return self::loadFromArray($hosts);
    }

    /**
     * Sanitizes an array of configured hosts.
     *
     * @param $hosts
     *
     * @return array
     */
    private static function loadFromArray($hosts)
    {
        $sanitized = [];

        foreach ($hosts as $host) {
            $sanitized[] = self::sanitizeHost($host);
        }

        return $sanitized;
    }

    /**
     * Loads Elasticsearch client configuration from the $sugar_config global.
     *
     * @return array
     */
    private static function loadFromSugarConfig()
    {
        global $sugar_config;

        $host = $sugar_config['search']['ElasticSearch']['host'] ?? '';
        $user = $sugar_config['search']['ElasticSearch']['user'] ?? '';
        $pass = $sugar_config['search']['ElasticSearch']['pass'] ?? '';

        $host = trim($host);
        $user = trim($user);

        if (empty($user)) {
            return [self::sanitizeHost($host)];
        }

        return [
            self::sanitizeHost([
                'host' => $host,
                'user' => $user,
                'pass' => $pass,
            ]),
        ];
    }

    /**
     * Returns the default connection (['127.0.0.1']).
     *
     * @return array
     */
    private static function loadDefaultConfig()
    {
        return [self::sanitizeHost('127.0.0.1')];
    }

    /**
     * Adds the scheme to url lacking of it.
     *
     * @param string $url
     * @param string $scheme
     *
     * @return string
     */
    private static function addHttp($url, $scheme = 'http')
    {
        if (!preg_match("~^(?:f|ht)tps?://~i", $url)) {
            $url = $scheme . '://' . $url;
        }

        return $url;
    }
}
