<?php
/**
 * Elasticsearch PHP client
 *
 * @link      https://github.com/elastic/elasticsearch-php/
 * @copyright Copyright (c) Elasticsearch B.V (https://www.elastic.co)
 * @license   http://www.apache.org/licenses/LICENSE-2.0 Apache License, Version 2.0
 * @license   https://www.gnu.org/licenses/lgpl-2.1.html GNU Lesser General Public License, Version 2.1 
 * 
 * Licensed to Elasticsearch B.V under one or more agreements.
 * Elasticsearch B.V licenses this file to you under the Apache 2.0 License or
 * the GNU Lesser General Public License, Version 2.1, at your option.
 * See the LICENSE file in the project root for more information.
 */
declare(strict_types = 1);

namespace Elasticsearch;

class Utility
{
    const ENV_URL_PLUS_AS_SPACE = 'ELASTIC_CLIENT_URL_PLUS_AS_SPACE';

    /**
     * Get the ENV variable with a thread safe fallback criteria
     * @see https://github.com/elastic/elasticsearch-php/issues/1237
     * 
     * @return string | false
     */
    public static function getEnv(string $env)
    {
        return $_SERVER[$env] ?? $_ENV[$env] ?? getenv($env);
    }

    /**
     * Encode a string in URL using urlencode() or rawurlencode()
     * according to env variable ES_URL_PLUS_AS_SPACE.
     * If ES_URL_PLUS_AS_SPACE is true use urlencode(), otherwise rawurlencode()
     * 
     * @see https://github.com/elastic/elasticsearch-php/issues/1278
     */
    public static function urlencode(string $url): string
    {
        $plusAsSpace = self::getEnv(self::ENV_URL_PLUS_AS_SPACE);
        return $plusAsSpace === false || $plusAsSpace === 'true'
            ? urlencode($url)
            : rawurlencode($url);
    }
}
