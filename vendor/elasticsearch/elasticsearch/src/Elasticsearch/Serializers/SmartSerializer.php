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

namespace Elasticsearch\Serializers;

use Elasticsearch\Common\Exceptions;
use Elasticsearch\Common\Exceptions\Serializer\JsonErrorException;
use JsonException;

if (!defined('JSON_INVALID_UTF8_SUBSTITUTE')) {
    //PHP < 7.2 Define it as 0 so it does nothing
    define('JSON_INVALID_UTF8_SUBSTITUTE', 0);
}

class SmartSerializer implements SerializerInterface
{
    /**
     * {@inheritdoc}
     */
    public function serialize($data): string
    {
        if (is_string($data) === true) {
            return $data;
        } else {
            $data = json_encode($data, JSON_PRESERVE_ZERO_FRACTION + JSON_INVALID_UTF8_SUBSTITUTE);
            if ($data === false) {
                throw new Exceptions\RuntimeException("Failed to JSON encode: ".json_last_error_msg());
            }
            if ($data === '[]') {
                return '{}';
            } else {
                return $data;
            }
        }
    }

    /**
     * {@inheritdoc}
     */
    public function deserialize(?string $data, array $headers)
    {
        if (isset($headers['content_type']) === true) {
            if (strpos($headers['content_type'], 'json') !== false) {
                return $this->decode($data);
            } else {
                //Not json, return as string
                return $data;
            }
        } else {
            //No content headers, assume json
            return $this->decode($data);
        }
    }

    /**
     * @todo For 2.0, remove the E_NOTICE check before raising the exception.
     *
     * @param string|null $data
     *
     * @return array
     * @throws JsonErrorException
     */
    private function decode(?string $data): array
    {
        if ($data === null || strlen($data) === 0) {
            return [];
        }

        if (version_compare(PHP_VERSION, '7.3.0') >= 0) {
            try {
                $result = json_decode($data, true, 512, JSON_THROW_ON_ERROR);
                return $result;
            } catch (JsonException $e) {
                switch ($e->getCode()) {
                    case JSON_ERROR_UTF16:
                        try {
                            $data = str_replace('\\', '\\\\', $data);
                            $result = json_decode($data, true, 512, JSON_THROW_ON_ERROR);
                            return $result;
                        } catch (JsonException $e) {
                            throw new JsonErrorException($e->getCode(), $data, []);
                        }
                }
                throw new JsonErrorException($e->getCode(), $data, []);
            }
        }

        $result = @json_decode($data, true);
        // Throw exception only if E_NOTICE is on to maintain backwards-compatibility on systems that silently ignore E_NOTICEs.
        if (json_last_error() !== JSON_ERROR_NONE && (error_reporting() & E_NOTICE) === E_NOTICE) {
            throw new JsonErrorException(json_last_error(), $data, $result);
        }
        return $result;
    }
}
