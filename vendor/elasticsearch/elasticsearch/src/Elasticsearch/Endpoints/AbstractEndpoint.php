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

namespace Elasticsearch\Endpoints;

use Elasticsearch\Common\Exceptions\UnexpectedValueException;
use Elasticsearch\Serializers\SerializerInterface;
use Elasticsearch\Utility;

abstract class AbstractEndpoint
{
    /**
     * @var array
     */
    protected $params =[];

    /**
     * @var string
     */
    protected $index = null;

    /**
     * @var string
     */
    protected $type = null;

    /**
     * @var string|int
     */
    protected $id = null;

    /**
     * @var string
     */
    protected $method = null;

    /**
     * @var string|array
     */
    protected $body = null;

    /**
     * @var array
     */
    private $options = [];

    /**
     * @var SerializerInterface
     */
    protected $serializer;

    /**
     * @return string[]
     */
    abstract public function getParamWhitelist(): array;

    /**
     * @return string
     */
    abstract public function getURI(): string;

    /**
     * @return string
     */
    abstract public function getMethod(): string;


    /**
     * Set the parameters for this endpoint
     *
     * @param  string[] $params Array of parameters
     * @return $this
     */
    public function setParams(array $params)
    {
        $this->extractOptions($params);
        $this->checkUserParams($params);
        $params = $this->convertCustom($params);
        $this->params = $this->convertArraysToStrings($params);

        return $this;
    }

    public function getParams(): array
    {
        return $this->params;
    }

    public function getOptions(): array
    {
        return $this->options;
    }

    public function getIndex(): ?string
    {
        return $this->index;
    }

    /**
     * @return $this
     */
    public function setIndex($index)
    {
        if ($index === null) {
            return $this;
        }

        if (is_array($index) === true) {
            $index = array_map('trim', $index);
            $index = implode(",", $index);
        }

        $this->index = Utility::urlencode($index);

        return $this;
    }

    /**
     * @deprecated
     */
    public function getType(): ?string
    {
        return $this->type;
    }

    /**
     * @deprecated
     * @return     $this
     */
    public function setType(?string $type)
    {
        if ($type === null) {
            return $this;
        }

        if (is_array($type) === true) {
            $type = array_map('trim', $type);
            $type = implode(",", $type);
        }

        $this->type = Utility::urlencode($type);

        return $this;
    }

    /**
     * @param int|string $docID
     *
     * @return $this
     */
    public function setId($docID)
    {
        if ($docID === null) {
            return $this;
        }

        if (is_int($docID)) {
            $docID = (string) $docID;
        }
        
        $this->id = Utility::urlencode($docID);

        return $this;
    }

    /**
     * @return array|string
     */
    public function getBody()
    {
        return $this->body;
    }

    protected function getOptionalURI(string $endpoint): string
    {
        $uri = [];
        $uri[] = $this->getOptionalIndex();
        $uri[] = $this->getOptionalType();
        $uri[] = $endpoint;
        $uri =  array_filter($uri);

        return '/' . implode('/', $uri);
    }

    private function getOptionalIndex(): string
    {
        if (isset($this->index) === true) {
            return $this->index;
        } else {
            return '_all';
        }
    }

    private function getOptionalType(): string
    {
        if (isset($this->type) === true) {
            return $this->type;
        } else {
            return '';
        }
    }

    /**
     * @param array $params
     *
     * @throws UnexpectedValueException
     */
    private function checkUserParams(array $params)
    {
        if (empty($params)) {
            return; //no params, just return.
        }

        $whitelist = array_merge(
            $this->getParamWhitelist(),
            [ 'pretty', 'human', 'error_trace', 'source', 'filter_path', 'opaqueId' ]
        );

        $invalid = array_diff(array_keys($params), $whitelist);
        if (count($invalid) > 0) {
            sort($invalid);
            sort($whitelist);
            throw new UnexpectedValueException(
                sprintf(
                    (count($invalid) > 1 ? '"%s" are not valid parameters.' : '"%s" is not a valid parameter.').' Allowed parameters are "%s"',
                    implode('", "', $invalid),
                    implode('", "', $whitelist)
                )
            );
        }
    }

    /**
     * @param array $params Note: this is passed by-reference!
     */
    private function extractOptions(&$params)
    {
        // Extract out client options, then start transforming
        if (isset($params['client']) === true) {
            // Check if the opaqueId is populated and add the header
            if (isset($params['client']['opaqueId']) === true) {
                if (isset($params['client']['headers']) === false) {
                    $params['client']['headers'] = [];
                }
                $params['client']['headers']['x-opaque-id'] = [trim($params['client']['opaqueId'])];
                unset($params['client']['opaqueId']);
            }

            $this->options['client'] = $params['client'];
            unset($params['client']);
        }
        $ignore = isset($this->options['client']['ignore']) ? $this->options['client']['ignore'] : null;
        if (isset($ignore) === true) {
            if (is_string($ignore)) {
                $this->options['client']['ignore'] = explode(",", $ignore);
            } elseif (is_array($ignore)) {
                $this->options['client']['ignore'] = $ignore;
            } else {
                $this->options['client']['ignore'] = [$ignore];
            }
        }
    }

    private function convertCustom(array $params): array
    {
        if (isset($params['custom']) === true) {
            foreach ($params['custom'] as $k => $v) {
                $params[$k] = $v;
            }
            unset($params['custom']);
        }

        return $params;
    }

    private function convertArraysToStrings(array $params): array
    {
        foreach ($params as $key => &$value) {
            if (!($key === 'client' || $key == 'custom') && is_array($value) === true) {
                if ($this->isNestedArray($value) !== true) {
                    $value = implode(",", $value);
                }
            }
        }

        return $params;
    }

    private function isNestedArray(array $a): bool
    {
        foreach ($a as $v) {
            if (is_array($v)) {
                return true;
            }
        }

        return false;
    }
}
