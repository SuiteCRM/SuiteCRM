<?php
/**
 *
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
namespace SuiteCRM\API\JsonApi\v1\Resource;

use SuiteCRM\API\JsonApi\v1\Links;
use SuiteCRM\API\JsonApi\v1\Enumerator\ResourceEnum;
use SuiteCRM\API\v8\Exception\BadRequestException;
use SuiteCRM\API\v8\Exception\ConflictException;

/**
 * Class Resource
 * @package SuiteCRM\API\JsonApi\v1\Resource
 * @see http://jsonapi.org/format/1.0/#document-resource-objects
 */
class Resource extends ResourceIdentifier
{
    protected static $JSON_API_SKIP_RESERVED_KEYWORDS = array(
        'id',
        'type',
    );

    protected static $JSON_API_RESERVED_KEYWORDS = array(
        'id',
        'type',
        'data',
        self::META,
        'jsonapi',
        self::LINKS,
        'included',
        'self',
        'related',
        self::ATTRIBUTES,
        self::RELATIONSHIPS,
        'href',
        'first',
        'last',
        'prev',
        'next',
        'related',
        'errors',
    );
    public const DATA_RELATIONSHIPS = '/data/relationships/';
    public const RELATIONSHIPS = 'relationships';
    public const LINKS = 'links';
    public const META = 'meta';
    public const ATTRIBUTES = 'attributes';

    /**
     * @var array $attributes
     */
    protected $attributes;

    /**
     * @var array $relationships
     */
    protected $relationships;

    /**
     * @var Links $links
     */
    protected $links;

    /**
     * @var string $source rfc6901
     * @see https://tools.ietf.org/html/rfc6901
     */
    protected $source;

    /**
     * @param array $data
     * @param string $source rfc6901
     * @return Resource|$this
     * @throws ConflictException
     * @throws BadRequestException
     * @see https://tools.ietf.org/html/rfc6901
     */
    public function fromJsonApiRequest(array $data, $source = ResourceEnum::DEFAULT_SOURCE)
    {
        if (isset($data['id'])) {
            $this->id = $data['id'];
        }
        $this->type = $data['type'];
        $this->source = $source;

        if ($this->type === null) {
            $exception = new ConflictException('[Missing "type" key in data]');
            $exception->setSource('/data/attributes/type');
            throw $exception;
        }

        if (!isset($data[self::ATTRIBUTES])) {
            $exception = new BadRequestException('[Missing attributes]');
            $exception->setSource('/data/attributes');
            throw $exception;
        }

        $this->attributesFromDataArray($data);
        $this->relationshipFromDataArray($data);

        return clone $this;
    }

    /**
     * @param Resource $resource
     */
    public function mergeAttributes(Resource $resource)
    {
        $resourceArray = $resource->toJsonApiResponse();
        $this->attributes = array_merge($this->attributes, $resourceArray[self::ATTRIBUTES]);
    }

    /**
     * @return array
     */
    public function toJsonApiResponse()
    {
        return $this->toJsonApiResponseWithFields(array_keys($this->attributes));
    }


    /**
     * @param array $fields
     * @return array - Return only the fields which exist in the $fields
     */
    public function toJsonApiResponseWithFields(array $fields)
    {
        $response = array();

        $response['id'] = $this->id;
        $response['type'] = $this->type;

        foreach ($this->attributes as $attribute => $value) {
            if ($attribute === 'id') {
                continue;
            }
            if (in_array($attribute, $fields) === true) {
                if (substr($attribute, -5) === "_file") {
                    $value = "<OMITTED>";
                } else {
                    $value = $this->attributes[$attribute];
                }
                $response[self::ATTRIBUTES][$attribute] = $value;
            }
        }

        if ($this->meta !== null) {
            $response[self::META] = $this->meta;
        }

        if ($this->links !== null) {
            $response[self::LINKS] = $this->links->toJsonApiResponse();
        }

        if ($this->relationships !== null) {
            $response[self::RELATIONSHIPS] = $this->relationships;
        }

        return $response;
    }

    /**
     * @param Links $links
     * @return $this
     */
    public function withLinks(Links $links)
    {
        $this->links = $links;

        return clone $this;
    }

    /**
     * @param Relationship $relationship
     * @return Resource|$this
     */
    public function withRelationship(\SuiteCRM\API\JsonApi\v1\Resource\Relationship $relationship)
    {
        $relationshipName = $relationship->getRelatationshipName();
        $this->relationships[$relationshipName] = $relationship->toJsonApiResponse();
        return clone $this;
    }

    public function getRelationshipByName($link)
    {
        return $this->relationships[$link]['data'];
    }

    /**
     * Reserved words which must not be used in the Json API Request / Response
     * @return array
     */
    public function getReservedKeywords()
    {
        return self::$JSON_API_RESERVED_KEYWORDS;
    }
    /**
     * @throws ConflictException
     */
    protected function validateResource()
    {
        // Validate ID
        if ($this->id === null) {
            $exception = new ConflictException('[Missing "id" key in data]"');
            $exception->setSource($this->source . '/attributes/id');
            throw $exception;
        }

        // Validate Type
        if ($this->type === null) {
            $exception = new ConflictException('[Missing "type" key in data]');
            $exception->setSource($this->source . '/attributes/type');
            throw $exception;
        }
    }

    /**
     * @param array $data
     * @throws BadRequestException
     */
    private function relationshipFromDataArray(array $data)
    {
        if (isset($data[self::RELATIONSHIPS])) {
            $dataRelationships = $data[self::RELATIONSHIPS];
            // Validate relationships
            foreach ($dataRelationships as $relationshipName => $relationship) {
                if (isset($relationship['data']) === false) {
                    $exception = new BadRequestException('[Resource] [missing relationship data]');
                    $exception->setSource('/data/relationships/{link}/data');
                    throw $exception;
                }

                if (empty($relationship['data'])) {
                    // ignore as it us an indication that we need remove the related items
                    continue;
                }

                // Detect Relationship type
                if (isset($relationship['data'][0])) {
                    // detected to many
                    $toManyRelationships = $relationship['data'];
                    /** @var array $toManyRelationships */
                    foreach ($toManyRelationships as $toManyRelationshipName => $toManyRelationship) {
                        // validate relationship
                        $this->validateToManyRelationshipFromDataArray(
                            $toManyRelationship,
                            $relationshipName,
                            $toManyRelationshipName
                        );
                    }
                } else {
                    // detected to one
                    $toOneRelationship = $relationship['data'];
                    $this->validateToOneRelationshipFromDataArray($toOneRelationship, $relationshipName);
                }
            }
            $this->relationships = $dataRelationships;
        }
    }

    /**
     * @param $data
     */
    private function attributesFromDataArray($data)
    {
        global $sugar_config;
        foreach ($data[self::ATTRIBUTES] as $attributeName => $attributeValue) {
            if ($this->attributes === null) {
                $this->attributes = array();
            }

            // Filter security sensitive information from attributes
            if (
                isset($sugar_config['filter_module_fields'][$this->type]) &&
                in_array($attributeName, $sugar_config['filter_module_fields'][$this->type], true)
            ) {
                continue;
            }

            $this->attributes[$attributeName] = $attributeValue;
        }
    }

    /**
     * @param $toOneRelationship
     * @param $relationshipName
     * @throws BadRequestException
     */
    private function validateToOneRelationshipFromDataArray($toOneRelationship, $relationshipName)
    {
        // validate relationship
        if (isset($toOneRelationship['id']) === false || empty($toOneRelationship['id'])) {
            $exception = new BadRequestException('[Resource] [missing "to one" relationship field] "id"');
            $exception->setSource(self::DATA_RELATIONSHIPS . $relationshipName . '/id');
            throw $exception;
        }

        if (isset($toOneRelationship['type']) === false || empty($toOneRelationship['type'])) {
            $exception = new BadRequestException('[Resource] [missing "to one" relationship field] "type"');
            $exception->setSource(self::DATA_RELATIONSHIPS . $relationshipName . '/type');
            throw $exception;
        }

        if (isset($toOneRelationship[self::ATTRIBUTES]) === true) {
            $exception = new BadRequestException('[Resource] [invalid "to one" relationship field] "attributes"');
            $exception->setSource(self::DATA_RELATIONSHIPS . $relationshipName . '/attributes');
            $exception->setDetail('A related item\'s cannot be updated in the relationships object');
            throw $exception;
        }
    }

    /**
     * @param $toManyRelationship
     * @param $relationshipName
     * @param $toManyRelationshipName
     * @throws BadRequestException
     */
    private function validateToManyRelationshipFromDataArray(
        $toManyRelationship,
        $relationshipName,
        $toManyRelationshipName
    ) {
        if (isset($toManyRelationship['id']) === false || empty($toManyRelationship['id'])) {
            $exception = new BadRequestException('[Resource] [missing "to many" relationship field] "id"');
            $exception->setSource(
                self::DATA_RELATIONSHIPS . $relationshipName . '/' . $toManyRelationshipName . '/id'
            );
            throw $exception;
        }

        if (isset($toManyRelationship['type']) === false || empty($toManyRelationship['type'])) {
            $exception = new BadRequestException('[Resource] [missing "to many" relationship field] "type"');
            $exception->setSource(
                self::DATA_RELATIONSHIPS . $relationshipName . '/' . $toManyRelationshipName . '/type'
            );
            throw $exception;
        }

        if (isset($toManyRelationship[self::ATTRIBUTES]) === true) {
            $exception = new BadRequestException('[Resource] [invalid "to many" relationship field] "attributes"');
            $exception->setSource(
                self::DATA_RELATIONSHIPS . $relationshipName . '/' . $toManyRelationshipName . '/attributes'
            );
            $exception->setDetail('A related item\'s cannot be updated in the relationships object');
            throw $exception;
        }
    }
}
