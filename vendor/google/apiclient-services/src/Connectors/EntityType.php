<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\Connectors;

class EntityType extends \Google\Collection
{
  protected $collection_key = 'operations';
  protected $fieldsType = Field::class;
  protected $fieldsDataType = 'array';
  protected $jsonSchemaType = JsonSchema::class;
  protected $jsonSchemaDataType = '';
  /**
   * @var string
   */
  public $name;
  /**
   * @var string[]
   */
  public $operations;

  /**
   * @param Field[]
   */
  public function setFields($fields)
  {
    $this->fields = $fields;
  }
  /**
   * @return Field[]
   */
  public function getFields()
  {
    return $this->fields;
  }
  /**
   * @param JsonSchema
   */
  public function setJsonSchema(JsonSchema $jsonSchema)
  {
    $this->jsonSchema = $jsonSchema;
  }
  /**
   * @return JsonSchema
   */
  public function getJsonSchema()
  {
    return $this->jsonSchema;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string[]
   */
  public function setOperations($operations)
  {
    $this->operations = $operations;
  }
  /**
   * @return string[]
   */
  public function getOperations()
  {
    return $this->operations;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EntityType::class, 'Google_Service_Connectors_EntityType');
