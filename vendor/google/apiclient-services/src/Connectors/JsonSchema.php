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

class JsonSchema extends \Google\Collection
{
  protected $collection_key = 'type';
  /**
   * @var array[]
   */
  public $additionalDetails;
  /**
   * @var array
   */
  public $default;
  /**
   * @var string
   */
  public $description;
  /**
   * @var array[]
   */
  public $enum;
  /**
   * @var string
   */
  public $format;
  protected $itemsType = JsonSchema::class;
  protected $itemsDataType = '';
  /**
   * @var string
   */
  public $jdbcType;
  protected $propertiesType = JsonSchema::class;
  protected $propertiesDataType = 'map';
  /**
   * @var string[]
   */
  public $required;
  /**
   * @var string[]
   */
  public $type;

  /**
   * @param array[]
   */
  public function setAdditionalDetails($additionalDetails)
  {
    $this->additionalDetails = $additionalDetails;
  }
  /**
   * @return array[]
   */
  public function getAdditionalDetails()
  {
    return $this->additionalDetails;
  }
  /**
   * @param array
   */
  public function setDefault($default)
  {
    $this->default = $default;
  }
  /**
   * @return array
   */
  public function getDefault()
  {
    return $this->default;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param array[]
   */
  public function setEnum($enum)
  {
    $this->enum = $enum;
  }
  /**
   * @return array[]
   */
  public function getEnum()
  {
    return $this->enum;
  }
  /**
   * @param string
   */
  public function setFormat($format)
  {
    $this->format = $format;
  }
  /**
   * @return string
   */
  public function getFormat()
  {
    return $this->format;
  }
  /**
   * @param JsonSchema
   */
  public function setItems(JsonSchema $items)
  {
    $this->items = $items;
  }
  /**
   * @return JsonSchema
   */
  public function getItems()
  {
    return $this->items;
  }
  /**
   * @param string
   */
  public function setJdbcType($jdbcType)
  {
    $this->jdbcType = $jdbcType;
  }
  /**
   * @return string
   */
  public function getJdbcType()
  {
    return $this->jdbcType;
  }
  /**
   * @param JsonSchema[]
   */
  public function setProperties($properties)
  {
    $this->properties = $properties;
  }
  /**
   * @return JsonSchema[]
   */
  public function getProperties()
  {
    return $this->properties;
  }
  /**
   * @param string[]
   */
  public function setRequired($required)
  {
    $this->required = $required;
  }
  /**
   * @return string[]
   */
  public function getRequired()
  {
    return $this->required;
  }
  /**
   * @param string[]
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string[]
   */
  public function getType()
  {
    return $this->type;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(JsonSchema::class, 'Google_Service_Connectors_JsonSchema');
