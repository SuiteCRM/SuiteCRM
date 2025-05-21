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

namespace Google\Service\CloudDataplex;

class GoogleCloudDataplexV1AspectTypeMetadataTemplateAnnotations extends \Google\Collection
{
  protected $collection_key = 'stringValues';
  /**
   * @var string
   */
  public $deprecated;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var int
   */
  public $displayOrder;
  /**
   * @var string
   */
  public $stringType;
  /**
   * @var string[]
   */
  public $stringValues;

  /**
   * @param string
   */
  public function setDeprecated($deprecated)
  {
    $this->deprecated = $deprecated;
  }
  /**
   * @return string
   */
  public function getDeprecated()
  {
    return $this->deprecated;
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
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param int
   */
  public function setDisplayOrder($displayOrder)
  {
    $this->displayOrder = $displayOrder;
  }
  /**
   * @return int
   */
  public function getDisplayOrder()
  {
    return $this->displayOrder;
  }
  /**
   * @param string
   */
  public function setStringType($stringType)
  {
    $this->stringType = $stringType;
  }
  /**
   * @return string
   */
  public function getStringType()
  {
    return $this->stringType;
  }
  /**
   * @param string[]
   */
  public function setStringValues($stringValues)
  {
    $this->stringValues = $stringValues;
  }
  /**
   * @return string[]
   */
  public function getStringValues()
  {
    return $this->stringValues;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDataplexV1AspectTypeMetadataTemplateAnnotations::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1AspectTypeMetadataTemplateAnnotations');
