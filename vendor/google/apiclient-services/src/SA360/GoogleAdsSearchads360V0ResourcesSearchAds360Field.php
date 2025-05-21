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

namespace Google\Service\SA360;

class GoogleAdsSearchads360V0ResourcesSearchAds360Field extends \Google\Collection
{
  protected $collection_key = 'selectableWith';
  /**
   * @var string[]
   */
  public $attributeResources;
  /**
   * @var string
   */
  public $category;
  /**
   * @var string
   */
  public $dataType;
  /**
   * @var string[]
   */
  public $enumValues;
  /**
   * @var bool
   */
  public $filterable;
  /**
   * @var bool
   */
  public $isRepeated;
  /**
   * @var string[]
   */
  public $metrics;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $resourceName;
  /**
   * @var string[]
   */
  public $segments;
  /**
   * @var bool
   */
  public $selectable;
  /**
   * @var string[]
   */
  public $selectableWith;
  /**
   * @var bool
   */
  public $sortable;
  /**
   * @var string
   */
  public $typeUrl;

  /**
   * @param string[]
   */
  public function setAttributeResources($attributeResources)
  {
    $this->attributeResources = $attributeResources;
  }
  /**
   * @return string[]
   */
  public function getAttributeResources()
  {
    return $this->attributeResources;
  }
  /**
   * @param string
   */
  public function setCategory($category)
  {
    $this->category = $category;
  }
  /**
   * @return string
   */
  public function getCategory()
  {
    return $this->category;
  }
  /**
   * @param string
   */
  public function setDataType($dataType)
  {
    $this->dataType = $dataType;
  }
  /**
   * @return string
   */
  public function getDataType()
  {
    return $this->dataType;
  }
  /**
   * @param string[]
   */
  public function setEnumValues($enumValues)
  {
    $this->enumValues = $enumValues;
  }
  /**
   * @return string[]
   */
  public function getEnumValues()
  {
    return $this->enumValues;
  }
  /**
   * @param bool
   */
  public function setFilterable($filterable)
  {
    $this->filterable = $filterable;
  }
  /**
   * @return bool
   */
  public function getFilterable()
  {
    return $this->filterable;
  }
  /**
   * @param bool
   */
  public function setIsRepeated($isRepeated)
  {
    $this->isRepeated = $isRepeated;
  }
  /**
   * @return bool
   */
  public function getIsRepeated()
  {
    return $this->isRepeated;
  }
  /**
   * @param string[]
   */
  public function setMetrics($metrics)
  {
    $this->metrics = $metrics;
  }
  /**
   * @return string[]
   */
  public function getMetrics()
  {
    return $this->metrics;
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
   * @param string
   */
  public function setResourceName($resourceName)
  {
    $this->resourceName = $resourceName;
  }
  /**
   * @return string
   */
  public function getResourceName()
  {
    return $this->resourceName;
  }
  /**
   * @param string[]
   */
  public function setSegments($segments)
  {
    $this->segments = $segments;
  }
  /**
   * @return string[]
   */
  public function getSegments()
  {
    return $this->segments;
  }
  /**
   * @param bool
   */
  public function setSelectable($selectable)
  {
    $this->selectable = $selectable;
  }
  /**
   * @return bool
   */
  public function getSelectable()
  {
    return $this->selectable;
  }
  /**
   * @param string[]
   */
  public function setSelectableWith($selectableWith)
  {
    $this->selectableWith = $selectableWith;
  }
  /**
   * @return string[]
   */
  public function getSelectableWith()
  {
    return $this->selectableWith;
  }
  /**
   * @param bool
   */
  public function setSortable($sortable)
  {
    $this->sortable = $sortable;
  }
  /**
   * @return bool
   */
  public function getSortable()
  {
    return $this->sortable;
  }
  /**
   * @param string
   */
  public function setTypeUrl($typeUrl)
  {
    $this->typeUrl = $typeUrl;
  }
  /**
   * @return string
   */
  public function getTypeUrl()
  {
    return $this->typeUrl;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ResourcesSearchAds360Field::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ResourcesSearchAds360Field');
