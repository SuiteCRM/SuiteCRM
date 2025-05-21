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

class GoogleAdsSearchads360V0ResourcesCustomColumn extends \Google\Collection
{
  protected $collection_key = 'referencedSystemColumns';
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $name;
  /**
   * @var bool
   */
  public $queryable;
  /**
   * @var string[]
   */
  public $referencedSystemColumns;
  /**
   * @var bool
   */
  public $referencesAttributes;
  /**
   * @var bool
   */
  public $referencesMetrics;
  /**
   * @var string
   */
  public $renderType;
  /**
   * @var string
   */
  public $resourceName;
  /**
   * @var string
   */
  public $valueType;

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
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
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
   * @param bool
   */
  public function setQueryable($queryable)
  {
    $this->queryable = $queryable;
  }
  /**
   * @return bool
   */
  public function getQueryable()
  {
    return $this->queryable;
  }
  /**
   * @param string[]
   */
  public function setReferencedSystemColumns($referencedSystemColumns)
  {
    $this->referencedSystemColumns = $referencedSystemColumns;
  }
  /**
   * @return string[]
   */
  public function getReferencedSystemColumns()
  {
    return $this->referencedSystemColumns;
  }
  /**
   * @param bool
   */
  public function setReferencesAttributes($referencesAttributes)
  {
    $this->referencesAttributes = $referencesAttributes;
  }
  /**
   * @return bool
   */
  public function getReferencesAttributes()
  {
    return $this->referencesAttributes;
  }
  /**
   * @param bool
   */
  public function setReferencesMetrics($referencesMetrics)
  {
    $this->referencesMetrics = $referencesMetrics;
  }
  /**
   * @return bool
   */
  public function getReferencesMetrics()
  {
    return $this->referencesMetrics;
  }
  /**
   * @param string
   */
  public function setRenderType($renderType)
  {
    $this->renderType = $renderType;
  }
  /**
   * @return string
   */
  public function getRenderType()
  {
    return $this->renderType;
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
   * @param string
   */
  public function setValueType($valueType)
  {
    $this->valueType = $valueType;
  }
  /**
   * @return string
   */
  public function getValueType()
  {
    return $this->valueType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ResourcesCustomColumn::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ResourcesCustomColumn');
