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

namespace Google\Service\Datastore;

class FindNearest extends \Google\Model
{
  /**
   * @var string
   */
  public $distanceMeasure;
  /**
   * @var string
   */
  public $distanceResultProperty;
  public $distanceThreshold;
  /**
   * @var int
   */
  public $limit;
  protected $queryVectorType = Value::class;
  protected $queryVectorDataType = '';
  protected $vectorPropertyType = PropertyReference::class;
  protected $vectorPropertyDataType = '';

  /**
   * @param string
   */
  public function setDistanceMeasure($distanceMeasure)
  {
    $this->distanceMeasure = $distanceMeasure;
  }
  /**
   * @return string
   */
  public function getDistanceMeasure()
  {
    return $this->distanceMeasure;
  }
  /**
   * @param string
   */
  public function setDistanceResultProperty($distanceResultProperty)
  {
    $this->distanceResultProperty = $distanceResultProperty;
  }
  /**
   * @return string
   */
  public function getDistanceResultProperty()
  {
    return $this->distanceResultProperty;
  }
  public function setDistanceThreshold($distanceThreshold)
  {
    $this->distanceThreshold = $distanceThreshold;
  }
  public function getDistanceThreshold()
  {
    return $this->distanceThreshold;
  }
  /**
   * @param int
   */
  public function setLimit($limit)
  {
    $this->limit = $limit;
  }
  /**
   * @return int
   */
  public function getLimit()
  {
    return $this->limit;
  }
  /**
   * @param Value
   */
  public function setQueryVector(Value $queryVector)
  {
    $this->queryVector = $queryVector;
  }
  /**
   * @return Value
   */
  public function getQueryVector()
  {
    return $this->queryVector;
  }
  /**
   * @param PropertyReference
   */
  public function setVectorProperty(PropertyReference $vectorProperty)
  {
    $this->vectorProperty = $vectorProperty;
  }
  /**
   * @return PropertyReference
   */
  public function getVectorProperty()
  {
    return $this->vectorProperty;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(FindNearest::class, 'Google_Service_Datastore_FindNearest');
