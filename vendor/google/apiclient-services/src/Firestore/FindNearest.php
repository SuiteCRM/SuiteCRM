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

namespace Google\Service\Firestore;

class FindNearest extends \Google\Model
{
  /**
   * @var string
   */
  public $distanceMeasure;
  /**
   * @var string
   */
  public $distanceResultField;
  public $distanceThreshold;
  /**
   * @var int
   */
  public $limit;
  protected $queryVectorType = Value::class;
  protected $queryVectorDataType = '';
  protected $vectorFieldType = FieldReference::class;
  protected $vectorFieldDataType = '';

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
  public function setDistanceResultField($distanceResultField)
  {
    $this->distanceResultField = $distanceResultField;
  }
  /**
   * @return string
   */
  public function getDistanceResultField()
  {
    return $this->distanceResultField;
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
   * @param FieldReference
   */
  public function setVectorField(FieldReference $vectorField)
  {
    $this->vectorField = $vectorField;
  }
  /**
   * @return FieldReference
   */
  public function getVectorField()
  {
    return $this->vectorField;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(FindNearest::class, 'Google_Service_Firestore_FindNearest');
