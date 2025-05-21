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

namespace Google\Service\CloudRedis;

class ObservabilityMetricData extends \Google\Model
{
  /**
   * @var string
   */
  public $aggregationType;
  /**
   * @var string
   */
  public $metricType;
  /**
   * @var string
   */
  public $observationTime;
  /**
   * @var string
   */
  public $resourceName;
  protected $valueType = TypedValue::class;
  protected $valueDataType = '';

  /**
   * @param string
   */
  public function setAggregationType($aggregationType)
  {
    $this->aggregationType = $aggregationType;
  }
  /**
   * @return string
   */
  public function getAggregationType()
  {
    return $this->aggregationType;
  }
  /**
   * @param string
   */
  public function setMetricType($metricType)
  {
    $this->metricType = $metricType;
  }
  /**
   * @return string
   */
  public function getMetricType()
  {
    return $this->metricType;
  }
  /**
   * @param string
   */
  public function setObservationTime($observationTime)
  {
    $this->observationTime = $observationTime;
  }
  /**
   * @return string
   */
  public function getObservationTime()
  {
    return $this->observationTime;
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
   * @param TypedValue
   */
  public function setValue(TypedValue $value)
  {
    $this->value = $value;
  }
  /**
   * @return TypedValue
   */
  public function getValue()
  {
    return $this->value;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ObservabilityMetricData::class, 'Google_Service_CloudRedis_ObservabilityMetricData');
