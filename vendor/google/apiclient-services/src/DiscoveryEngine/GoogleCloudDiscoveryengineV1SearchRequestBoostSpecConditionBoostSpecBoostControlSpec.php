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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1SearchRequestBoostSpecConditionBoostSpecBoostControlSpec extends \Google\Collection
{
  protected $collection_key = 'controlPoints';
  /**
   * @var string
   */
  public $attributeType;
  protected $controlPointsType = GoogleCloudDiscoveryengineV1SearchRequestBoostSpecConditionBoostSpecBoostControlSpecControlPoint::class;
  protected $controlPointsDataType = 'array';
  /**
   * @var string
   */
  public $fieldName;
  /**
   * @var string
   */
  public $interpolationType;

  /**
   * @param string
   */
  public function setAttributeType($attributeType)
  {
    $this->attributeType = $attributeType;
  }
  /**
   * @return string
   */
  public function getAttributeType()
  {
    return $this->attributeType;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SearchRequestBoostSpecConditionBoostSpecBoostControlSpecControlPoint[]
   */
  public function setControlPoints($controlPoints)
  {
    $this->controlPoints = $controlPoints;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchRequestBoostSpecConditionBoostSpecBoostControlSpecControlPoint[]
   */
  public function getControlPoints()
  {
    return $this->controlPoints;
  }
  /**
   * @param string
   */
  public function setFieldName($fieldName)
  {
    $this->fieldName = $fieldName;
  }
  /**
   * @return string
   */
  public function getFieldName()
  {
    return $this->fieldName;
  }
  /**
   * @param string
   */
  public function setInterpolationType($interpolationType)
  {
    $this->interpolationType = $interpolationType;
  }
  /**
   * @return string
   */
  public function getInterpolationType()
  {
    return $this->interpolationType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1SearchRequestBoostSpecConditionBoostSpecBoostControlSpec::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1SearchRequestBoostSpecConditionBoostSpecBoostControlSpec');
