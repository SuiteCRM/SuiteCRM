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

namespace Google\Service\Apigee;

class GoogleCloudApigeeV1SecurityIncident extends \Google\Collection
{
  protected $collection_key = 'detectionTypes';
  /**
   * @var string[]
   */
  public $detectionTypes;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $firstDetectedTime;
  /**
   * @var string
   */
  public $lastDetectedTime;
  /**
   * @var string
   */
  public $lastObservabilityChangeTime;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $observability;
  /**
   * @var string
   */
  public $riskLevel;
  /**
   * @var string
   */
  public $trafficCount;

  /**
   * @param string[]
   */
  public function setDetectionTypes($detectionTypes)
  {
    $this->detectionTypes = $detectionTypes;
  }
  /**
   * @return string[]
   */
  public function getDetectionTypes()
  {
    return $this->detectionTypes;
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
   * @param string
   */
  public function setFirstDetectedTime($firstDetectedTime)
  {
    $this->firstDetectedTime = $firstDetectedTime;
  }
  /**
   * @return string
   */
  public function getFirstDetectedTime()
  {
    return $this->firstDetectedTime;
  }
  /**
   * @param string
   */
  public function setLastDetectedTime($lastDetectedTime)
  {
    $this->lastDetectedTime = $lastDetectedTime;
  }
  /**
   * @return string
   */
  public function getLastDetectedTime()
  {
    return $this->lastDetectedTime;
  }
  /**
   * @param string
   */
  public function setLastObservabilityChangeTime($lastObservabilityChangeTime)
  {
    $this->lastObservabilityChangeTime = $lastObservabilityChangeTime;
  }
  /**
   * @return string
   */
  public function getLastObservabilityChangeTime()
  {
    return $this->lastObservabilityChangeTime;
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
  public function setObservability($observability)
  {
    $this->observability = $observability;
  }
  /**
   * @return string
   */
  public function getObservability()
  {
    return $this->observability;
  }
  /**
   * @param string
   */
  public function setRiskLevel($riskLevel)
  {
    $this->riskLevel = $riskLevel;
  }
  /**
   * @return string
   */
  public function getRiskLevel()
  {
    return $this->riskLevel;
  }
  /**
   * @param string
   */
  public function setTrafficCount($trafficCount)
  {
    $this->trafficCount = $trafficCount;
  }
  /**
   * @return string
   */
  public function getTrafficCount()
  {
    return $this->trafficCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudApigeeV1SecurityIncident::class, 'Google_Service_Apigee_GoogleCloudApigeeV1SecurityIncident');
