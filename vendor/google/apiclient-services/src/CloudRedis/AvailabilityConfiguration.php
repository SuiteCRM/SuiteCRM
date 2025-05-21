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

class AvailabilityConfiguration extends \Google\Model
{
  /**
   * @var bool
   */
  public $automaticFailoverRoutingConfigured;
  /**
   * @var string
   */
  public $availabilityType;
  /**
   * @var bool
   */
  public $crossRegionReplicaConfigured;
  /**
   * @var bool
   */
  public $externalReplicaConfigured;
  /**
   * @var bool
   */
  public $promotableReplicaConfigured;

  /**
   * @param bool
   */
  public function setAutomaticFailoverRoutingConfigured($automaticFailoverRoutingConfigured)
  {
    $this->automaticFailoverRoutingConfigured = $automaticFailoverRoutingConfigured;
  }
  /**
   * @return bool
   */
  public function getAutomaticFailoverRoutingConfigured()
  {
    return $this->automaticFailoverRoutingConfigured;
  }
  /**
   * @param string
   */
  public function setAvailabilityType($availabilityType)
  {
    $this->availabilityType = $availabilityType;
  }
  /**
   * @return string
   */
  public function getAvailabilityType()
  {
    return $this->availabilityType;
  }
  /**
   * @param bool
   */
  public function setCrossRegionReplicaConfigured($crossRegionReplicaConfigured)
  {
    $this->crossRegionReplicaConfigured = $crossRegionReplicaConfigured;
  }
  /**
   * @return bool
   */
  public function getCrossRegionReplicaConfigured()
  {
    return $this->crossRegionReplicaConfigured;
  }
  /**
   * @param bool
   */
  public function setExternalReplicaConfigured($externalReplicaConfigured)
  {
    $this->externalReplicaConfigured = $externalReplicaConfigured;
  }
  /**
   * @return bool
   */
  public function getExternalReplicaConfigured()
  {
    return $this->externalReplicaConfigured;
  }
  /**
   * @param bool
   */
  public function setPromotableReplicaConfigured($promotableReplicaConfigured)
  {
    $this->promotableReplicaConfigured = $promotableReplicaConfigured;
  }
  /**
   * @return bool
   */
  public function getPromotableReplicaConfigured()
  {
    return $this->promotableReplicaConfigured;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AvailabilityConfiguration::class, 'Google_Service_CloudRedis_AvailabilityConfiguration');
