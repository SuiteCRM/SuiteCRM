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

namespace Google\Service\CloudDomains;

class GeoPolicyItem extends \Google\Collection
{
  protected $collection_key = 'signatureRrdata';
  protected $healthCheckedTargetsType = HealthCheckTargets::class;
  protected $healthCheckedTargetsDataType = '';
  /**
   * @var string
   */
  public $location;
  /**
   * @var string[]
   */
  public $rrdata;
  /**
   * @var string[]
   */
  public $signatureRrdata;

  /**
   * @param HealthCheckTargets
   */
  public function setHealthCheckedTargets(HealthCheckTargets $healthCheckedTargets)
  {
    $this->healthCheckedTargets = $healthCheckedTargets;
  }
  /**
   * @return HealthCheckTargets
   */
  public function getHealthCheckedTargets()
  {
    return $this->healthCheckedTargets;
  }
  /**
   * @param string
   */
  public function setLocation($location)
  {
    $this->location = $location;
  }
  /**
   * @return string
   */
  public function getLocation()
  {
    return $this->location;
  }
  /**
   * @param string[]
   */
  public function setRrdata($rrdata)
  {
    $this->rrdata = $rrdata;
  }
  /**
   * @return string[]
   */
  public function getRrdata()
  {
    return $this->rrdata;
  }
  /**
   * @param string[]
   */
  public function setSignatureRrdata($signatureRrdata)
  {
    $this->signatureRrdata = $signatureRrdata;
  }
  /**
   * @return string[]
   */
  public function getSignatureRrdata()
  {
    return $this->signatureRrdata;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GeoPolicyItem::class, 'Google_Service_CloudDomains_GeoPolicyItem');
