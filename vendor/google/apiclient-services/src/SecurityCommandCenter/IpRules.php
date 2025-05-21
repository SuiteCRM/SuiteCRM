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

namespace Google\Service\SecurityCommandCenter;

class IpRules extends \Google\Collection
{
  protected $collection_key = 'sourceIpRanges';
  protected $allowedType = Allowed::class;
  protected $allowedDataType = '';
  protected $deniedType = Denied::class;
  protected $deniedDataType = '';
  /**
   * @var string[]
   */
  public $destinationIpRanges;
  /**
   * @var string
   */
  public $direction;
  /**
   * @var string[]
   */
  public $exposedServices;
  /**
   * @var string[]
   */
  public $sourceIpRanges;

  /**
   * @param Allowed
   */
  public function setAllowed(Allowed $allowed)
  {
    $this->allowed = $allowed;
  }
  /**
   * @return Allowed
   */
  public function getAllowed()
  {
    return $this->allowed;
  }
  /**
   * @param Denied
   */
  public function setDenied(Denied $denied)
  {
    $this->denied = $denied;
  }
  /**
   * @return Denied
   */
  public function getDenied()
  {
    return $this->denied;
  }
  /**
   * @param string[]
   */
  public function setDestinationIpRanges($destinationIpRanges)
  {
    $this->destinationIpRanges = $destinationIpRanges;
  }
  /**
   * @return string[]
   */
  public function getDestinationIpRanges()
  {
    return $this->destinationIpRanges;
  }
  /**
   * @param string
   */
  public function setDirection($direction)
  {
    $this->direction = $direction;
  }
  /**
   * @return string
   */
  public function getDirection()
  {
    return $this->direction;
  }
  /**
   * @param string[]
   */
  public function setExposedServices($exposedServices)
  {
    $this->exposedServices = $exposedServices;
  }
  /**
   * @return string[]
   */
  public function getExposedServices()
  {
    return $this->exposedServices;
  }
  /**
   * @param string[]
   */
  public function setSourceIpRanges($sourceIpRanges)
  {
    $this->sourceIpRanges = $sourceIpRanges;
  }
  /**
   * @return string[]
   */
  public function getSourceIpRanges()
  {
    return $this->sourceIpRanges;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(IpRules::class, 'Google_Service_SecurityCommandCenter_IpRules');
