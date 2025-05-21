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

namespace Google\Service\MapsPlaces;

class GoogleMapsPlacesV1EVChargeOptionsConnectorAggregation extends \Google\Model
{
  /**
   * @var string
   */
  public $availabilityLastUpdateTime;
  /**
   * @var int
   */
  public $availableCount;
  /**
   * @var int
   */
  public $count;
  public $maxChargeRateKw;
  /**
   * @var int
   */
  public $outOfServiceCount;
  /**
   * @var string
   */
  public $type;

  /**
   * @param string
   */
  public function setAvailabilityLastUpdateTime($availabilityLastUpdateTime)
  {
    $this->availabilityLastUpdateTime = $availabilityLastUpdateTime;
  }
  /**
   * @return string
   */
  public function getAvailabilityLastUpdateTime()
  {
    return $this->availabilityLastUpdateTime;
  }
  /**
   * @param int
   */
  public function setAvailableCount($availableCount)
  {
    $this->availableCount = $availableCount;
  }
  /**
   * @return int
   */
  public function getAvailableCount()
  {
    return $this->availableCount;
  }
  /**
   * @param int
   */
  public function setCount($count)
  {
    $this->count = $count;
  }
  /**
   * @return int
   */
  public function getCount()
  {
    return $this->count;
  }
  public function setMaxChargeRateKw($maxChargeRateKw)
  {
    $this->maxChargeRateKw = $maxChargeRateKw;
  }
  public function getMaxChargeRateKw()
  {
    return $this->maxChargeRateKw;
  }
  /**
   * @param int
   */
  public function setOutOfServiceCount($outOfServiceCount)
  {
    $this->outOfServiceCount = $outOfServiceCount;
  }
  /**
   * @return int
   */
  public function getOutOfServiceCount()
  {
    return $this->outOfServiceCount;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsPlacesV1EVChargeOptionsConnectorAggregation::class, 'Google_Service_MapsPlaces_GoogleMapsPlacesV1EVChargeOptionsConnectorAggregation');
