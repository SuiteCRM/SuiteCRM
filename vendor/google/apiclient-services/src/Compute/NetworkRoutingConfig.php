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

namespace Google\Service\Compute;

class NetworkRoutingConfig extends \Google\Model
{
  /**
   * @var bool
   */
  public $bgpAlwaysCompareMed;
  /**
   * @var string
   */
  public $bgpBestPathSelectionMode;
  /**
   * @var string
   */
  public $bgpInterRegionCost;
  /**
   * @var bool
   */
  public $effectiveBgpAlwaysCompareMed;
  /**
   * @var string
   */
  public $effectiveBgpInterRegionCost;
  /**
   * @var string
   */
  public $routingMode;

  /**
   * @param bool
   */
  public function setBgpAlwaysCompareMed($bgpAlwaysCompareMed)
  {
    $this->bgpAlwaysCompareMed = $bgpAlwaysCompareMed;
  }
  /**
   * @return bool
   */
  public function getBgpAlwaysCompareMed()
  {
    return $this->bgpAlwaysCompareMed;
  }
  /**
   * @param string
   */
  public function setBgpBestPathSelectionMode($bgpBestPathSelectionMode)
  {
    $this->bgpBestPathSelectionMode = $bgpBestPathSelectionMode;
  }
  /**
   * @return string
   */
  public function getBgpBestPathSelectionMode()
  {
    return $this->bgpBestPathSelectionMode;
  }
  /**
   * @param string
   */
  public function setBgpInterRegionCost($bgpInterRegionCost)
  {
    $this->bgpInterRegionCost = $bgpInterRegionCost;
  }
  /**
   * @return string
   */
  public function getBgpInterRegionCost()
  {
    return $this->bgpInterRegionCost;
  }
  /**
   * @param bool
   */
  public function setEffectiveBgpAlwaysCompareMed($effectiveBgpAlwaysCompareMed)
  {
    $this->effectiveBgpAlwaysCompareMed = $effectiveBgpAlwaysCompareMed;
  }
  /**
   * @return bool
   */
  public function getEffectiveBgpAlwaysCompareMed()
  {
    return $this->effectiveBgpAlwaysCompareMed;
  }
  /**
   * @param string
   */
  public function setEffectiveBgpInterRegionCost($effectiveBgpInterRegionCost)
  {
    $this->effectiveBgpInterRegionCost = $effectiveBgpInterRegionCost;
  }
  /**
   * @return string
   */
  public function getEffectiveBgpInterRegionCost()
  {
    return $this->effectiveBgpInterRegionCost;
  }
  /**
   * @param string
   */
  public function setRoutingMode($routingMode)
  {
    $this->routingMode = $routingMode;
  }
  /**
   * @return string
   */
  public function getRoutingMode()
  {
    return $this->routingMode;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(NetworkRoutingConfig::class, 'Google_Service_Compute_NetworkRoutingConfig');
