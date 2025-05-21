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

namespace Google\Service\TPU;

class NetworkConfig extends \Google\Model
{
  /**
   * @var bool
   */
  public $canIpForward;
  /**
   * @var bool
   */
  public $enableExternalIps;
  /**
   * @var string
   */
  public $network;
  /**
   * @var int
   */
  public $queueCount;
  /**
   * @var string
   */
  public $subnetwork;

  /**
   * @param bool
   */
  public function setCanIpForward($canIpForward)
  {
    $this->canIpForward = $canIpForward;
  }
  /**
   * @return bool
   */
  public function getCanIpForward()
  {
    return $this->canIpForward;
  }
  /**
   * @param bool
   */
  public function setEnableExternalIps($enableExternalIps)
  {
    $this->enableExternalIps = $enableExternalIps;
  }
  /**
   * @return bool
   */
  public function getEnableExternalIps()
  {
    return $this->enableExternalIps;
  }
  /**
   * @param string
   */
  public function setNetwork($network)
  {
    $this->network = $network;
  }
  /**
   * @return string
   */
  public function getNetwork()
  {
    return $this->network;
  }
  /**
   * @param int
   */
  public function setQueueCount($queueCount)
  {
    $this->queueCount = $queueCount;
  }
  /**
   * @return int
   */
  public function getQueueCount()
  {
    return $this->queueCount;
  }
  /**
   * @param string
   */
  public function setSubnetwork($subnetwork)
  {
    $this->subnetwork = $subnetwork;
  }
  /**
   * @return string
   */
  public function getSubnetwork()
  {
    return $this->subnetwork;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(NetworkConfig::class, 'Google_Service_TPU_NetworkConfig');
