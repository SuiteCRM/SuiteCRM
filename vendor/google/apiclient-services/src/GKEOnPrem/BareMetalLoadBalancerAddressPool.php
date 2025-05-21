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

namespace Google\Service\GKEOnPrem;

class BareMetalLoadBalancerAddressPool extends \Google\Collection
{
  protected $collection_key = 'addresses';
  /**
   * @var string[]
   */
  public $addresses;
  /**
   * @var bool
   */
  public $avoidBuggyIps;
  /**
   * @var bool
   */
  public $manualAssign;
  /**
   * @var string
   */
  public $pool;

  /**
   * @param string[]
   */
  public function setAddresses($addresses)
  {
    $this->addresses = $addresses;
  }
  /**
   * @return string[]
   */
  public function getAddresses()
  {
    return $this->addresses;
  }
  /**
   * @param bool
   */
  public function setAvoidBuggyIps($avoidBuggyIps)
  {
    $this->avoidBuggyIps = $avoidBuggyIps;
  }
  /**
   * @return bool
   */
  public function getAvoidBuggyIps()
  {
    return $this->avoidBuggyIps;
  }
  /**
   * @param bool
   */
  public function setManualAssign($manualAssign)
  {
    $this->manualAssign = $manualAssign;
  }
  /**
   * @return bool
   */
  public function getManualAssign()
  {
    return $this->manualAssign;
  }
  /**
   * @param string
   */
  public function setPool($pool)
  {
    $this->pool = $pool;
  }
  /**
   * @return string
   */
  public function getPool()
  {
    return $this->pool;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BareMetalLoadBalancerAddressPool::class, 'Google_Service_GKEOnPrem_BareMetalLoadBalancerAddressPool');
