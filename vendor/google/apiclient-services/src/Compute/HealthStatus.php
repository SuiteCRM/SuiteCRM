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

class HealthStatus extends \Google\Model
{
  /**
   * @var string[]
   */
  public $annotations;
  /**
   * @var string
   */
  public $forwardingRule;
  /**
   * @var string
   */
  public $forwardingRuleIp;
  /**
   * @var string
   */
  public $healthState;
  /**
   * @var string
   */
  public $instance;
  /**
   * @var string
   */
  public $ipAddress;
  /**
   * @var string
   */
  public $ipv6Address;
  /**
   * @var string
   */
  public $ipv6HealthState;
  /**
   * @var int
   */
  public $port;
  /**
   * @var string
   */
  public $weight;
  /**
   * @var string
   */
  public $weightError;

  /**
   * @param string[]
   */
  public function setAnnotations($annotations)
  {
    $this->annotations = $annotations;
  }
  /**
   * @return string[]
   */
  public function getAnnotations()
  {
    return $this->annotations;
  }
  /**
   * @param string
   */
  public function setForwardingRule($forwardingRule)
  {
    $this->forwardingRule = $forwardingRule;
  }
  /**
   * @return string
   */
  public function getForwardingRule()
  {
    return $this->forwardingRule;
  }
  /**
   * @param string
   */
  public function setForwardingRuleIp($forwardingRuleIp)
  {
    $this->forwardingRuleIp = $forwardingRuleIp;
  }
  /**
   * @return string
   */
  public function getForwardingRuleIp()
  {
    return $this->forwardingRuleIp;
  }
  /**
   * @param string
   */
  public function setHealthState($healthState)
  {
    $this->healthState = $healthState;
  }
  /**
   * @return string
   */
  public function getHealthState()
  {
    return $this->healthState;
  }
  /**
   * @param string
   */
  public function setInstance($instance)
  {
    $this->instance = $instance;
  }
  /**
   * @return string
   */
  public function getInstance()
  {
    return $this->instance;
  }
  /**
   * @param string
   */
  public function setIpAddress($ipAddress)
  {
    $this->ipAddress = $ipAddress;
  }
  /**
   * @return string
   */
  public function getIpAddress()
  {
    return $this->ipAddress;
  }
  /**
   * @param string
   */
  public function setIpv6Address($ipv6Address)
  {
    $this->ipv6Address = $ipv6Address;
  }
  /**
   * @return string
   */
  public function getIpv6Address()
  {
    return $this->ipv6Address;
  }
  /**
   * @param string
   */
  public function setIpv6HealthState($ipv6HealthState)
  {
    $this->ipv6HealthState = $ipv6HealthState;
  }
  /**
   * @return string
   */
  public function getIpv6HealthState()
  {
    return $this->ipv6HealthState;
  }
  /**
   * @param int
   */
  public function setPort($port)
  {
    $this->port = $port;
  }
  /**
   * @return int
   */
  public function getPort()
  {
    return $this->port;
  }
  /**
   * @param string
   */
  public function setWeight($weight)
  {
    $this->weight = $weight;
  }
  /**
   * @return string
   */
  public function getWeight()
  {
    return $this->weight;
  }
  /**
   * @param string
   */
  public function setWeightError($weightError)
  {
    $this->weightError = $weightError;
  }
  /**
   * @return string
   */
  public function getWeightError()
  {
    return $this->weightError;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(HealthStatus::class, 'Google_Service_Compute_HealthStatus');
