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

namespace Google\Service\ServerlessVPCAccess;

class Connector extends \Google\Collection
{
  protected $collection_key = 'connectedProjects';
  /**
   * @var string[]
   */
  public $connectedProjects;
  /**
   * @var string
   */
  public $ipCidrRange;
  /**
   * @var string
   */
  public $machineType;
  /**
   * @var int
   */
  public $maxInstances;
  /**
   * @var int
   */
  public $maxThroughput;
  /**
   * @var int
   */
  public $minInstances;
  /**
   * @var int
   */
  public $minThroughput;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $network;
  /**
   * @var string
   */
  public $state;
  protected $subnetType = Subnet::class;
  protected $subnetDataType = '';

  /**
   * @param string[]
   */
  public function setConnectedProjects($connectedProjects)
  {
    $this->connectedProjects = $connectedProjects;
  }
  /**
   * @return string[]
   */
  public function getConnectedProjects()
  {
    return $this->connectedProjects;
  }
  /**
   * @param string
   */
  public function setIpCidrRange($ipCidrRange)
  {
    $this->ipCidrRange = $ipCidrRange;
  }
  /**
   * @return string
   */
  public function getIpCidrRange()
  {
    return $this->ipCidrRange;
  }
  /**
   * @param string
   */
  public function setMachineType($machineType)
  {
    $this->machineType = $machineType;
  }
  /**
   * @return string
   */
  public function getMachineType()
  {
    return $this->machineType;
  }
  /**
   * @param int
   */
  public function setMaxInstances($maxInstances)
  {
    $this->maxInstances = $maxInstances;
  }
  /**
   * @return int
   */
  public function getMaxInstances()
  {
    return $this->maxInstances;
  }
  /**
   * @param int
   */
  public function setMaxThroughput($maxThroughput)
  {
    $this->maxThroughput = $maxThroughput;
  }
  /**
   * @return int
   */
  public function getMaxThroughput()
  {
    return $this->maxThroughput;
  }
  /**
   * @param int
   */
  public function setMinInstances($minInstances)
  {
    $this->minInstances = $minInstances;
  }
  /**
   * @return int
   */
  public function getMinInstances()
  {
    return $this->minInstances;
  }
  /**
   * @param int
   */
  public function setMinThroughput($minThroughput)
  {
    $this->minThroughput = $minThroughput;
  }
  /**
   * @return int
   */
  public function getMinThroughput()
  {
    return $this->minThroughput;
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
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param Subnet
   */
  public function setSubnet(Subnet $subnet)
  {
    $this->subnet = $subnet;
  }
  /**
   * @return Subnet
   */
  public function getSubnet()
  {
    return $this->subnet;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Connector::class, 'Google_Service_ServerlessVPCAccess_Connector');
