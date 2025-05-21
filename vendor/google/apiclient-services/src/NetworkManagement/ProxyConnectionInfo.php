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

namespace Google\Service\NetworkManagement;

class ProxyConnectionInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $networkUri;
  /**
   * @var string
   */
  public $newDestinationIp;
  /**
   * @var int
   */
  public $newDestinationPort;
  /**
   * @var string
   */
  public $newSourceIp;
  /**
   * @var int
   */
  public $newSourcePort;
  /**
   * @var string
   */
  public $oldDestinationIp;
  /**
   * @var int
   */
  public $oldDestinationPort;
  /**
   * @var string
   */
  public $oldSourceIp;
  /**
   * @var int
   */
  public $oldSourcePort;
  /**
   * @var string
   */
  public $protocol;
  /**
   * @var string
   */
  public $subnetUri;

  /**
   * @param string
   */
  public function setNetworkUri($networkUri)
  {
    $this->networkUri = $networkUri;
  }
  /**
   * @return string
   */
  public function getNetworkUri()
  {
    return $this->networkUri;
  }
  /**
   * @param string
   */
  public function setNewDestinationIp($newDestinationIp)
  {
    $this->newDestinationIp = $newDestinationIp;
  }
  /**
   * @return string
   */
  public function getNewDestinationIp()
  {
    return $this->newDestinationIp;
  }
  /**
   * @param int
   */
  public function setNewDestinationPort($newDestinationPort)
  {
    $this->newDestinationPort = $newDestinationPort;
  }
  /**
   * @return int
   */
  public function getNewDestinationPort()
  {
    return $this->newDestinationPort;
  }
  /**
   * @param string
   */
  public function setNewSourceIp($newSourceIp)
  {
    $this->newSourceIp = $newSourceIp;
  }
  /**
   * @return string
   */
  public function getNewSourceIp()
  {
    return $this->newSourceIp;
  }
  /**
   * @param int
   */
  public function setNewSourcePort($newSourcePort)
  {
    $this->newSourcePort = $newSourcePort;
  }
  /**
   * @return int
   */
  public function getNewSourcePort()
  {
    return $this->newSourcePort;
  }
  /**
   * @param string
   */
  public function setOldDestinationIp($oldDestinationIp)
  {
    $this->oldDestinationIp = $oldDestinationIp;
  }
  /**
   * @return string
   */
  public function getOldDestinationIp()
  {
    return $this->oldDestinationIp;
  }
  /**
   * @param int
   */
  public function setOldDestinationPort($oldDestinationPort)
  {
    $this->oldDestinationPort = $oldDestinationPort;
  }
  /**
   * @return int
   */
  public function getOldDestinationPort()
  {
    return $this->oldDestinationPort;
  }
  /**
   * @param string
   */
  public function setOldSourceIp($oldSourceIp)
  {
    $this->oldSourceIp = $oldSourceIp;
  }
  /**
   * @return string
   */
  public function getOldSourceIp()
  {
    return $this->oldSourceIp;
  }
  /**
   * @param int
   */
  public function setOldSourcePort($oldSourcePort)
  {
    $this->oldSourcePort = $oldSourcePort;
  }
  /**
   * @return int
   */
  public function getOldSourcePort()
  {
    return $this->oldSourcePort;
  }
  /**
   * @param string
   */
  public function setProtocol($protocol)
  {
    $this->protocol = $protocol;
  }
  /**
   * @return string
   */
  public function getProtocol()
  {
    return $this->protocol;
  }
  /**
   * @param string
   */
  public function setSubnetUri($subnetUri)
  {
    $this->subnetUri = $subnetUri;
  }
  /**
   * @return string
   */
  public function getSubnetUri()
  {
    return $this->subnetUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProxyConnectionInfo::class, 'Google_Service_NetworkManagement_ProxyConnectionInfo');
