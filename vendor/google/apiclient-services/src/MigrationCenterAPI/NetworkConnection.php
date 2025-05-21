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

namespace Google\Service\MigrationCenterAPI;

class NetworkConnection extends \Google\Model
{
  /**
   * @var string
   */
  public $localIpAddress;
  /**
   * @var int
   */
  public $localPort;
  /**
   * @var string
   */
  public $pid;
  /**
   * @var string
   */
  public $processName;
  /**
   * @var string
   */
  public $protocol;
  /**
   * @var string
   */
  public $remoteIpAddress;
  /**
   * @var int
   */
  public $remotePort;
  /**
   * @var string
   */
  public $state;

  /**
   * @param string
   */
  public function setLocalIpAddress($localIpAddress)
  {
    $this->localIpAddress = $localIpAddress;
  }
  /**
   * @return string
   */
  public function getLocalIpAddress()
  {
    return $this->localIpAddress;
  }
  /**
   * @param int
   */
  public function setLocalPort($localPort)
  {
    $this->localPort = $localPort;
  }
  /**
   * @return int
   */
  public function getLocalPort()
  {
    return $this->localPort;
  }
  /**
   * @param string
   */
  public function setPid($pid)
  {
    $this->pid = $pid;
  }
  /**
   * @return string
   */
  public function getPid()
  {
    return $this->pid;
  }
  /**
   * @param string
   */
  public function setProcessName($processName)
  {
    $this->processName = $processName;
  }
  /**
   * @return string
   */
  public function getProcessName()
  {
    return $this->processName;
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
  public function setRemoteIpAddress($remoteIpAddress)
  {
    $this->remoteIpAddress = $remoteIpAddress;
  }
  /**
   * @return string
   */
  public function getRemoteIpAddress()
  {
    return $this->remoteIpAddress;
  }
  /**
   * @param int
   */
  public function setRemotePort($remotePort)
  {
    $this->remotePort = $remotePort;
  }
  /**
   * @return int
   */
  public function getRemotePort()
  {
    return $this->remotePort;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(NetworkConnection::class, 'Google_Service_MigrationCenterAPI_NetworkConnection');
