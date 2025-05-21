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

namespace Google\Service\NetAppFiles;

class HybridPeeringDetails extends \Google\Model
{
  /**
   * @var string
   */
  public $command;
  /**
   * @var string
   */
  public $commandExpiryTime;
  /**
   * @var string
   */
  public $passphrase;
  /**
   * @var string
   */
  public $peerClusterName;
  /**
   * @var string
   */
  public $peerSvmName;
  /**
   * @var string
   */
  public $peerVolumeName;
  /**
   * @var string
   */
  public $subnetIp;

  /**
   * @param string
   */
  public function setCommand($command)
  {
    $this->command = $command;
  }
  /**
   * @return string
   */
  public function getCommand()
  {
    return $this->command;
  }
  /**
   * @param string
   */
  public function setCommandExpiryTime($commandExpiryTime)
  {
    $this->commandExpiryTime = $commandExpiryTime;
  }
  /**
   * @return string
   */
  public function getCommandExpiryTime()
  {
    return $this->commandExpiryTime;
  }
  /**
   * @param string
   */
  public function setPassphrase($passphrase)
  {
    $this->passphrase = $passphrase;
  }
  /**
   * @return string
   */
  public function getPassphrase()
  {
    return $this->passphrase;
  }
  /**
   * @param string
   */
  public function setPeerClusterName($peerClusterName)
  {
    $this->peerClusterName = $peerClusterName;
  }
  /**
   * @return string
   */
  public function getPeerClusterName()
  {
    return $this->peerClusterName;
  }
  /**
   * @param string
   */
  public function setPeerSvmName($peerSvmName)
  {
    $this->peerSvmName = $peerSvmName;
  }
  /**
   * @return string
   */
  public function getPeerSvmName()
  {
    return $this->peerSvmName;
  }
  /**
   * @param string
   */
  public function setPeerVolumeName($peerVolumeName)
  {
    $this->peerVolumeName = $peerVolumeName;
  }
  /**
   * @return string
   */
  public function getPeerVolumeName()
  {
    return $this->peerVolumeName;
  }
  /**
   * @param string
   */
  public function setSubnetIp($subnetIp)
  {
    $this->subnetIp = $subnetIp;
  }
  /**
   * @return string
   */
  public function getSubnetIp()
  {
    return $this->subnetIp;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(HybridPeeringDetails::class, 'Google_Service_NetAppFiles_HybridPeeringDetails');
