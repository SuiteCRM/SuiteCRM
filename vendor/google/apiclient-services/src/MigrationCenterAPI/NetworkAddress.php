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

class NetworkAddress extends \Google\Model
{
  /**
   * @var string
   */
  public $assignment;
  /**
   * @var string
   */
  public $bcast;
  /**
   * @var string
   */
  public $fqdn;
  /**
   * @var string
   */
  public $ipAddress;
  /**
   * @var string
   */
  public $subnetMask;

  /**
   * @param string
   */
  public function setAssignment($assignment)
  {
    $this->assignment = $assignment;
  }
  /**
   * @return string
   */
  public function getAssignment()
  {
    return $this->assignment;
  }
  /**
   * @param string
   */
  public function setBcast($bcast)
  {
    $this->bcast = $bcast;
  }
  /**
   * @return string
   */
  public function getBcast()
  {
    return $this->bcast;
  }
  /**
   * @param string
   */
  public function setFqdn($fqdn)
  {
    $this->fqdn = $fqdn;
  }
  /**
   * @return string
   */
  public function getFqdn()
  {
    return $this->fqdn;
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
  public function setSubnetMask($subnetMask)
  {
    $this->subnetMask = $subnetMask;
  }
  /**
   * @return string
   */
  public function getSubnetMask()
  {
    return $this->subnetMask;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(NetworkAddress::class, 'Google_Service_MigrationCenterAPI_NetworkAddress');
