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

class NetworkAttachmentConnectedEndpoint extends \Google\Collection
{
  protected $collection_key = 'secondaryIpCidrRanges';
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
  public $projectIdOrNum;
  /**
   * @var string[]
   */
  public $secondaryIpCidrRanges;
  /**
   * @var string
   */
  public $status;
  /**
   * @var string
   */
  public $subnetwork;
  /**
   * @var string
   */
  public $subnetworkCidrRange;

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
  public function setProjectIdOrNum($projectIdOrNum)
  {
    $this->projectIdOrNum = $projectIdOrNum;
  }
  /**
   * @return string
   */
  public function getProjectIdOrNum()
  {
    return $this->projectIdOrNum;
  }
  /**
   * @param string[]
   */
  public function setSecondaryIpCidrRanges($secondaryIpCidrRanges)
  {
    $this->secondaryIpCidrRanges = $secondaryIpCidrRanges;
  }
  /**
   * @return string[]
   */
  public function getSecondaryIpCidrRanges()
  {
    return $this->secondaryIpCidrRanges;
  }
  /**
   * @param string
   */
  public function setStatus($status)
  {
    $this->status = $status;
  }
  /**
   * @return string
   */
  public function getStatus()
  {
    return $this->status;
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
  /**
   * @param string
   */
  public function setSubnetworkCidrRange($subnetworkCidrRange)
  {
    $this->subnetworkCidrRange = $subnetworkCidrRange;
  }
  /**
   * @return string
   */
  public function getSubnetworkCidrRange()
  {
    return $this->subnetworkCidrRange;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(NetworkAttachmentConnectedEndpoint::class, 'Google_Service_Compute_NetworkAttachmentConnectedEndpoint');
