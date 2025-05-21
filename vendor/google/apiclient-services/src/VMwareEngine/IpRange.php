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

namespace Google\Service\VMwareEngine;

class IpRange extends \Google\Model
{
  /**
   * @var string
   */
  public $externalAddress;
  /**
   * @var string
   */
  public $ipAddress;
  /**
   * @var string
   */
  public $ipAddressRange;

  /**
   * @param string
   */
  public function setExternalAddress($externalAddress)
  {
    $this->externalAddress = $externalAddress;
  }
  /**
   * @return string
   */
  public function getExternalAddress()
  {
    return $this->externalAddress;
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
  public function setIpAddressRange($ipAddressRange)
  {
    $this->ipAddressRange = $ipAddressRange;
  }
  /**
   * @return string
   */
  public function getIpAddressRange()
  {
    return $this->ipAddressRange;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(IpRange::class, 'Google_Service_VMwareEngine_IpRange');
