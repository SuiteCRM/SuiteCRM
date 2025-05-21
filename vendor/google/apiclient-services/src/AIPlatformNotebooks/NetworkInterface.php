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

namespace Google\Service\AIPlatformNotebooks;

class NetworkInterface extends \Google\Collection
{
  protected $collection_key = 'accessConfigs';
  protected $accessConfigsType = AccessConfig::class;
  protected $accessConfigsDataType = 'array';
  /**
   * @var string
   */
  public $network;
  /**
   * @var string
   */
  public $nicType;
  /**
   * @var string
   */
  public $subnet;

  /**
   * @param AccessConfig[]
   */
  public function setAccessConfigs($accessConfigs)
  {
    $this->accessConfigs = $accessConfigs;
  }
  /**
   * @return AccessConfig[]
   */
  public function getAccessConfigs()
  {
    return $this->accessConfigs;
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
  public function setNicType($nicType)
  {
    $this->nicType = $nicType;
  }
  /**
   * @return string
   */
  public function getNicType()
  {
    return $this->nicType;
  }
  /**
   * @param string
   */
  public function setSubnet($subnet)
  {
    $this->subnet = $subnet;
  }
  /**
   * @return string
   */
  public function getSubnet()
  {
    return $this->subnet;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(NetworkInterface::class, 'Google_Service_AIPlatformNotebooks_NetworkInterface');
