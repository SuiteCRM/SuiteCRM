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

class BareMetalBgpPeerConfig extends \Google\Collection
{
  protected $collection_key = 'controlPlaneNodes';
  /**
   * @var string
   */
  public $asn;
  /**
   * @var string[]
   */
  public $controlPlaneNodes;
  /**
   * @var string
   */
  public $ipAddress;

  /**
   * @param string
   */
  public function setAsn($asn)
  {
    $this->asn = $asn;
  }
  /**
   * @return string
   */
  public function getAsn()
  {
    return $this->asn;
  }
  /**
   * @param string[]
   */
  public function setControlPlaneNodes($controlPlaneNodes)
  {
    $this->controlPlaneNodes = $controlPlaneNodes;
  }
  /**
   * @return string[]
   */
  public function getControlPlaneNodes()
  {
    return $this->controlPlaneNodes;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BareMetalBgpPeerConfig::class, 'Google_Service_GKEOnPrem_BareMetalBgpPeerConfig');
