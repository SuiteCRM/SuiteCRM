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

class BareMetalNetworkConfig extends \Google\Model
{
  /**
   * @var bool
   */
  public $advancedNetworking;
  protected $islandModeCidrType = BareMetalIslandModeCidrConfig::class;
  protected $islandModeCidrDataType = '';
  protected $multipleNetworkInterfacesConfigType = BareMetalMultipleNetworkInterfacesConfig::class;
  protected $multipleNetworkInterfacesConfigDataType = '';
  protected $srIovConfigType = BareMetalSrIovConfig::class;
  protected $srIovConfigDataType = '';

  /**
   * @param bool
   */
  public function setAdvancedNetworking($advancedNetworking)
  {
    $this->advancedNetworking = $advancedNetworking;
  }
  /**
   * @return bool
   */
  public function getAdvancedNetworking()
  {
    return $this->advancedNetworking;
  }
  /**
   * @param BareMetalIslandModeCidrConfig
   */
  public function setIslandModeCidr(BareMetalIslandModeCidrConfig $islandModeCidr)
  {
    $this->islandModeCidr = $islandModeCidr;
  }
  /**
   * @return BareMetalIslandModeCidrConfig
   */
  public function getIslandModeCidr()
  {
    return $this->islandModeCidr;
  }
  /**
   * @param BareMetalMultipleNetworkInterfacesConfig
   */
  public function setMultipleNetworkInterfacesConfig(BareMetalMultipleNetworkInterfacesConfig $multipleNetworkInterfacesConfig)
  {
    $this->multipleNetworkInterfacesConfig = $multipleNetworkInterfacesConfig;
  }
  /**
   * @return BareMetalMultipleNetworkInterfacesConfig
   */
  public function getMultipleNetworkInterfacesConfig()
  {
    return $this->multipleNetworkInterfacesConfig;
  }
  /**
   * @param BareMetalSrIovConfig
   */
  public function setSrIovConfig(BareMetalSrIovConfig $srIovConfig)
  {
    $this->srIovConfig = $srIovConfig;
  }
  /**
   * @return BareMetalSrIovConfig
   */
  public function getSrIovConfig()
  {
    return $this->srIovConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BareMetalNetworkConfig::class, 'Google_Service_GKEOnPrem_BareMetalNetworkConfig');
