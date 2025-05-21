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

namespace Google\Service\Container;

class IPEndpointsConfig extends \Google\Model
{
  protected $authorizedNetworksConfigType = MasterAuthorizedNetworksConfig::class;
  protected $authorizedNetworksConfigDataType = '';
  /**
   * @var bool
   */
  public $enablePublicEndpoint;
  /**
   * @var bool
   */
  public $enabled;
  /**
   * @var bool
   */
  public $globalAccess;
  /**
   * @var string
   */
  public $privateEndpoint;
  /**
   * @var string
   */
  public $privateEndpointSubnetwork;
  /**
   * @var string
   */
  public $publicEndpoint;

  /**
   * @param MasterAuthorizedNetworksConfig
   */
  public function setAuthorizedNetworksConfig(MasterAuthorizedNetworksConfig $authorizedNetworksConfig)
  {
    $this->authorizedNetworksConfig = $authorizedNetworksConfig;
  }
  /**
   * @return MasterAuthorizedNetworksConfig
   */
  public function getAuthorizedNetworksConfig()
  {
    return $this->authorizedNetworksConfig;
  }
  /**
   * @param bool
   */
  public function setEnablePublicEndpoint($enablePublicEndpoint)
  {
    $this->enablePublicEndpoint = $enablePublicEndpoint;
  }
  /**
   * @return bool
   */
  public function getEnablePublicEndpoint()
  {
    return $this->enablePublicEndpoint;
  }
  /**
   * @param bool
   */
  public function setEnabled($enabled)
  {
    $this->enabled = $enabled;
  }
  /**
   * @return bool
   */
  public function getEnabled()
  {
    return $this->enabled;
  }
  /**
   * @param bool
   */
  public function setGlobalAccess($globalAccess)
  {
    $this->globalAccess = $globalAccess;
  }
  /**
   * @return bool
   */
  public function getGlobalAccess()
  {
    return $this->globalAccess;
  }
  /**
   * @param string
   */
  public function setPrivateEndpoint($privateEndpoint)
  {
    $this->privateEndpoint = $privateEndpoint;
  }
  /**
   * @return string
   */
  public function getPrivateEndpoint()
  {
    return $this->privateEndpoint;
  }
  /**
   * @param string
   */
  public function setPrivateEndpointSubnetwork($privateEndpointSubnetwork)
  {
    $this->privateEndpointSubnetwork = $privateEndpointSubnetwork;
  }
  /**
   * @return string
   */
  public function getPrivateEndpointSubnetwork()
  {
    return $this->privateEndpointSubnetwork;
  }
  /**
   * @param string
   */
  public function setPublicEndpoint($publicEndpoint)
  {
    $this->publicEndpoint = $publicEndpoint;
  }
  /**
   * @return string
   */
  public function getPublicEndpoint()
  {
    return $this->publicEndpoint;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(IPEndpointsConfig::class, 'Google_Service_Container_IPEndpointsConfig');
