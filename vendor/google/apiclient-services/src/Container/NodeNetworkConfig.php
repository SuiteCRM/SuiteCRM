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

class NodeNetworkConfig extends \Google\Collection
{
  protected $collection_key = 'additionalPodNetworkConfigs';
  protected $additionalNodeNetworkConfigsType = AdditionalNodeNetworkConfig::class;
  protected $additionalNodeNetworkConfigsDataType = 'array';
  protected $additionalPodNetworkConfigsType = AdditionalPodNetworkConfig::class;
  protected $additionalPodNetworkConfigsDataType = 'array';
  /**
   * @var bool
   */
  public $createPodRange;
  /**
   * @var bool
   */
  public $enablePrivateNodes;
  protected $networkPerformanceConfigType = NetworkPerformanceConfig::class;
  protected $networkPerformanceConfigDataType = '';
  protected $podCidrOverprovisionConfigType = PodCIDROverprovisionConfig::class;
  protected $podCidrOverprovisionConfigDataType = '';
  /**
   * @var string
   */
  public $podIpv4CidrBlock;
  public $podIpv4RangeUtilization;
  /**
   * @var string
   */
  public $podRange;

  /**
   * @param AdditionalNodeNetworkConfig[]
   */
  public function setAdditionalNodeNetworkConfigs($additionalNodeNetworkConfigs)
  {
    $this->additionalNodeNetworkConfigs = $additionalNodeNetworkConfigs;
  }
  /**
   * @return AdditionalNodeNetworkConfig[]
   */
  public function getAdditionalNodeNetworkConfigs()
  {
    return $this->additionalNodeNetworkConfigs;
  }
  /**
   * @param AdditionalPodNetworkConfig[]
   */
  public function setAdditionalPodNetworkConfigs($additionalPodNetworkConfigs)
  {
    $this->additionalPodNetworkConfigs = $additionalPodNetworkConfigs;
  }
  /**
   * @return AdditionalPodNetworkConfig[]
   */
  public function getAdditionalPodNetworkConfigs()
  {
    return $this->additionalPodNetworkConfigs;
  }
  /**
   * @param bool
   */
  public function setCreatePodRange($createPodRange)
  {
    $this->createPodRange = $createPodRange;
  }
  /**
   * @return bool
   */
  public function getCreatePodRange()
  {
    return $this->createPodRange;
  }
  /**
   * @param bool
   */
  public function setEnablePrivateNodes($enablePrivateNodes)
  {
    $this->enablePrivateNodes = $enablePrivateNodes;
  }
  /**
   * @return bool
   */
  public function getEnablePrivateNodes()
  {
    return $this->enablePrivateNodes;
  }
  /**
   * @param NetworkPerformanceConfig
   */
  public function setNetworkPerformanceConfig(NetworkPerformanceConfig $networkPerformanceConfig)
  {
    $this->networkPerformanceConfig = $networkPerformanceConfig;
  }
  /**
   * @return NetworkPerformanceConfig
   */
  public function getNetworkPerformanceConfig()
  {
    return $this->networkPerformanceConfig;
  }
  /**
   * @param PodCIDROverprovisionConfig
   */
  public function setPodCidrOverprovisionConfig(PodCIDROverprovisionConfig $podCidrOverprovisionConfig)
  {
    $this->podCidrOverprovisionConfig = $podCidrOverprovisionConfig;
  }
  /**
   * @return PodCIDROverprovisionConfig
   */
  public function getPodCidrOverprovisionConfig()
  {
    return $this->podCidrOverprovisionConfig;
  }
  /**
   * @param string
   */
  public function setPodIpv4CidrBlock($podIpv4CidrBlock)
  {
    $this->podIpv4CidrBlock = $podIpv4CidrBlock;
  }
  /**
   * @return string
   */
  public function getPodIpv4CidrBlock()
  {
    return $this->podIpv4CidrBlock;
  }
  public function setPodIpv4RangeUtilization($podIpv4RangeUtilization)
  {
    $this->podIpv4RangeUtilization = $podIpv4RangeUtilization;
  }
  public function getPodIpv4RangeUtilization()
  {
    return $this->podIpv4RangeUtilization;
  }
  /**
   * @param string
   */
  public function setPodRange($podRange)
  {
    $this->podRange = $podRange;
  }
  /**
   * @return string
   */
  public function getPodRange()
  {
    return $this->podRange;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(NodeNetworkConfig::class, 'Google_Service_Container_NodeNetworkConfig');
