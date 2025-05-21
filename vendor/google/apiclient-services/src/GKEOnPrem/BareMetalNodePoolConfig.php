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

class BareMetalNodePoolConfig extends \Google\Collection
{
  protected $collection_key = 'taints';
  protected $kubeletConfigType = BareMetalKubeletConfig::class;
  protected $kubeletConfigDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  protected $nodeConfigsType = BareMetalNodeConfig::class;
  protected $nodeConfigsDataType = 'array';
  /**
   * @var string
   */
  public $operatingSystem;
  protected $taintsType = NodeTaint::class;
  protected $taintsDataType = 'array';

  /**
   * @param BareMetalKubeletConfig
   */
  public function setKubeletConfig(BareMetalKubeletConfig $kubeletConfig)
  {
    $this->kubeletConfig = $kubeletConfig;
  }
  /**
   * @return BareMetalKubeletConfig
   */
  public function getKubeletConfig()
  {
    return $this->kubeletConfig;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
  }
  /**
   * @param BareMetalNodeConfig[]
   */
  public function setNodeConfigs($nodeConfigs)
  {
    $this->nodeConfigs = $nodeConfigs;
  }
  /**
   * @return BareMetalNodeConfig[]
   */
  public function getNodeConfigs()
  {
    return $this->nodeConfigs;
  }
  /**
   * @param string
   */
  public function setOperatingSystem($operatingSystem)
  {
    $this->operatingSystem = $operatingSystem;
  }
  /**
   * @return string
   */
  public function getOperatingSystem()
  {
    return $this->operatingSystem;
  }
  /**
   * @param NodeTaint[]
   */
  public function setTaints($taints)
  {
    $this->taints = $taints;
  }
  /**
   * @return NodeTaint[]
   */
  public function getTaints()
  {
    return $this->taints;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BareMetalNodePoolConfig::class, 'Google_Service_GKEOnPrem_BareMetalNodePoolConfig');
