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

class ManagementCluster extends \Google\Model
{
  /**
   * @var string
   */
  public $clusterId;
  protected $nodeTypeConfigsType = NodeTypeConfig::class;
  protected $nodeTypeConfigsDataType = 'map';
  protected $stretchedClusterConfigType = StretchedClusterConfig::class;
  protected $stretchedClusterConfigDataType = '';

  /**
   * @param string
   */
  public function setClusterId($clusterId)
  {
    $this->clusterId = $clusterId;
  }
  /**
   * @return string
   */
  public function getClusterId()
  {
    return $this->clusterId;
  }
  /**
   * @param NodeTypeConfig[]
   */
  public function setNodeTypeConfigs($nodeTypeConfigs)
  {
    $this->nodeTypeConfigs = $nodeTypeConfigs;
  }
  /**
   * @return NodeTypeConfig[]
   */
  public function getNodeTypeConfigs()
  {
    return $this->nodeTypeConfigs;
  }
  /**
   * @param StretchedClusterConfig
   */
  public function setStretchedClusterConfig(StretchedClusterConfig $stretchedClusterConfig)
  {
    $this->stretchedClusterConfig = $stretchedClusterConfig;
  }
  /**
   * @return StretchedClusterConfig
   */
  public function getStretchedClusterConfig()
  {
    return $this->stretchedClusterConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ManagementCluster::class, 'Google_Service_VMwareEngine_ManagementCluster');
