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

class Cluster extends \Google\Model
{
  protected $autoscalingSettingsType = AutoscalingSettings::class;
  protected $autoscalingSettingsDataType = '';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var bool
   */
  public $management;
  /**
   * @var string
   */
  public $name;
  protected $nodeTypeConfigsType = NodeTypeConfig::class;
  protected $nodeTypeConfigsDataType = 'map';
  /**
   * @var string
   */
  public $state;
  protected $stretchedClusterConfigType = StretchedClusterConfig::class;
  protected $stretchedClusterConfigDataType = '';
  /**
   * @var string
   */
  public $uid;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param AutoscalingSettings
   */
  public function setAutoscalingSettings(AutoscalingSettings $autoscalingSettings)
  {
    $this->autoscalingSettings = $autoscalingSettings;
  }
  /**
   * @return AutoscalingSettings
   */
  public function getAutoscalingSettings()
  {
    return $this->autoscalingSettings;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param bool
   */
  public function setManagement($management)
  {
    $this->management = $management;
  }
  /**
   * @return bool
   */
  public function getManagement()
  {
    return $this->management;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
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
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
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
  /**
   * @param string
   */
  public function setUid($uid)
  {
    $this->uid = $uid;
  }
  /**
   * @return string
   */
  public function getUid()
  {
    return $this->uid;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Cluster::class, 'Google_Service_VMwareEngine_Cluster');
