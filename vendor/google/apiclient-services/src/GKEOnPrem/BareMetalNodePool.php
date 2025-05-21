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

class BareMetalNodePool extends \Google\Model
{
  /**
   * @var string[]
   */
  public $annotations;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $deleteTime;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $etag;
  /**
   * @var string
   */
  public $name;
  protected $nodePoolConfigType = BareMetalNodePoolConfig::class;
  protected $nodePoolConfigDataType = '';
  /**
   * @var bool
   */
  public $reconciling;
  /**
   * @var string
   */
  public $state;
  protected $statusType = ResourceStatus::class;
  protected $statusDataType = '';
  /**
   * @var string
   */
  public $uid;
  /**
   * @var string
   */
  public $updateTime;
  protected $upgradePolicyType = BareMetalNodePoolUpgradePolicy::class;
  protected $upgradePolicyDataType = '';

  /**
   * @param string[]
   */
  public function setAnnotations($annotations)
  {
    $this->annotations = $annotations;
  }
  /**
   * @return string[]
   */
  public function getAnnotations()
  {
    return $this->annotations;
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
   * @param string
   */
  public function setDeleteTime($deleteTime)
  {
    $this->deleteTime = $deleteTime;
  }
  /**
   * @return string
   */
  public function getDeleteTime()
  {
    return $this->deleteTime;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param string
   */
  public function setEtag($etag)
  {
    $this->etag = $etag;
  }
  /**
   * @return string
   */
  public function getEtag()
  {
    return $this->etag;
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
   * @param BareMetalNodePoolConfig
   */
  public function setNodePoolConfig(BareMetalNodePoolConfig $nodePoolConfig)
  {
    $this->nodePoolConfig = $nodePoolConfig;
  }
  /**
   * @return BareMetalNodePoolConfig
   */
  public function getNodePoolConfig()
  {
    return $this->nodePoolConfig;
  }
  /**
   * @param bool
   */
  public function setReconciling($reconciling)
  {
    $this->reconciling = $reconciling;
  }
  /**
   * @return bool
   */
  public function getReconciling()
  {
    return $this->reconciling;
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
   * @param ResourceStatus
   */
  public function setStatus(ResourceStatus $status)
  {
    $this->status = $status;
  }
  /**
   * @return ResourceStatus
   */
  public function getStatus()
  {
    return $this->status;
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
  /**
   * @param BareMetalNodePoolUpgradePolicy
   */
  public function setUpgradePolicy(BareMetalNodePoolUpgradePolicy $upgradePolicy)
  {
    $this->upgradePolicy = $upgradePolicy;
  }
  /**
   * @return BareMetalNodePoolUpgradePolicy
   */
  public function getUpgradePolicy()
  {
    return $this->upgradePolicy;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BareMetalNodePool::class, 'Google_Service_GKEOnPrem_BareMetalNodePool');
