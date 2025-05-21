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

class PrivateCloud extends \Google\Model
{
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
  public $description;
  /**
   * @var string
   */
  public $expireTime;
  protected $hcxType = Hcx::class;
  protected $hcxDataType = '';
  protected $managementClusterType = ManagementCluster::class;
  protected $managementClusterDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $networkConfigType = NetworkConfig::class;
  protected $networkConfigDataType = '';
  protected $nsxType = Nsx::class;
  protected $nsxDataType = '';
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $type;
  /**
   * @var string
   */
  public $uid;
  /**
   * @var string
   */
  public $updateTime;
  protected $vcenterType = Vcenter::class;
  protected $vcenterDataType = '';

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
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setExpireTime($expireTime)
  {
    $this->expireTime = $expireTime;
  }
  /**
   * @return string
   */
  public function getExpireTime()
  {
    return $this->expireTime;
  }
  /**
   * @param Hcx
   */
  public function setHcx(Hcx $hcx)
  {
    $this->hcx = $hcx;
  }
  /**
   * @return Hcx
   */
  public function getHcx()
  {
    return $this->hcx;
  }
  /**
   * @param ManagementCluster
   */
  public function setManagementCluster(ManagementCluster $managementCluster)
  {
    $this->managementCluster = $managementCluster;
  }
  /**
   * @return ManagementCluster
   */
  public function getManagementCluster()
  {
    return $this->managementCluster;
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
   * @param NetworkConfig
   */
  public function setNetworkConfig(NetworkConfig $networkConfig)
  {
    $this->networkConfig = $networkConfig;
  }
  /**
   * @return NetworkConfig
   */
  public function getNetworkConfig()
  {
    return $this->networkConfig;
  }
  /**
   * @param Nsx
   */
  public function setNsx(Nsx $nsx)
  {
    $this->nsx = $nsx;
  }
  /**
   * @return Nsx
   */
  public function getNsx()
  {
    return $this->nsx;
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
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
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
   * @param Vcenter
   */
  public function setVcenter(Vcenter $vcenter)
  {
    $this->vcenter = $vcenter;
  }
  /**
   * @return Vcenter
   */
  public function getVcenter()
  {
    return $this->vcenter;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PrivateCloud::class, 'Google_Service_VMwareEngine_PrivateCloud');
