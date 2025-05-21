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

class VmwareCluster extends \Google\Model
{
  /**
   * @var string
   */
  public $adminClusterMembership;
  /**
   * @var string
   */
  public $adminClusterName;
  /**
   * @var string[]
   */
  public $annotations;
  protected $antiAffinityGroupsType = VmwareAAGConfig::class;
  protected $antiAffinityGroupsDataType = '';
  protected $authorizationType = Authorization::class;
  protected $authorizationDataType = '';
  protected $autoRepairConfigType = VmwareAutoRepairConfig::class;
  protected $autoRepairConfigDataType = '';
  protected $binaryAuthorizationType = BinaryAuthorization::class;
  protected $binaryAuthorizationDataType = '';
  protected $controlPlaneNodeType = VmwareControlPlaneNodeConfig::class;
  protected $controlPlaneNodeDataType = '';
  /**
   * @var string
   */
  public $createTime;
  protected $dataplaneV2Type = VmwareDataplaneV2Config::class;
  protected $dataplaneV2DataType = '';
  /**
   * @var string
   */
  public $deleteTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var bool
   */
  public $disableBundledIngress;
  /**
   * @var bool
   */
  public $enableControlPlaneV2;
  /**
   * @var string
   */
  public $endpoint;
  /**
   * @var string
   */
  public $etag;
  protected $fleetType = Fleet::class;
  protected $fleetDataType = '';
  protected $loadBalancerType = VmwareLoadBalancerConfig::class;
  protected $loadBalancerDataType = '';
  /**
   * @var string
   */
  public $localName;
  /**
   * @var string
   */
  public $name;
  protected $networkConfigType = VmwareNetworkConfig::class;
  protected $networkConfigDataType = '';
  /**
   * @var string
   */
  public $onPremVersion;
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
  protected $storageType = VmwareStorageConfig::class;
  protected $storageDataType = '';
  /**
   * @var string
   */
  public $uid;
  /**
   * @var string
   */
  public $updateTime;
  protected $upgradePolicyType = VmwareClusterUpgradePolicy::class;
  protected $upgradePolicyDataType = '';
  protected $validationCheckType = ValidationCheck::class;
  protected $validationCheckDataType = '';
  protected $vcenterType = VmwareVCenterConfig::class;
  protected $vcenterDataType = '';
  /**
   * @var bool
   */
  public $vmTrackingEnabled;

  /**
   * @param string
   */
  public function setAdminClusterMembership($adminClusterMembership)
  {
    $this->adminClusterMembership = $adminClusterMembership;
  }
  /**
   * @return string
   */
  public function getAdminClusterMembership()
  {
    return $this->adminClusterMembership;
  }
  /**
   * @param string
   */
  public function setAdminClusterName($adminClusterName)
  {
    $this->adminClusterName = $adminClusterName;
  }
  /**
   * @return string
   */
  public function getAdminClusterName()
  {
    return $this->adminClusterName;
  }
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
   * @param VmwareAAGConfig
   */
  public function setAntiAffinityGroups(VmwareAAGConfig $antiAffinityGroups)
  {
    $this->antiAffinityGroups = $antiAffinityGroups;
  }
  /**
   * @return VmwareAAGConfig
   */
  public function getAntiAffinityGroups()
  {
    return $this->antiAffinityGroups;
  }
  /**
   * @param Authorization
   */
  public function setAuthorization(Authorization $authorization)
  {
    $this->authorization = $authorization;
  }
  /**
   * @return Authorization
   */
  public function getAuthorization()
  {
    return $this->authorization;
  }
  /**
   * @param VmwareAutoRepairConfig
   */
  public function setAutoRepairConfig(VmwareAutoRepairConfig $autoRepairConfig)
  {
    $this->autoRepairConfig = $autoRepairConfig;
  }
  /**
   * @return VmwareAutoRepairConfig
   */
  public function getAutoRepairConfig()
  {
    return $this->autoRepairConfig;
  }
  /**
   * @param BinaryAuthorization
   */
  public function setBinaryAuthorization(BinaryAuthorization $binaryAuthorization)
  {
    $this->binaryAuthorization = $binaryAuthorization;
  }
  /**
   * @return BinaryAuthorization
   */
  public function getBinaryAuthorization()
  {
    return $this->binaryAuthorization;
  }
  /**
   * @param VmwareControlPlaneNodeConfig
   */
  public function setControlPlaneNode(VmwareControlPlaneNodeConfig $controlPlaneNode)
  {
    $this->controlPlaneNode = $controlPlaneNode;
  }
  /**
   * @return VmwareControlPlaneNodeConfig
   */
  public function getControlPlaneNode()
  {
    return $this->controlPlaneNode;
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
   * @param VmwareDataplaneV2Config
   */
  public function setDataplaneV2(VmwareDataplaneV2Config $dataplaneV2)
  {
    $this->dataplaneV2 = $dataplaneV2;
  }
  /**
   * @return VmwareDataplaneV2Config
   */
  public function getDataplaneV2()
  {
    return $this->dataplaneV2;
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
   * @param bool
   */
  public function setDisableBundledIngress($disableBundledIngress)
  {
    $this->disableBundledIngress = $disableBundledIngress;
  }
  /**
   * @return bool
   */
  public function getDisableBundledIngress()
  {
    return $this->disableBundledIngress;
  }
  /**
   * @param bool
   */
  public function setEnableControlPlaneV2($enableControlPlaneV2)
  {
    $this->enableControlPlaneV2 = $enableControlPlaneV2;
  }
  /**
   * @return bool
   */
  public function getEnableControlPlaneV2()
  {
    return $this->enableControlPlaneV2;
  }
  /**
   * @param string
   */
  public function setEndpoint($endpoint)
  {
    $this->endpoint = $endpoint;
  }
  /**
   * @return string
   */
  public function getEndpoint()
  {
    return $this->endpoint;
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
   * @param Fleet
   */
  public function setFleet(Fleet $fleet)
  {
    $this->fleet = $fleet;
  }
  /**
   * @return Fleet
   */
  public function getFleet()
  {
    return $this->fleet;
  }
  /**
   * @param VmwareLoadBalancerConfig
   */
  public function setLoadBalancer(VmwareLoadBalancerConfig $loadBalancer)
  {
    $this->loadBalancer = $loadBalancer;
  }
  /**
   * @return VmwareLoadBalancerConfig
   */
  public function getLoadBalancer()
  {
    return $this->loadBalancer;
  }
  /**
   * @param string
   */
  public function setLocalName($localName)
  {
    $this->localName = $localName;
  }
  /**
   * @return string
   */
  public function getLocalName()
  {
    return $this->localName;
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
   * @param VmwareNetworkConfig
   */
  public function setNetworkConfig(VmwareNetworkConfig $networkConfig)
  {
    $this->networkConfig = $networkConfig;
  }
  /**
   * @return VmwareNetworkConfig
   */
  public function getNetworkConfig()
  {
    return $this->networkConfig;
  }
  /**
   * @param string
   */
  public function setOnPremVersion($onPremVersion)
  {
    $this->onPremVersion = $onPremVersion;
  }
  /**
   * @return string
   */
  public function getOnPremVersion()
  {
    return $this->onPremVersion;
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
   * @param VmwareStorageConfig
   */
  public function setStorage(VmwareStorageConfig $storage)
  {
    $this->storage = $storage;
  }
  /**
   * @return VmwareStorageConfig
   */
  public function getStorage()
  {
    return $this->storage;
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
   * @param VmwareClusterUpgradePolicy
   */
  public function setUpgradePolicy(VmwareClusterUpgradePolicy $upgradePolicy)
  {
    $this->upgradePolicy = $upgradePolicy;
  }
  /**
   * @return VmwareClusterUpgradePolicy
   */
  public function getUpgradePolicy()
  {
    return $this->upgradePolicy;
  }
  /**
   * @param ValidationCheck
   */
  public function setValidationCheck(ValidationCheck $validationCheck)
  {
    $this->validationCheck = $validationCheck;
  }
  /**
   * @return ValidationCheck
   */
  public function getValidationCheck()
  {
    return $this->validationCheck;
  }
  /**
   * @param VmwareVCenterConfig
   */
  public function setVcenter(VmwareVCenterConfig $vcenter)
  {
    $this->vcenter = $vcenter;
  }
  /**
   * @return VmwareVCenterConfig
   */
  public function getVcenter()
  {
    return $this->vcenter;
  }
  /**
   * @param bool
   */
  public function setVmTrackingEnabled($vmTrackingEnabled)
  {
    $this->vmTrackingEnabled = $vmTrackingEnabled;
  }
  /**
   * @return bool
   */
  public function getVmTrackingEnabled()
  {
    return $this->vmTrackingEnabled;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(VmwareCluster::class, 'Google_Service_GKEOnPrem_VmwareCluster');
