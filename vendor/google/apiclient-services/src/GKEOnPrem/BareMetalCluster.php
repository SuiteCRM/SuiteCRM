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

class BareMetalCluster extends \Google\Model
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
  /**
   * @var string
   */
  public $bareMetalVersion;
  protected $binaryAuthorizationType = BinaryAuthorization::class;
  protected $binaryAuthorizationDataType = '';
  protected $clusterOperationsType = BareMetalClusterOperationsConfig::class;
  protected $clusterOperationsDataType = '';
  protected $controlPlaneType = BareMetalControlPlaneConfig::class;
  protected $controlPlaneDataType = '';
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
  public $endpoint;
  /**
   * @var string
   */
  public $etag;
  protected $fleetType = Fleet::class;
  protected $fleetDataType = '';
  protected $loadBalancerType = BareMetalLoadBalancerConfig::class;
  protected $loadBalancerDataType = '';
  /**
   * @var string
   */
  public $localName;
  protected $maintenanceConfigType = BareMetalMaintenanceConfig::class;
  protected $maintenanceConfigDataType = '';
  protected $maintenanceStatusType = BareMetalMaintenanceStatus::class;
  protected $maintenanceStatusDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $networkConfigType = BareMetalNetworkConfig::class;
  protected $networkConfigDataType = '';
  protected $nodeAccessConfigType = BareMetalNodeAccessConfig::class;
  protected $nodeAccessConfigDataType = '';
  protected $nodeConfigType = BareMetalWorkloadNodeConfig::class;
  protected $nodeConfigDataType = '';
  protected $osEnvironmentConfigType = BareMetalOsEnvironmentConfig::class;
  protected $osEnvironmentConfigDataType = '';
  protected $proxyType = BareMetalProxyConfig::class;
  protected $proxyDataType = '';
  /**
   * @var bool
   */
  public $reconciling;
  protected $securityConfigType = BareMetalSecurityConfig::class;
  protected $securityConfigDataType = '';
  /**
   * @var string
   */
  public $state;
  protected $statusType = ResourceStatus::class;
  protected $statusDataType = '';
  protected $storageType = BareMetalStorageConfig::class;
  protected $storageDataType = '';
  /**
   * @var string
   */
  public $uid;
  /**
   * @var string
   */
  public $updateTime;
  protected $upgradePolicyType = BareMetalClusterUpgradePolicy::class;
  protected $upgradePolicyDataType = '';
  protected $validationCheckType = ValidationCheck::class;
  protected $validationCheckDataType = '';

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
   * @param string
   */
  public function setBareMetalVersion($bareMetalVersion)
  {
    $this->bareMetalVersion = $bareMetalVersion;
  }
  /**
   * @return string
   */
  public function getBareMetalVersion()
  {
    return $this->bareMetalVersion;
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
   * @param BareMetalClusterOperationsConfig
   */
  public function setClusterOperations(BareMetalClusterOperationsConfig $clusterOperations)
  {
    $this->clusterOperations = $clusterOperations;
  }
  /**
   * @return BareMetalClusterOperationsConfig
   */
  public function getClusterOperations()
  {
    return $this->clusterOperations;
  }
  /**
   * @param BareMetalControlPlaneConfig
   */
  public function setControlPlane(BareMetalControlPlaneConfig $controlPlane)
  {
    $this->controlPlane = $controlPlane;
  }
  /**
   * @return BareMetalControlPlaneConfig
   */
  public function getControlPlane()
  {
    return $this->controlPlane;
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
   * @param BareMetalLoadBalancerConfig
   */
  public function setLoadBalancer(BareMetalLoadBalancerConfig $loadBalancer)
  {
    $this->loadBalancer = $loadBalancer;
  }
  /**
   * @return BareMetalLoadBalancerConfig
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
   * @param BareMetalMaintenanceConfig
   */
  public function setMaintenanceConfig(BareMetalMaintenanceConfig $maintenanceConfig)
  {
    $this->maintenanceConfig = $maintenanceConfig;
  }
  /**
   * @return BareMetalMaintenanceConfig
   */
  public function getMaintenanceConfig()
  {
    return $this->maintenanceConfig;
  }
  /**
   * @param BareMetalMaintenanceStatus
   */
  public function setMaintenanceStatus(BareMetalMaintenanceStatus $maintenanceStatus)
  {
    $this->maintenanceStatus = $maintenanceStatus;
  }
  /**
   * @return BareMetalMaintenanceStatus
   */
  public function getMaintenanceStatus()
  {
    return $this->maintenanceStatus;
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
   * @param BareMetalNetworkConfig
   */
  public function setNetworkConfig(BareMetalNetworkConfig $networkConfig)
  {
    $this->networkConfig = $networkConfig;
  }
  /**
   * @return BareMetalNetworkConfig
   */
  public function getNetworkConfig()
  {
    return $this->networkConfig;
  }
  /**
   * @param BareMetalNodeAccessConfig
   */
  public function setNodeAccessConfig(BareMetalNodeAccessConfig $nodeAccessConfig)
  {
    $this->nodeAccessConfig = $nodeAccessConfig;
  }
  /**
   * @return BareMetalNodeAccessConfig
   */
  public function getNodeAccessConfig()
  {
    return $this->nodeAccessConfig;
  }
  /**
   * @param BareMetalWorkloadNodeConfig
   */
  public function setNodeConfig(BareMetalWorkloadNodeConfig $nodeConfig)
  {
    $this->nodeConfig = $nodeConfig;
  }
  /**
   * @return BareMetalWorkloadNodeConfig
   */
  public function getNodeConfig()
  {
    return $this->nodeConfig;
  }
  /**
   * @param BareMetalOsEnvironmentConfig
   */
  public function setOsEnvironmentConfig(BareMetalOsEnvironmentConfig $osEnvironmentConfig)
  {
    $this->osEnvironmentConfig = $osEnvironmentConfig;
  }
  /**
   * @return BareMetalOsEnvironmentConfig
   */
  public function getOsEnvironmentConfig()
  {
    return $this->osEnvironmentConfig;
  }
  /**
   * @param BareMetalProxyConfig
   */
  public function setProxy(BareMetalProxyConfig $proxy)
  {
    $this->proxy = $proxy;
  }
  /**
   * @return BareMetalProxyConfig
   */
  public function getProxy()
  {
    return $this->proxy;
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
   * @param BareMetalSecurityConfig
   */
  public function setSecurityConfig(BareMetalSecurityConfig $securityConfig)
  {
    $this->securityConfig = $securityConfig;
  }
  /**
   * @return BareMetalSecurityConfig
   */
  public function getSecurityConfig()
  {
    return $this->securityConfig;
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
   * @param BareMetalStorageConfig
   */
  public function setStorage(BareMetalStorageConfig $storage)
  {
    $this->storage = $storage;
  }
  /**
   * @return BareMetalStorageConfig
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
   * @param BareMetalClusterUpgradePolicy
   */
  public function setUpgradePolicy(BareMetalClusterUpgradePolicy $upgradePolicy)
  {
    $this->upgradePolicy = $upgradePolicy;
  }
  /**
   * @return BareMetalClusterUpgradePolicy
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BareMetalCluster::class, 'Google_Service_GKEOnPrem_BareMetalCluster');
