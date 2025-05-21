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

class BareMetalAdminCluster extends \Google\Model
{
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
  protected $clusterOperationsType = BareMetalAdminClusterOperationsConfig::class;
  protected $clusterOperationsDataType = '';
  protected $controlPlaneType = BareMetalAdminControlPlaneConfig::class;
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
  protected $loadBalancerType = BareMetalAdminLoadBalancerConfig::class;
  protected $loadBalancerDataType = '';
  /**
   * @var string
   */
  public $localName;
  protected $maintenanceConfigType = BareMetalAdminMaintenanceConfig::class;
  protected $maintenanceConfigDataType = '';
  protected $maintenanceStatusType = BareMetalAdminMaintenanceStatus::class;
  protected $maintenanceStatusDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $networkConfigType = BareMetalAdminNetworkConfig::class;
  protected $networkConfigDataType = '';
  protected $nodeAccessConfigType = BareMetalAdminNodeAccessConfig::class;
  protected $nodeAccessConfigDataType = '';
  protected $nodeConfigType = BareMetalAdminWorkloadNodeConfig::class;
  protected $nodeConfigDataType = '';
  protected $osEnvironmentConfigType = BareMetalAdminOsEnvironmentConfig::class;
  protected $osEnvironmentConfigDataType = '';
  protected $proxyType = BareMetalAdminProxyConfig::class;
  protected $proxyDataType = '';
  /**
   * @var bool
   */
  public $reconciling;
  protected $securityConfigType = BareMetalAdminSecurityConfig::class;
  protected $securityConfigDataType = '';
  /**
   * @var string
   */
  public $state;
  protected $statusType = ResourceStatus::class;
  protected $statusDataType = '';
  protected $storageType = BareMetalAdminStorageConfig::class;
  protected $storageDataType = '';
  /**
   * @var string
   */
  public $uid;
  /**
   * @var string
   */
  public $updateTime;
  protected $validationCheckType = ValidationCheck::class;
  protected $validationCheckDataType = '';

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
   * @param BareMetalAdminClusterOperationsConfig
   */
  public function setClusterOperations(BareMetalAdminClusterOperationsConfig $clusterOperations)
  {
    $this->clusterOperations = $clusterOperations;
  }
  /**
   * @return BareMetalAdminClusterOperationsConfig
   */
  public function getClusterOperations()
  {
    return $this->clusterOperations;
  }
  /**
   * @param BareMetalAdminControlPlaneConfig
   */
  public function setControlPlane(BareMetalAdminControlPlaneConfig $controlPlane)
  {
    $this->controlPlane = $controlPlane;
  }
  /**
   * @return BareMetalAdminControlPlaneConfig
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
   * @param BareMetalAdminLoadBalancerConfig
   */
  public function setLoadBalancer(BareMetalAdminLoadBalancerConfig $loadBalancer)
  {
    $this->loadBalancer = $loadBalancer;
  }
  /**
   * @return BareMetalAdminLoadBalancerConfig
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
   * @param BareMetalAdminMaintenanceConfig
   */
  public function setMaintenanceConfig(BareMetalAdminMaintenanceConfig $maintenanceConfig)
  {
    $this->maintenanceConfig = $maintenanceConfig;
  }
  /**
   * @return BareMetalAdminMaintenanceConfig
   */
  public function getMaintenanceConfig()
  {
    return $this->maintenanceConfig;
  }
  /**
   * @param BareMetalAdminMaintenanceStatus
   */
  public function setMaintenanceStatus(BareMetalAdminMaintenanceStatus $maintenanceStatus)
  {
    $this->maintenanceStatus = $maintenanceStatus;
  }
  /**
   * @return BareMetalAdminMaintenanceStatus
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
   * @param BareMetalAdminNetworkConfig
   */
  public function setNetworkConfig(BareMetalAdminNetworkConfig $networkConfig)
  {
    $this->networkConfig = $networkConfig;
  }
  /**
   * @return BareMetalAdminNetworkConfig
   */
  public function getNetworkConfig()
  {
    return $this->networkConfig;
  }
  /**
   * @param BareMetalAdminNodeAccessConfig
   */
  public function setNodeAccessConfig(BareMetalAdminNodeAccessConfig $nodeAccessConfig)
  {
    $this->nodeAccessConfig = $nodeAccessConfig;
  }
  /**
   * @return BareMetalAdminNodeAccessConfig
   */
  public function getNodeAccessConfig()
  {
    return $this->nodeAccessConfig;
  }
  /**
   * @param BareMetalAdminWorkloadNodeConfig
   */
  public function setNodeConfig(BareMetalAdminWorkloadNodeConfig $nodeConfig)
  {
    $this->nodeConfig = $nodeConfig;
  }
  /**
   * @return BareMetalAdminWorkloadNodeConfig
   */
  public function getNodeConfig()
  {
    return $this->nodeConfig;
  }
  /**
   * @param BareMetalAdminOsEnvironmentConfig
   */
  public function setOsEnvironmentConfig(BareMetalAdminOsEnvironmentConfig $osEnvironmentConfig)
  {
    $this->osEnvironmentConfig = $osEnvironmentConfig;
  }
  /**
   * @return BareMetalAdminOsEnvironmentConfig
   */
  public function getOsEnvironmentConfig()
  {
    return $this->osEnvironmentConfig;
  }
  /**
   * @param BareMetalAdminProxyConfig
   */
  public function setProxy(BareMetalAdminProxyConfig $proxy)
  {
    $this->proxy = $proxy;
  }
  /**
   * @return BareMetalAdminProxyConfig
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
   * @param BareMetalAdminSecurityConfig
   */
  public function setSecurityConfig(BareMetalAdminSecurityConfig $securityConfig)
  {
    $this->securityConfig = $securityConfig;
  }
  /**
   * @return BareMetalAdminSecurityConfig
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
   * @param BareMetalAdminStorageConfig
   */
  public function setStorage(BareMetalAdminStorageConfig $storage)
  {
    $this->storage = $storage;
  }
  /**
   * @return BareMetalAdminStorageConfig
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
class_alias(BareMetalAdminCluster::class, 'Google_Service_GKEOnPrem_BareMetalAdminCluster');
