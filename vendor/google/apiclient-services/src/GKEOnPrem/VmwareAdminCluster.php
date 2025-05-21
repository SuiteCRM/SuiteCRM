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

class VmwareAdminCluster extends \Google\Model
{
  protected $addonNodeType = VmwareAdminAddonNodeConfig::class;
  protected $addonNodeDataType = '';
  /**
   * @var string[]
   */
  public $annotations;
  protected $antiAffinityGroupsType = VmwareAAGConfig::class;
  protected $antiAffinityGroupsDataType = '';
  protected $authorizationType = VmwareAdminAuthorizationConfig::class;
  protected $authorizationDataType = '';
  protected $autoRepairConfigType = VmwareAutoRepairConfig::class;
  protected $autoRepairConfigDataType = '';
  /**
   * @var string
   */
  public $bootstrapClusterMembership;
  protected $controlPlaneNodeType = VmwareAdminControlPlaneNodeConfig::class;
  protected $controlPlaneNodeDataType = '';
  /**
   * @var string
   */
  public $createTime;
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
  /**
   * @var string
   */
  public $imageType;
  protected $loadBalancerType = VmwareAdminLoadBalancerConfig::class;
  protected $loadBalancerDataType = '';
  /**
   * @var string
   */
  public $localName;
  /**
   * @var string
   */
  public $name;
  protected $networkConfigType = VmwareAdminNetworkConfig::class;
  protected $networkConfigDataType = '';
  /**
   * @var string
   */
  public $onPremVersion;
  protected $platformConfigType = VmwarePlatformConfig::class;
  protected $platformConfigDataType = '';
  protected $preparedSecretsType = VmwareAdminPreparedSecretsConfig::class;
  protected $preparedSecretsDataType = '';
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
  protected $validationCheckType = ValidationCheck::class;
  protected $validationCheckDataType = '';
  protected $vcenterType = VmwareAdminVCenterConfig::class;
  protected $vcenterDataType = '';

  /**
   * @param VmwareAdminAddonNodeConfig
   */
  public function setAddonNode(VmwareAdminAddonNodeConfig $addonNode)
  {
    $this->addonNode = $addonNode;
  }
  /**
   * @return VmwareAdminAddonNodeConfig
   */
  public function getAddonNode()
  {
    return $this->addonNode;
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
   * @param VmwareAdminAuthorizationConfig
   */
  public function setAuthorization(VmwareAdminAuthorizationConfig $authorization)
  {
    $this->authorization = $authorization;
  }
  /**
   * @return VmwareAdminAuthorizationConfig
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
   * @param string
   */
  public function setBootstrapClusterMembership($bootstrapClusterMembership)
  {
    $this->bootstrapClusterMembership = $bootstrapClusterMembership;
  }
  /**
   * @return string
   */
  public function getBootstrapClusterMembership()
  {
    return $this->bootstrapClusterMembership;
  }
  /**
   * @param VmwareAdminControlPlaneNodeConfig
   */
  public function setControlPlaneNode(VmwareAdminControlPlaneNodeConfig $controlPlaneNode)
  {
    $this->controlPlaneNode = $controlPlaneNode;
  }
  /**
   * @return VmwareAdminControlPlaneNodeConfig
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
   * @param string
   */
  public function setImageType($imageType)
  {
    $this->imageType = $imageType;
  }
  /**
   * @return string
   */
  public function getImageType()
  {
    return $this->imageType;
  }
  /**
   * @param VmwareAdminLoadBalancerConfig
   */
  public function setLoadBalancer(VmwareAdminLoadBalancerConfig $loadBalancer)
  {
    $this->loadBalancer = $loadBalancer;
  }
  /**
   * @return VmwareAdminLoadBalancerConfig
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
   * @param VmwareAdminNetworkConfig
   */
  public function setNetworkConfig(VmwareAdminNetworkConfig $networkConfig)
  {
    $this->networkConfig = $networkConfig;
  }
  /**
   * @return VmwareAdminNetworkConfig
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
   * @param VmwarePlatformConfig
   */
  public function setPlatformConfig(VmwarePlatformConfig $platformConfig)
  {
    $this->platformConfig = $platformConfig;
  }
  /**
   * @return VmwarePlatformConfig
   */
  public function getPlatformConfig()
  {
    return $this->platformConfig;
  }
  /**
   * @param VmwareAdminPreparedSecretsConfig
   */
  public function setPreparedSecrets(VmwareAdminPreparedSecretsConfig $preparedSecrets)
  {
    $this->preparedSecrets = $preparedSecrets;
  }
  /**
   * @return VmwareAdminPreparedSecretsConfig
   */
  public function getPreparedSecrets()
  {
    return $this->preparedSecrets;
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
   * @param VmwareAdminVCenterConfig
   */
  public function setVcenter(VmwareAdminVCenterConfig $vcenter)
  {
    $this->vcenter = $vcenter;
  }
  /**
   * @return VmwareAdminVCenterConfig
   */
  public function getVcenter()
  {
    return $this->vcenter;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(VmwareAdminCluster::class, 'Google_Service_GKEOnPrem_VmwareAdminCluster');
