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

namespace Google\Service\GKEHub;

class FeatureState extends \Google\Model
{
  protected $appdevexperienceType = AppDevExperienceState::class;
  protected $appdevexperienceDataType = '';
  protected $clusterupgradeType = ClusterUpgradeState::class;
  protected $clusterupgradeDataType = '';
  protected $configmanagementType = ConfigManagementState::class;
  protected $configmanagementDataType = '';
  protected $identityserviceType = IdentityServiceState::class;
  protected $identityserviceDataType = '';
  protected $meteringType = MeteringState::class;
  protected $meteringDataType = '';
  protected $policycontrollerType = PolicyControllerState::class;
  protected $policycontrollerDataType = '';
  protected $servicemeshType = ServiceMeshState::class;
  protected $servicemeshDataType = '';
  protected $stateType = State::class;
  protected $stateDataType = '';

  /**
   * @param AppDevExperienceState
   */
  public function setAppdevexperience(AppDevExperienceState $appdevexperience)
  {
    $this->appdevexperience = $appdevexperience;
  }
  /**
   * @return AppDevExperienceState
   */
  public function getAppdevexperience()
  {
    return $this->appdevexperience;
  }
  /**
   * @param ClusterUpgradeState
   */
  public function setClusterupgrade(ClusterUpgradeState $clusterupgrade)
  {
    $this->clusterupgrade = $clusterupgrade;
  }
  /**
   * @return ClusterUpgradeState
   */
  public function getClusterupgrade()
  {
    return $this->clusterupgrade;
  }
  /**
   * @param ConfigManagementState
   */
  public function setConfigmanagement(ConfigManagementState $configmanagement)
  {
    $this->configmanagement = $configmanagement;
  }
  /**
   * @return ConfigManagementState
   */
  public function getConfigmanagement()
  {
    return $this->configmanagement;
  }
  /**
   * @param IdentityServiceState
   */
  public function setIdentityservice(IdentityServiceState $identityservice)
  {
    $this->identityservice = $identityservice;
  }
  /**
   * @return IdentityServiceState
   */
  public function getIdentityservice()
  {
    return $this->identityservice;
  }
  /**
   * @param MeteringState
   */
  public function setMetering(MeteringState $metering)
  {
    $this->metering = $metering;
  }
  /**
   * @return MeteringState
   */
  public function getMetering()
  {
    return $this->metering;
  }
  /**
   * @param PolicyControllerState
   */
  public function setPolicycontroller(PolicyControllerState $policycontroller)
  {
    $this->policycontroller = $policycontroller;
  }
  /**
   * @return PolicyControllerState
   */
  public function getPolicycontroller()
  {
    return $this->policycontroller;
  }
  /**
   * @param ServiceMeshState
   */
  public function setServicemesh(ServiceMeshState $servicemesh)
  {
    $this->servicemesh = $servicemesh;
  }
  /**
   * @return ServiceMeshState
   */
  public function getServicemesh()
  {
    return $this->servicemesh;
  }
  /**
   * @param State
   */
  public function setState(State $state)
  {
    $this->state = $state;
  }
  /**
   * @return State
   */
  public function getState()
  {
    return $this->state;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(FeatureState::class, 'Google_Service_GKEHub_FeatureState');
