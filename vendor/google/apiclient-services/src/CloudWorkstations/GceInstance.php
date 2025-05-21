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

namespace Google\Service\CloudWorkstations;

class GceInstance extends \Google\Collection
{
  protected $collection_key = 'tags';
  protected $acceleratorsType = Accelerator::class;
  protected $acceleratorsDataType = 'array';
  protected $boostConfigsType = BoostConfig::class;
  protected $boostConfigsDataType = 'array';
  /**
   * @var int
   */
  public $bootDiskSizeGb;
  protected $confidentialInstanceConfigType = GceConfidentialInstanceConfig::class;
  protected $confidentialInstanceConfigDataType = '';
  /**
   * @var bool
   */
  public $disablePublicIpAddresses;
  /**
   * @var bool
   */
  public $disableSsh;
  /**
   * @var bool
   */
  public $enableNestedVirtualization;
  /**
   * @var string
   */
  public $machineType;
  /**
   * @var int
   */
  public $poolSize;
  /**
   * @var int
   */
  public $pooledInstances;
  /**
   * @var string
   */
  public $serviceAccount;
  /**
   * @var string[]
   */
  public $serviceAccountScopes;
  protected $shieldedInstanceConfigType = GceShieldedInstanceConfig::class;
  protected $shieldedInstanceConfigDataType = '';
  /**
   * @var string[]
   */
  public $tags;
  /**
   * @var string[]
   */
  public $vmTags;

  /**
   * @param Accelerator[]
   */
  public function setAccelerators($accelerators)
  {
    $this->accelerators = $accelerators;
  }
  /**
   * @return Accelerator[]
   */
  public function getAccelerators()
  {
    return $this->accelerators;
  }
  /**
   * @param BoostConfig[]
   */
  public function setBoostConfigs($boostConfigs)
  {
    $this->boostConfigs = $boostConfigs;
  }
  /**
   * @return BoostConfig[]
   */
  public function getBoostConfigs()
  {
    return $this->boostConfigs;
  }
  /**
   * @param int
   */
  public function setBootDiskSizeGb($bootDiskSizeGb)
  {
    $this->bootDiskSizeGb = $bootDiskSizeGb;
  }
  /**
   * @return int
   */
  public function getBootDiskSizeGb()
  {
    return $this->bootDiskSizeGb;
  }
  /**
   * @param GceConfidentialInstanceConfig
   */
  public function setConfidentialInstanceConfig(GceConfidentialInstanceConfig $confidentialInstanceConfig)
  {
    $this->confidentialInstanceConfig = $confidentialInstanceConfig;
  }
  /**
   * @return GceConfidentialInstanceConfig
   */
  public function getConfidentialInstanceConfig()
  {
    return $this->confidentialInstanceConfig;
  }
  /**
   * @param bool
   */
  public function setDisablePublicIpAddresses($disablePublicIpAddresses)
  {
    $this->disablePublicIpAddresses = $disablePublicIpAddresses;
  }
  /**
   * @return bool
   */
  public function getDisablePublicIpAddresses()
  {
    return $this->disablePublicIpAddresses;
  }
  /**
   * @param bool
   */
  public function setDisableSsh($disableSsh)
  {
    $this->disableSsh = $disableSsh;
  }
  /**
   * @return bool
   */
  public function getDisableSsh()
  {
    return $this->disableSsh;
  }
  /**
   * @param bool
   */
  public function setEnableNestedVirtualization($enableNestedVirtualization)
  {
    $this->enableNestedVirtualization = $enableNestedVirtualization;
  }
  /**
   * @return bool
   */
  public function getEnableNestedVirtualization()
  {
    return $this->enableNestedVirtualization;
  }
  /**
   * @param string
   */
  public function setMachineType($machineType)
  {
    $this->machineType = $machineType;
  }
  /**
   * @return string
   */
  public function getMachineType()
  {
    return $this->machineType;
  }
  /**
   * @param int
   */
  public function setPoolSize($poolSize)
  {
    $this->poolSize = $poolSize;
  }
  /**
   * @return int
   */
  public function getPoolSize()
  {
    return $this->poolSize;
  }
  /**
   * @param int
   */
  public function setPooledInstances($pooledInstances)
  {
    $this->pooledInstances = $pooledInstances;
  }
  /**
   * @return int
   */
  public function getPooledInstances()
  {
    return $this->pooledInstances;
  }
  /**
   * @param string
   */
  public function setServiceAccount($serviceAccount)
  {
    $this->serviceAccount = $serviceAccount;
  }
  /**
   * @return string
   */
  public function getServiceAccount()
  {
    return $this->serviceAccount;
  }
  /**
   * @param string[]
   */
  public function setServiceAccountScopes($serviceAccountScopes)
  {
    $this->serviceAccountScopes = $serviceAccountScopes;
  }
  /**
   * @return string[]
   */
  public function getServiceAccountScopes()
  {
    return $this->serviceAccountScopes;
  }
  /**
   * @param GceShieldedInstanceConfig
   */
  public function setShieldedInstanceConfig(GceShieldedInstanceConfig $shieldedInstanceConfig)
  {
    $this->shieldedInstanceConfig = $shieldedInstanceConfig;
  }
  /**
   * @return GceShieldedInstanceConfig
   */
  public function getShieldedInstanceConfig()
  {
    return $this->shieldedInstanceConfig;
  }
  /**
   * @param string[]
   */
  public function setTags($tags)
  {
    $this->tags = $tags;
  }
  /**
   * @return string[]
   */
  public function getTags()
  {
    return $this->tags;
  }
  /**
   * @param string[]
   */
  public function setVmTags($vmTags)
  {
    $this->vmTags = $vmTags;
  }
  /**
   * @return string[]
   */
  public function getVmTags()
  {
    return $this->vmTags;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GceInstance::class, 'Google_Service_CloudWorkstations_GceInstance');
