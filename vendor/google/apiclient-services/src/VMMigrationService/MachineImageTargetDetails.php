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

namespace Google\Service\VMMigrationService;

class MachineImageTargetDetails extends \Google\Collection
{
  protected $collection_key = 'tags';
  /**
   * @var string[]
   */
  public $additionalLicenses;
  /**
   * @var string
   */
  public $description;
  protected $encryptionType = Encryption::class;
  protected $encryptionDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $machineImageName;
  protected $machineImageParametersOverridesType = MachineImageParametersOverrides::class;
  protected $machineImageParametersOverridesDataType = '';
  protected $networkInterfacesType = NetworkInterface::class;
  protected $networkInterfacesDataType = 'array';
  protected $osAdaptationParametersType = ImageImportOsAdaptationParameters::class;
  protected $osAdaptationParametersDataType = '';
  protected $serviceAccountType = ServiceAccount::class;
  protected $serviceAccountDataType = '';
  protected $shieldedInstanceConfigType = ShieldedInstanceConfig::class;
  protected $shieldedInstanceConfigDataType = '';
  /**
   * @var bool
   */
  public $singleRegionStorage;
  protected $skipOsAdaptationType = SkipOsAdaptation::class;
  protected $skipOsAdaptationDataType = '';
  /**
   * @var string[]
   */
  public $tags;
  /**
   * @var string
   */
  public $targetProject;

  /**
   * @param string[]
   */
  public function setAdditionalLicenses($additionalLicenses)
  {
    $this->additionalLicenses = $additionalLicenses;
  }
  /**
   * @return string[]
   */
  public function getAdditionalLicenses()
  {
    return $this->additionalLicenses;
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
   * @param Encryption
   */
  public function setEncryption(Encryption $encryption)
  {
    $this->encryption = $encryption;
  }
  /**
   * @return Encryption
   */
  public function getEncryption()
  {
    return $this->encryption;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
  }
  /**
   * @param string
   */
  public function setMachineImageName($machineImageName)
  {
    $this->machineImageName = $machineImageName;
  }
  /**
   * @return string
   */
  public function getMachineImageName()
  {
    return $this->machineImageName;
  }
  /**
   * @param MachineImageParametersOverrides
   */
  public function setMachineImageParametersOverrides(MachineImageParametersOverrides $machineImageParametersOverrides)
  {
    $this->machineImageParametersOverrides = $machineImageParametersOverrides;
  }
  /**
   * @return MachineImageParametersOverrides
   */
  public function getMachineImageParametersOverrides()
  {
    return $this->machineImageParametersOverrides;
  }
  /**
   * @param NetworkInterface[]
   */
  public function setNetworkInterfaces($networkInterfaces)
  {
    $this->networkInterfaces = $networkInterfaces;
  }
  /**
   * @return NetworkInterface[]
   */
  public function getNetworkInterfaces()
  {
    return $this->networkInterfaces;
  }
  /**
   * @param ImageImportOsAdaptationParameters
   */
  public function setOsAdaptationParameters(ImageImportOsAdaptationParameters $osAdaptationParameters)
  {
    $this->osAdaptationParameters = $osAdaptationParameters;
  }
  /**
   * @return ImageImportOsAdaptationParameters
   */
  public function getOsAdaptationParameters()
  {
    return $this->osAdaptationParameters;
  }
  /**
   * @param ServiceAccount
   */
  public function setServiceAccount(ServiceAccount $serviceAccount)
  {
    $this->serviceAccount = $serviceAccount;
  }
  /**
   * @return ServiceAccount
   */
  public function getServiceAccount()
  {
    return $this->serviceAccount;
  }
  /**
   * @param ShieldedInstanceConfig
   */
  public function setShieldedInstanceConfig(ShieldedInstanceConfig $shieldedInstanceConfig)
  {
    $this->shieldedInstanceConfig = $shieldedInstanceConfig;
  }
  /**
   * @return ShieldedInstanceConfig
   */
  public function getShieldedInstanceConfig()
  {
    return $this->shieldedInstanceConfig;
  }
  /**
   * @param bool
   */
  public function setSingleRegionStorage($singleRegionStorage)
  {
    $this->singleRegionStorage = $singleRegionStorage;
  }
  /**
   * @return bool
   */
  public function getSingleRegionStorage()
  {
    return $this->singleRegionStorage;
  }
  /**
   * @param SkipOsAdaptation
   */
  public function setSkipOsAdaptation(SkipOsAdaptation $skipOsAdaptation)
  {
    $this->skipOsAdaptation = $skipOsAdaptation;
  }
  /**
   * @return SkipOsAdaptation
   */
  public function getSkipOsAdaptation()
  {
    return $this->skipOsAdaptation;
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
   * @param string
   */
  public function setTargetProject($targetProject)
  {
    $this->targetProject = $targetProject;
  }
  /**
   * @return string
   */
  public function getTargetProject()
  {
    return $this->targetProject;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(MachineImageTargetDetails::class, 'Google_Service_VMMigrationService_MachineImageTargetDetails');
