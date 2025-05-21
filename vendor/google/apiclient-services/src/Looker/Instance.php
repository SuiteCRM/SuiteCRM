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

namespace Google\Service\Looker;

class Instance extends \Google\Model
{
  protected $adminSettingsType = AdminSettings::class;
  protected $adminSettingsDataType = '';
  /**
   * @var string
   */
  public $consumerNetwork;
  /**
   * @var string
   */
  public $createTime;
  protected $customDomainType = CustomDomain::class;
  protected $customDomainDataType = '';
  protected $denyMaintenancePeriodType = DenyMaintenancePeriod::class;
  protected $denyMaintenancePeriodDataType = '';
  /**
   * @var string
   */
  public $egressPublicIp;
  protected $encryptionConfigType = EncryptionConfig::class;
  protected $encryptionConfigDataType = '';
  /**
   * @var bool
   */
  public $fipsEnabled;
  /**
   * @var bool
   */
  public $geminiEnabled;
  /**
   * @var string
   */
  public $ingressPrivateIp;
  /**
   * @var string
   */
  public $ingressPublicIp;
  protected $lastDenyMaintenancePeriodType = DenyMaintenancePeriod::class;
  protected $lastDenyMaintenancePeriodDataType = '';
  /**
   * @var string
   */
  public $linkedLspProjectNumber;
  /**
   * @var string
   */
  public $lookerUri;
  /**
   * @var string
   */
  public $lookerVersion;
  protected $maintenanceScheduleType = MaintenanceSchedule::class;
  protected $maintenanceScheduleDataType = '';
  protected $maintenanceWindowType = MaintenanceWindow::class;
  protected $maintenanceWindowDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $oauthConfigType = OAuthConfig::class;
  protected $oauthConfigDataType = '';
  /**
   * @var string
   */
  public $platformEdition;
  /**
   * @var bool
   */
  public $privateIpEnabled;
  protected $pscConfigType = PscConfig::class;
  protected $pscConfigDataType = '';
  /**
   * @var bool
   */
  public $pscEnabled;
  /**
   * @var bool
   */
  public $publicIpEnabled;
  /**
   * @var string
   */
  public $reservedRange;
  /**
   * @var bool
   */
  public $satisfiesPzi;
  /**
   * @var bool
   */
  public $satisfiesPzs;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $updateTime;
  protected $userMetadataType = UserMetadata::class;
  protected $userMetadataDataType = '';

  /**
   * @param AdminSettings
   */
  public function setAdminSettings(AdminSettings $adminSettings)
  {
    $this->adminSettings = $adminSettings;
  }
  /**
   * @return AdminSettings
   */
  public function getAdminSettings()
  {
    return $this->adminSettings;
  }
  /**
   * @param string
   */
  public function setConsumerNetwork($consumerNetwork)
  {
    $this->consumerNetwork = $consumerNetwork;
  }
  /**
   * @return string
   */
  public function getConsumerNetwork()
  {
    return $this->consumerNetwork;
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
   * @param CustomDomain
   */
  public function setCustomDomain(CustomDomain $customDomain)
  {
    $this->customDomain = $customDomain;
  }
  /**
   * @return CustomDomain
   */
  public function getCustomDomain()
  {
    return $this->customDomain;
  }
  /**
   * @param DenyMaintenancePeriod
   */
  public function setDenyMaintenancePeriod(DenyMaintenancePeriod $denyMaintenancePeriod)
  {
    $this->denyMaintenancePeriod = $denyMaintenancePeriod;
  }
  /**
   * @return DenyMaintenancePeriod
   */
  public function getDenyMaintenancePeriod()
  {
    return $this->denyMaintenancePeriod;
  }
  /**
   * @param string
   */
  public function setEgressPublicIp($egressPublicIp)
  {
    $this->egressPublicIp = $egressPublicIp;
  }
  /**
   * @return string
   */
  public function getEgressPublicIp()
  {
    return $this->egressPublicIp;
  }
  /**
   * @param EncryptionConfig
   */
  public function setEncryptionConfig(EncryptionConfig $encryptionConfig)
  {
    $this->encryptionConfig = $encryptionConfig;
  }
  /**
   * @return EncryptionConfig
   */
  public function getEncryptionConfig()
  {
    return $this->encryptionConfig;
  }
  /**
   * @param bool
   */
  public function setFipsEnabled($fipsEnabled)
  {
    $this->fipsEnabled = $fipsEnabled;
  }
  /**
   * @return bool
   */
  public function getFipsEnabled()
  {
    return $this->fipsEnabled;
  }
  /**
   * @param bool
   */
  public function setGeminiEnabled($geminiEnabled)
  {
    $this->geminiEnabled = $geminiEnabled;
  }
  /**
   * @return bool
   */
  public function getGeminiEnabled()
  {
    return $this->geminiEnabled;
  }
  /**
   * @param string
   */
  public function setIngressPrivateIp($ingressPrivateIp)
  {
    $this->ingressPrivateIp = $ingressPrivateIp;
  }
  /**
   * @return string
   */
  public function getIngressPrivateIp()
  {
    return $this->ingressPrivateIp;
  }
  /**
   * @param string
   */
  public function setIngressPublicIp($ingressPublicIp)
  {
    $this->ingressPublicIp = $ingressPublicIp;
  }
  /**
   * @return string
   */
  public function getIngressPublicIp()
  {
    return $this->ingressPublicIp;
  }
  /**
   * @param DenyMaintenancePeriod
   */
  public function setLastDenyMaintenancePeriod(DenyMaintenancePeriod $lastDenyMaintenancePeriod)
  {
    $this->lastDenyMaintenancePeriod = $lastDenyMaintenancePeriod;
  }
  /**
   * @return DenyMaintenancePeriod
   */
  public function getLastDenyMaintenancePeriod()
  {
    return $this->lastDenyMaintenancePeriod;
  }
  /**
   * @param string
   */
  public function setLinkedLspProjectNumber($linkedLspProjectNumber)
  {
    $this->linkedLspProjectNumber = $linkedLspProjectNumber;
  }
  /**
   * @return string
   */
  public function getLinkedLspProjectNumber()
  {
    return $this->linkedLspProjectNumber;
  }
  /**
   * @param string
   */
  public function setLookerUri($lookerUri)
  {
    $this->lookerUri = $lookerUri;
  }
  /**
   * @return string
   */
  public function getLookerUri()
  {
    return $this->lookerUri;
  }
  /**
   * @param string
   */
  public function setLookerVersion($lookerVersion)
  {
    $this->lookerVersion = $lookerVersion;
  }
  /**
   * @return string
   */
  public function getLookerVersion()
  {
    return $this->lookerVersion;
  }
  /**
   * @param MaintenanceSchedule
   */
  public function setMaintenanceSchedule(MaintenanceSchedule $maintenanceSchedule)
  {
    $this->maintenanceSchedule = $maintenanceSchedule;
  }
  /**
   * @return MaintenanceSchedule
   */
  public function getMaintenanceSchedule()
  {
    return $this->maintenanceSchedule;
  }
  /**
   * @param MaintenanceWindow
   */
  public function setMaintenanceWindow(MaintenanceWindow $maintenanceWindow)
  {
    $this->maintenanceWindow = $maintenanceWindow;
  }
  /**
   * @return MaintenanceWindow
   */
  public function getMaintenanceWindow()
  {
    return $this->maintenanceWindow;
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
   * @param OAuthConfig
   */
  public function setOauthConfig(OAuthConfig $oauthConfig)
  {
    $this->oauthConfig = $oauthConfig;
  }
  /**
   * @return OAuthConfig
   */
  public function getOauthConfig()
  {
    return $this->oauthConfig;
  }
  /**
   * @param string
   */
  public function setPlatformEdition($platformEdition)
  {
    $this->platformEdition = $platformEdition;
  }
  /**
   * @return string
   */
  public function getPlatformEdition()
  {
    return $this->platformEdition;
  }
  /**
   * @param bool
   */
  public function setPrivateIpEnabled($privateIpEnabled)
  {
    $this->privateIpEnabled = $privateIpEnabled;
  }
  /**
   * @return bool
   */
  public function getPrivateIpEnabled()
  {
    return $this->privateIpEnabled;
  }
  /**
   * @param PscConfig
   */
  public function setPscConfig(PscConfig $pscConfig)
  {
    $this->pscConfig = $pscConfig;
  }
  /**
   * @return PscConfig
   */
  public function getPscConfig()
  {
    return $this->pscConfig;
  }
  /**
   * @param bool
   */
  public function setPscEnabled($pscEnabled)
  {
    $this->pscEnabled = $pscEnabled;
  }
  /**
   * @return bool
   */
  public function getPscEnabled()
  {
    return $this->pscEnabled;
  }
  /**
   * @param bool
   */
  public function setPublicIpEnabled($publicIpEnabled)
  {
    $this->publicIpEnabled = $publicIpEnabled;
  }
  /**
   * @return bool
   */
  public function getPublicIpEnabled()
  {
    return $this->publicIpEnabled;
  }
  /**
   * @param string
   */
  public function setReservedRange($reservedRange)
  {
    $this->reservedRange = $reservedRange;
  }
  /**
   * @return string
   */
  public function getReservedRange()
  {
    return $this->reservedRange;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzi($satisfiesPzi)
  {
    $this->satisfiesPzi = $satisfiesPzi;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzi()
  {
    return $this->satisfiesPzi;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzs($satisfiesPzs)
  {
    $this->satisfiesPzs = $satisfiesPzs;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzs()
  {
    return $this->satisfiesPzs;
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
   * @param UserMetadata
   */
  public function setUserMetadata(UserMetadata $userMetadata)
  {
    $this->userMetadata = $userMetadata;
  }
  /**
   * @return UserMetadata
   */
  public function getUserMetadata()
  {
    return $this->userMetadata;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Instance::class, 'Google_Service_Looker_Instance');
