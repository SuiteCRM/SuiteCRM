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

namespace Google\Service\Verifiedaccess;

class DeviceSignals extends \Google\Collection
{
  protected $collection_key = 'systemDnsServers';
  /**
   * @var bool
   */
  public $allowScreenLock;
  /**
   * @var string
   */
  public $browserVersion;
  /**
   * @var bool
   */
  public $builtInDnsClientEnabled;
  /**
   * @var bool
   */
  public $chromeRemoteDesktopAppBlocked;
  protected $crowdStrikeAgentType = CrowdStrikeAgent::class;
  protected $crowdStrikeAgentDataType = '';
  /**
   * @var string[]
   */
  public $deviceAffiliationIds;
  /**
   * @var string
   */
  public $deviceEnrollmentDomain;
  /**
   * @var string
   */
  public $deviceManufacturer;
  /**
   * @var string
   */
  public $deviceModel;
  /**
   * @var string
   */
  public $diskEncryption;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $hostname;
  /**
   * @var string[]
   */
  public $imei;
  /**
   * @var string[]
   */
  public $macAddresses;
  /**
   * @var string[]
   */
  public $meid;
  /**
   * @var string
   */
  public $operatingSystem;
  /**
   * @var string
   */
  public $osFirewall;
  /**
   * @var string
   */
  public $osVersion;
  /**
   * @var string
   */
  public $passwordProtectionWarningTrigger;
  /**
   * @var string[]
   */
  public $profileAffiliationIds;
  /**
   * @var string
   */
  public $profileEnrollmentDomain;
  /**
   * @var string
   */
  public $realtimeUrlCheckMode;
  /**
   * @var string
   */
  public $safeBrowsingProtectionLevel;
  /**
   * @var string
   */
  public $screenLockSecured;
  /**
   * @var string
   */
  public $secureBootMode;
  /**
   * @var string
   */
  public $serialNumber;
  /**
   * @var bool
   */
  public $siteIsolationEnabled;
  /**
   * @var string[]
   */
  public $systemDnsServers;
  /**
   * @var bool
   */
  public $thirdPartyBlockingEnabled;
  /**
   * @var string
   */
  public $trigger;
  /**
   * @var string
   */
  public $windowsMachineDomain;
  /**
   * @var string
   */
  public $windowsUserDomain;

  /**
   * @param bool
   */
  public function setAllowScreenLock($allowScreenLock)
  {
    $this->allowScreenLock = $allowScreenLock;
  }
  /**
   * @return bool
   */
  public function getAllowScreenLock()
  {
    return $this->allowScreenLock;
  }
  /**
   * @param string
   */
  public function setBrowserVersion($browserVersion)
  {
    $this->browserVersion = $browserVersion;
  }
  /**
   * @return string
   */
  public function getBrowserVersion()
  {
    return $this->browserVersion;
  }
  /**
   * @param bool
   */
  public function setBuiltInDnsClientEnabled($builtInDnsClientEnabled)
  {
    $this->builtInDnsClientEnabled = $builtInDnsClientEnabled;
  }
  /**
   * @return bool
   */
  public function getBuiltInDnsClientEnabled()
  {
    return $this->builtInDnsClientEnabled;
  }
  /**
   * @param bool
   */
  public function setChromeRemoteDesktopAppBlocked($chromeRemoteDesktopAppBlocked)
  {
    $this->chromeRemoteDesktopAppBlocked = $chromeRemoteDesktopAppBlocked;
  }
  /**
   * @return bool
   */
  public function getChromeRemoteDesktopAppBlocked()
  {
    return $this->chromeRemoteDesktopAppBlocked;
  }
  /**
   * @param CrowdStrikeAgent
   */
  public function setCrowdStrikeAgent(CrowdStrikeAgent $crowdStrikeAgent)
  {
    $this->crowdStrikeAgent = $crowdStrikeAgent;
  }
  /**
   * @return CrowdStrikeAgent
   */
  public function getCrowdStrikeAgent()
  {
    return $this->crowdStrikeAgent;
  }
  /**
   * @param string[]
   */
  public function setDeviceAffiliationIds($deviceAffiliationIds)
  {
    $this->deviceAffiliationIds = $deviceAffiliationIds;
  }
  /**
   * @return string[]
   */
  public function getDeviceAffiliationIds()
  {
    return $this->deviceAffiliationIds;
  }
  /**
   * @param string
   */
  public function setDeviceEnrollmentDomain($deviceEnrollmentDomain)
  {
    $this->deviceEnrollmentDomain = $deviceEnrollmentDomain;
  }
  /**
   * @return string
   */
  public function getDeviceEnrollmentDomain()
  {
    return $this->deviceEnrollmentDomain;
  }
  /**
   * @param string
   */
  public function setDeviceManufacturer($deviceManufacturer)
  {
    $this->deviceManufacturer = $deviceManufacturer;
  }
  /**
   * @return string
   */
  public function getDeviceManufacturer()
  {
    return $this->deviceManufacturer;
  }
  /**
   * @param string
   */
  public function setDeviceModel($deviceModel)
  {
    $this->deviceModel = $deviceModel;
  }
  /**
   * @return string
   */
  public function getDeviceModel()
  {
    return $this->deviceModel;
  }
  /**
   * @param string
   */
  public function setDiskEncryption($diskEncryption)
  {
    $this->diskEncryption = $diskEncryption;
  }
  /**
   * @return string
   */
  public function getDiskEncryption()
  {
    return $this->diskEncryption;
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
  public function setHostname($hostname)
  {
    $this->hostname = $hostname;
  }
  /**
   * @return string
   */
  public function getHostname()
  {
    return $this->hostname;
  }
  /**
   * @param string[]
   */
  public function setImei($imei)
  {
    $this->imei = $imei;
  }
  /**
   * @return string[]
   */
  public function getImei()
  {
    return $this->imei;
  }
  /**
   * @param string[]
   */
  public function setMacAddresses($macAddresses)
  {
    $this->macAddresses = $macAddresses;
  }
  /**
   * @return string[]
   */
  public function getMacAddresses()
  {
    return $this->macAddresses;
  }
  /**
   * @param string[]
   */
  public function setMeid($meid)
  {
    $this->meid = $meid;
  }
  /**
   * @return string[]
   */
  public function getMeid()
  {
    return $this->meid;
  }
  /**
   * @param string
   */
  public function setOperatingSystem($operatingSystem)
  {
    $this->operatingSystem = $operatingSystem;
  }
  /**
   * @return string
   */
  public function getOperatingSystem()
  {
    return $this->operatingSystem;
  }
  /**
   * @param string
   */
  public function setOsFirewall($osFirewall)
  {
    $this->osFirewall = $osFirewall;
  }
  /**
   * @return string
   */
  public function getOsFirewall()
  {
    return $this->osFirewall;
  }
  /**
   * @param string
   */
  public function setOsVersion($osVersion)
  {
    $this->osVersion = $osVersion;
  }
  /**
   * @return string
   */
  public function getOsVersion()
  {
    return $this->osVersion;
  }
  /**
   * @param string
   */
  public function setPasswordProtectionWarningTrigger($passwordProtectionWarningTrigger)
  {
    $this->passwordProtectionWarningTrigger = $passwordProtectionWarningTrigger;
  }
  /**
   * @return string
   */
  public function getPasswordProtectionWarningTrigger()
  {
    return $this->passwordProtectionWarningTrigger;
  }
  /**
   * @param string[]
   */
  public function setProfileAffiliationIds($profileAffiliationIds)
  {
    $this->profileAffiliationIds = $profileAffiliationIds;
  }
  /**
   * @return string[]
   */
  public function getProfileAffiliationIds()
  {
    return $this->profileAffiliationIds;
  }
  /**
   * @param string
   */
  public function setProfileEnrollmentDomain($profileEnrollmentDomain)
  {
    $this->profileEnrollmentDomain = $profileEnrollmentDomain;
  }
  /**
   * @return string
   */
  public function getProfileEnrollmentDomain()
  {
    return $this->profileEnrollmentDomain;
  }
  /**
   * @param string
   */
  public function setRealtimeUrlCheckMode($realtimeUrlCheckMode)
  {
    $this->realtimeUrlCheckMode = $realtimeUrlCheckMode;
  }
  /**
   * @return string
   */
  public function getRealtimeUrlCheckMode()
  {
    return $this->realtimeUrlCheckMode;
  }
  /**
   * @param string
   */
  public function setSafeBrowsingProtectionLevel($safeBrowsingProtectionLevel)
  {
    $this->safeBrowsingProtectionLevel = $safeBrowsingProtectionLevel;
  }
  /**
   * @return string
   */
  public function getSafeBrowsingProtectionLevel()
  {
    return $this->safeBrowsingProtectionLevel;
  }
  /**
   * @param string
   */
  public function setScreenLockSecured($screenLockSecured)
  {
    $this->screenLockSecured = $screenLockSecured;
  }
  /**
   * @return string
   */
  public function getScreenLockSecured()
  {
    return $this->screenLockSecured;
  }
  /**
   * @param string
   */
  public function setSecureBootMode($secureBootMode)
  {
    $this->secureBootMode = $secureBootMode;
  }
  /**
   * @return string
   */
  public function getSecureBootMode()
  {
    return $this->secureBootMode;
  }
  /**
   * @param string
   */
  public function setSerialNumber($serialNumber)
  {
    $this->serialNumber = $serialNumber;
  }
  /**
   * @return string
   */
  public function getSerialNumber()
  {
    return $this->serialNumber;
  }
  /**
   * @param bool
   */
  public function setSiteIsolationEnabled($siteIsolationEnabled)
  {
    $this->siteIsolationEnabled = $siteIsolationEnabled;
  }
  /**
   * @return bool
   */
  public function getSiteIsolationEnabled()
  {
    return $this->siteIsolationEnabled;
  }
  /**
   * @param string[]
   */
  public function setSystemDnsServers($systemDnsServers)
  {
    $this->systemDnsServers = $systemDnsServers;
  }
  /**
   * @return string[]
   */
  public function getSystemDnsServers()
  {
    return $this->systemDnsServers;
  }
  /**
   * @param bool
   */
  public function setThirdPartyBlockingEnabled($thirdPartyBlockingEnabled)
  {
    $this->thirdPartyBlockingEnabled = $thirdPartyBlockingEnabled;
  }
  /**
   * @return bool
   */
  public function getThirdPartyBlockingEnabled()
  {
    return $this->thirdPartyBlockingEnabled;
  }
  /**
   * @param string
   */
  public function setTrigger($trigger)
  {
    $this->trigger = $trigger;
  }
  /**
   * @return string
   */
  public function getTrigger()
  {
    return $this->trigger;
  }
  /**
   * @param string
   */
  public function setWindowsMachineDomain($windowsMachineDomain)
  {
    $this->windowsMachineDomain = $windowsMachineDomain;
  }
  /**
   * @return string
   */
  public function getWindowsMachineDomain()
  {
    return $this->windowsMachineDomain;
  }
  /**
   * @param string
   */
  public function setWindowsUserDomain($windowsUserDomain)
  {
    $this->windowsUserDomain = $windowsUserDomain;
  }
  /**
   * @return string
   */
  public function getWindowsUserDomain()
  {
    return $this->windowsUserDomain;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DeviceSignals::class, 'Google_Service_Verifiedaccess_DeviceSignals');
