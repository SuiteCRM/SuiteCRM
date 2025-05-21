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

namespace Google\Service\ChromeManagement;

class GoogleChromeManagementVersionsV1ChromeBrowserProfile extends \Google\Model
{
  /**
   * @var string
   */
  public $affiliationState;
  /**
   * @var string
   */
  public $annotatedLocation;
  /**
   * @var string
   */
  public $annotatedUser;
  protected $attestationCredentialType = GoogleChromeManagementVersionsV1AttestationCredential::class;
  protected $attestationCredentialDataType = '';
  /**
   * @var string
   */
  public $browserChannel;
  /**
   * @var string
   */
  public $browserVersion;
  protected $deviceInfoType = GoogleChromeManagementVersionsV1DeviceInfo::class;
  protected $deviceInfoDataType = '';
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $etag;
  /**
   * @var string
   */
  public $extensionCount;
  /**
   * @var string
   */
  public $firstEnrollmentTime;
  /**
   * @var string
   */
  public $identityProvider;
  /**
   * @var string
   */
  public $lastActivityTime;
  /**
   * @var string
   */
  public $lastPolicyFetchTime;
  /**
   * @var string
   */
  public $lastPolicySyncTime;
  /**
   * @var string
   */
  public $lastStatusReportTime;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $osPlatformType;
  /**
   * @var string
   */
  public $osPlatformVersion;
  /**
   * @var string
   */
  public $osVersion;
  /**
   * @var string
   */
  public $policyCount;
  /**
   * @var string
   */
  public $profileId;
  /**
   * @var string
   */
  public $profilePermanentId;
  protected $reportingDataType = GoogleChromeManagementVersionsV1ReportingData::class;
  protected $reportingDataDataType = '';
  /**
   * @var string
   */
  public $userEmail;
  /**
   * @var string
   */
  public $userId;

  /**
   * @param string
   */
  public function setAffiliationState($affiliationState)
  {
    $this->affiliationState = $affiliationState;
  }
  /**
   * @return string
   */
  public function getAffiliationState()
  {
    return $this->affiliationState;
  }
  /**
   * @param string
   */
  public function setAnnotatedLocation($annotatedLocation)
  {
    $this->annotatedLocation = $annotatedLocation;
  }
  /**
   * @return string
   */
  public function getAnnotatedLocation()
  {
    return $this->annotatedLocation;
  }
  /**
   * @param string
   */
  public function setAnnotatedUser($annotatedUser)
  {
    $this->annotatedUser = $annotatedUser;
  }
  /**
   * @return string
   */
  public function getAnnotatedUser()
  {
    return $this->annotatedUser;
  }
  /**
   * @param GoogleChromeManagementVersionsV1AttestationCredential
   */
  public function setAttestationCredential(GoogleChromeManagementVersionsV1AttestationCredential $attestationCredential)
  {
    $this->attestationCredential = $attestationCredential;
  }
  /**
   * @return GoogleChromeManagementVersionsV1AttestationCredential
   */
  public function getAttestationCredential()
  {
    return $this->attestationCredential;
  }
  /**
   * @param string
   */
  public function setBrowserChannel($browserChannel)
  {
    $this->browserChannel = $browserChannel;
  }
  /**
   * @return string
   */
  public function getBrowserChannel()
  {
    return $this->browserChannel;
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
   * @param GoogleChromeManagementVersionsV1DeviceInfo
   */
  public function setDeviceInfo(GoogleChromeManagementVersionsV1DeviceInfo $deviceInfo)
  {
    $this->deviceInfo = $deviceInfo;
  }
  /**
   * @return GoogleChromeManagementVersionsV1DeviceInfo
   */
  public function getDeviceInfo()
  {
    return $this->deviceInfo;
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
   * @param string
   */
  public function setExtensionCount($extensionCount)
  {
    $this->extensionCount = $extensionCount;
  }
  /**
   * @return string
   */
  public function getExtensionCount()
  {
    return $this->extensionCount;
  }
  /**
   * @param string
   */
  public function setFirstEnrollmentTime($firstEnrollmentTime)
  {
    $this->firstEnrollmentTime = $firstEnrollmentTime;
  }
  /**
   * @return string
   */
  public function getFirstEnrollmentTime()
  {
    return $this->firstEnrollmentTime;
  }
  /**
   * @param string
   */
  public function setIdentityProvider($identityProvider)
  {
    $this->identityProvider = $identityProvider;
  }
  /**
   * @return string
   */
  public function getIdentityProvider()
  {
    return $this->identityProvider;
  }
  /**
   * @param string
   */
  public function setLastActivityTime($lastActivityTime)
  {
    $this->lastActivityTime = $lastActivityTime;
  }
  /**
   * @return string
   */
  public function getLastActivityTime()
  {
    return $this->lastActivityTime;
  }
  /**
   * @param string
   */
  public function setLastPolicyFetchTime($lastPolicyFetchTime)
  {
    $this->lastPolicyFetchTime = $lastPolicyFetchTime;
  }
  /**
   * @return string
   */
  public function getLastPolicyFetchTime()
  {
    return $this->lastPolicyFetchTime;
  }
  /**
   * @param string
   */
  public function setLastPolicySyncTime($lastPolicySyncTime)
  {
    $this->lastPolicySyncTime = $lastPolicySyncTime;
  }
  /**
   * @return string
   */
  public function getLastPolicySyncTime()
  {
    return $this->lastPolicySyncTime;
  }
  /**
   * @param string
   */
  public function setLastStatusReportTime($lastStatusReportTime)
  {
    $this->lastStatusReportTime = $lastStatusReportTime;
  }
  /**
   * @return string
   */
  public function getLastStatusReportTime()
  {
    return $this->lastStatusReportTime;
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
   * @param string
   */
  public function setOsPlatformType($osPlatformType)
  {
    $this->osPlatformType = $osPlatformType;
  }
  /**
   * @return string
   */
  public function getOsPlatformType()
  {
    return $this->osPlatformType;
  }
  /**
   * @param string
   */
  public function setOsPlatformVersion($osPlatformVersion)
  {
    $this->osPlatformVersion = $osPlatformVersion;
  }
  /**
   * @return string
   */
  public function getOsPlatformVersion()
  {
    return $this->osPlatformVersion;
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
  public function setPolicyCount($policyCount)
  {
    $this->policyCount = $policyCount;
  }
  /**
   * @return string
   */
  public function getPolicyCount()
  {
    return $this->policyCount;
  }
  /**
   * @param string
   */
  public function setProfileId($profileId)
  {
    $this->profileId = $profileId;
  }
  /**
   * @return string
   */
  public function getProfileId()
  {
    return $this->profileId;
  }
  /**
   * @param string
   */
  public function setProfilePermanentId($profilePermanentId)
  {
    $this->profilePermanentId = $profilePermanentId;
  }
  /**
   * @return string
   */
  public function getProfilePermanentId()
  {
    return $this->profilePermanentId;
  }
  /**
   * @param GoogleChromeManagementVersionsV1ReportingData
   */
  public function setReportingData(GoogleChromeManagementVersionsV1ReportingData $reportingData)
  {
    $this->reportingData = $reportingData;
  }
  /**
   * @return GoogleChromeManagementVersionsV1ReportingData
   */
  public function getReportingData()
  {
    return $this->reportingData;
  }
  /**
   * @param string
   */
  public function setUserEmail($userEmail)
  {
    $this->userEmail = $userEmail;
  }
  /**
   * @return string
   */
  public function getUserEmail()
  {
    return $this->userEmail;
  }
  /**
   * @param string
   */
  public function setUserId($userId)
  {
    $this->userId = $userId;
  }
  /**
   * @return string
   */
  public function getUserId()
  {
    return $this->userId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleChromeManagementVersionsV1ChromeBrowserProfile::class, 'Google_Service_ChromeManagement_GoogleChromeManagementVersionsV1ChromeBrowserProfile');
