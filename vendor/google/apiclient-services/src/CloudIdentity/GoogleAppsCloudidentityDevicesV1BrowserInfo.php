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

namespace Google\Service\CloudIdentity;

class GoogleAppsCloudidentityDevicesV1BrowserInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $browserManagementState;
  /**
   * @var string
   */
  public $browserVersion;
  /**
   * @var bool
   */
  public $isBuiltInDnsClientEnabled;
  /**
   * @var bool
   */
  public $isBulkDataEntryAnalysisEnabled;
  /**
   * @var bool
   */
  public $isChromeCleanupEnabled;
  /**
   * @var bool
   */
  public $isChromeRemoteDesktopAppBlocked;
  /**
   * @var bool
   */
  public $isFileDownloadAnalysisEnabled;
  /**
   * @var bool
   */
  public $isFileUploadAnalysisEnabled;
  /**
   * @var bool
   */
  public $isRealtimeUrlCheckEnabled;
  /**
   * @var bool
   */
  public $isSecurityEventAnalysisEnabled;
  /**
   * @var bool
   */
  public $isSiteIsolationEnabled;
  /**
   * @var bool
   */
  public $isThirdPartyBlockingEnabled;
  /**
   * @var string
   */
  public $passwordProtectionWarningTrigger;
  /**
   * @var string
   */
  public $safeBrowsingProtectionLevel;

  /**
   * @param string
   */
  public function setBrowserManagementState($browserManagementState)
  {
    $this->browserManagementState = $browserManagementState;
  }
  /**
   * @return string
   */
  public function getBrowserManagementState()
  {
    return $this->browserManagementState;
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
  public function setIsBuiltInDnsClientEnabled($isBuiltInDnsClientEnabled)
  {
    $this->isBuiltInDnsClientEnabled = $isBuiltInDnsClientEnabled;
  }
  /**
   * @return bool
   */
  public function getIsBuiltInDnsClientEnabled()
  {
    return $this->isBuiltInDnsClientEnabled;
  }
  /**
   * @param bool
   */
  public function setIsBulkDataEntryAnalysisEnabled($isBulkDataEntryAnalysisEnabled)
  {
    $this->isBulkDataEntryAnalysisEnabled = $isBulkDataEntryAnalysisEnabled;
  }
  /**
   * @return bool
   */
  public function getIsBulkDataEntryAnalysisEnabled()
  {
    return $this->isBulkDataEntryAnalysisEnabled;
  }
  /**
   * @param bool
   */
  public function setIsChromeCleanupEnabled($isChromeCleanupEnabled)
  {
    $this->isChromeCleanupEnabled = $isChromeCleanupEnabled;
  }
  /**
   * @return bool
   */
  public function getIsChromeCleanupEnabled()
  {
    return $this->isChromeCleanupEnabled;
  }
  /**
   * @param bool
   */
  public function setIsChromeRemoteDesktopAppBlocked($isChromeRemoteDesktopAppBlocked)
  {
    $this->isChromeRemoteDesktopAppBlocked = $isChromeRemoteDesktopAppBlocked;
  }
  /**
   * @return bool
   */
  public function getIsChromeRemoteDesktopAppBlocked()
  {
    return $this->isChromeRemoteDesktopAppBlocked;
  }
  /**
   * @param bool
   */
  public function setIsFileDownloadAnalysisEnabled($isFileDownloadAnalysisEnabled)
  {
    $this->isFileDownloadAnalysisEnabled = $isFileDownloadAnalysisEnabled;
  }
  /**
   * @return bool
   */
  public function getIsFileDownloadAnalysisEnabled()
  {
    return $this->isFileDownloadAnalysisEnabled;
  }
  /**
   * @param bool
   */
  public function setIsFileUploadAnalysisEnabled($isFileUploadAnalysisEnabled)
  {
    $this->isFileUploadAnalysisEnabled = $isFileUploadAnalysisEnabled;
  }
  /**
   * @return bool
   */
  public function getIsFileUploadAnalysisEnabled()
  {
    return $this->isFileUploadAnalysisEnabled;
  }
  /**
   * @param bool
   */
  public function setIsRealtimeUrlCheckEnabled($isRealtimeUrlCheckEnabled)
  {
    $this->isRealtimeUrlCheckEnabled = $isRealtimeUrlCheckEnabled;
  }
  /**
   * @return bool
   */
  public function getIsRealtimeUrlCheckEnabled()
  {
    return $this->isRealtimeUrlCheckEnabled;
  }
  /**
   * @param bool
   */
  public function setIsSecurityEventAnalysisEnabled($isSecurityEventAnalysisEnabled)
  {
    $this->isSecurityEventAnalysisEnabled = $isSecurityEventAnalysisEnabled;
  }
  /**
   * @return bool
   */
  public function getIsSecurityEventAnalysisEnabled()
  {
    return $this->isSecurityEventAnalysisEnabled;
  }
  /**
   * @param bool
   */
  public function setIsSiteIsolationEnabled($isSiteIsolationEnabled)
  {
    $this->isSiteIsolationEnabled = $isSiteIsolationEnabled;
  }
  /**
   * @return bool
   */
  public function getIsSiteIsolationEnabled()
  {
    return $this->isSiteIsolationEnabled;
  }
  /**
   * @param bool
   */
  public function setIsThirdPartyBlockingEnabled($isThirdPartyBlockingEnabled)
  {
    $this->isThirdPartyBlockingEnabled = $isThirdPartyBlockingEnabled;
  }
  /**
   * @return bool
   */
  public function getIsThirdPartyBlockingEnabled()
  {
    return $this->isThirdPartyBlockingEnabled;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAppsCloudidentityDevicesV1BrowserInfo::class, 'Google_Service_CloudIdentity_GoogleAppsCloudidentityDevicesV1BrowserInfo');
