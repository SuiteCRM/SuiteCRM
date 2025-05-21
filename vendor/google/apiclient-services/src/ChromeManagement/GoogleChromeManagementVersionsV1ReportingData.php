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

class GoogleChromeManagementVersionsV1ReportingData extends \Google\Collection
{
  protected $collection_key = 'policyData';
  /**
   * @var string
   */
  public $browserExecutablePath;
  protected $extensionDataType = GoogleChromeManagementVersionsV1ReportingDataExtensionData::class;
  protected $extensionDataDataType = 'array';
  protected $extensionPolicyDataType = GoogleChromeManagementVersionsV1ReportingDataExtensionPolicyData::class;
  protected $extensionPolicyDataDataType = 'array';
  /**
   * @var string
   */
  public $installedBrowserVersion;
  protected $policyDataType = GoogleChromeManagementVersionsV1ReportingDataPolicyData::class;
  protected $policyDataDataType = 'array';
  /**
   * @var string
   */
  public $profilePath;

  /**
   * @param string
   */
  public function setBrowserExecutablePath($browserExecutablePath)
  {
    $this->browserExecutablePath = $browserExecutablePath;
  }
  /**
   * @return string
   */
  public function getBrowserExecutablePath()
  {
    return $this->browserExecutablePath;
  }
  /**
   * @param GoogleChromeManagementVersionsV1ReportingDataExtensionData[]
   */
  public function setExtensionData($extensionData)
  {
    $this->extensionData = $extensionData;
  }
  /**
   * @return GoogleChromeManagementVersionsV1ReportingDataExtensionData[]
   */
  public function getExtensionData()
  {
    return $this->extensionData;
  }
  /**
   * @param GoogleChromeManagementVersionsV1ReportingDataExtensionPolicyData[]
   */
  public function setExtensionPolicyData($extensionPolicyData)
  {
    $this->extensionPolicyData = $extensionPolicyData;
  }
  /**
   * @return GoogleChromeManagementVersionsV1ReportingDataExtensionPolicyData[]
   */
  public function getExtensionPolicyData()
  {
    return $this->extensionPolicyData;
  }
  /**
   * @param string
   */
  public function setInstalledBrowserVersion($installedBrowserVersion)
  {
    $this->installedBrowserVersion = $installedBrowserVersion;
  }
  /**
   * @return string
   */
  public function getInstalledBrowserVersion()
  {
    return $this->installedBrowserVersion;
  }
  /**
   * @param GoogleChromeManagementVersionsV1ReportingDataPolicyData[]
   */
  public function setPolicyData($policyData)
  {
    $this->policyData = $policyData;
  }
  /**
   * @return GoogleChromeManagementVersionsV1ReportingDataPolicyData[]
   */
  public function getPolicyData()
  {
    return $this->policyData;
  }
  /**
   * @param string
   */
  public function setProfilePath($profilePath)
  {
    $this->profilePath = $profilePath;
  }
  /**
   * @return string
   */
  public function getProfilePath()
  {
    return $this->profilePath;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleChromeManagementVersionsV1ReportingData::class, 'Google_Service_ChromeManagement_GoogleChromeManagementVersionsV1ReportingData');
