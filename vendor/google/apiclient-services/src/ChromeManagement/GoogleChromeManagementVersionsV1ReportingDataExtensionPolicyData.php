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

class GoogleChromeManagementVersionsV1ReportingDataExtensionPolicyData extends \Google\Collection
{
  protected $collection_key = 'policyData';
  /**
   * @var string
   */
  public $extensionId;
  /**
   * @var string
   */
  public $extensionName;
  protected $policyDataType = GoogleChromeManagementVersionsV1ReportingDataPolicyData::class;
  protected $policyDataDataType = 'array';

  /**
   * @param string
   */
  public function setExtensionId($extensionId)
  {
    $this->extensionId = $extensionId;
  }
  /**
   * @return string
   */
  public function getExtensionId()
  {
    return $this->extensionId;
  }
  /**
   * @param string
   */
  public function setExtensionName($extensionName)
  {
    $this->extensionName = $extensionName;
  }
  /**
   * @return string
   */
  public function getExtensionName()
  {
    return $this->extensionName;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleChromeManagementVersionsV1ReportingDataExtensionPolicyData::class, 'Google_Service_ChromeManagement_GoogleChromeManagementVersionsV1ReportingDataExtensionPolicyData');
