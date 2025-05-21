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

namespace Google\Service\DLP;

class GooglePrivacyDlpV2DiscoveryConfig extends \Google\Collection
{
  protected $collection_key = 'targets';
  protected $actionsType = GooglePrivacyDlpV2DataProfileAction::class;
  protected $actionsDataType = 'array';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $displayName;
  protected $errorsType = GooglePrivacyDlpV2Error::class;
  protected $errorsDataType = 'array';
  /**
   * @var string[]
   */
  public $inspectTemplates;
  /**
   * @var string
   */
  public $lastRunTime;
  /**
   * @var string
   */
  public $name;
  protected $orgConfigType = GooglePrivacyDlpV2OrgConfig::class;
  protected $orgConfigDataType = '';
  protected $otherCloudStartingLocationType = GooglePrivacyDlpV2OtherCloudDiscoveryStartingLocation::class;
  protected $otherCloudStartingLocationDataType = '';
  protected $processingLocationType = GooglePrivacyDlpV2ProcessingLocation::class;
  protected $processingLocationDataType = '';
  /**
   * @var string
   */
  public $status;
  protected $targetsType = GooglePrivacyDlpV2DiscoveryTarget::class;
  protected $targetsDataType = 'array';
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param GooglePrivacyDlpV2DataProfileAction[]
   */
  public function setActions($actions)
  {
    $this->actions = $actions;
  }
  /**
   * @return GooglePrivacyDlpV2DataProfileAction[]
   */
  public function getActions()
  {
    return $this->actions;
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
   * @param GooglePrivacyDlpV2Error[]
   */
  public function setErrors($errors)
  {
    $this->errors = $errors;
  }
  /**
   * @return GooglePrivacyDlpV2Error[]
   */
  public function getErrors()
  {
    return $this->errors;
  }
  /**
   * @param string[]
   */
  public function setInspectTemplates($inspectTemplates)
  {
    $this->inspectTemplates = $inspectTemplates;
  }
  /**
   * @return string[]
   */
  public function getInspectTemplates()
  {
    return $this->inspectTemplates;
  }
  /**
   * @param string
   */
  public function setLastRunTime($lastRunTime)
  {
    $this->lastRunTime = $lastRunTime;
  }
  /**
   * @return string
   */
  public function getLastRunTime()
  {
    return $this->lastRunTime;
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
   * @param GooglePrivacyDlpV2OrgConfig
   */
  public function setOrgConfig(GooglePrivacyDlpV2OrgConfig $orgConfig)
  {
    $this->orgConfig = $orgConfig;
  }
  /**
   * @return GooglePrivacyDlpV2OrgConfig
   */
  public function getOrgConfig()
  {
    return $this->orgConfig;
  }
  /**
   * @param GooglePrivacyDlpV2OtherCloudDiscoveryStartingLocation
   */
  public function setOtherCloudStartingLocation(GooglePrivacyDlpV2OtherCloudDiscoveryStartingLocation $otherCloudStartingLocation)
  {
    $this->otherCloudStartingLocation = $otherCloudStartingLocation;
  }
  /**
   * @return GooglePrivacyDlpV2OtherCloudDiscoveryStartingLocation
   */
  public function getOtherCloudStartingLocation()
  {
    return $this->otherCloudStartingLocation;
  }
  /**
   * @param GooglePrivacyDlpV2ProcessingLocation
   */
  public function setProcessingLocation(GooglePrivacyDlpV2ProcessingLocation $processingLocation)
  {
    $this->processingLocation = $processingLocation;
  }
  /**
   * @return GooglePrivacyDlpV2ProcessingLocation
   */
  public function getProcessingLocation()
  {
    return $this->processingLocation;
  }
  /**
   * @param string
   */
  public function setStatus($status)
  {
    $this->status = $status;
  }
  /**
   * @return string
   */
  public function getStatus()
  {
    return $this->status;
  }
  /**
   * @param GooglePrivacyDlpV2DiscoveryTarget[]
   */
  public function setTargets($targets)
  {
    $this->targets = $targets;
  }
  /**
   * @return GooglePrivacyDlpV2DiscoveryTarget[]
   */
  public function getTargets()
  {
    return $this->targets;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GooglePrivacyDlpV2DiscoveryConfig::class, 'Google_Service_DLP_GooglePrivacyDlpV2DiscoveryConfig');
