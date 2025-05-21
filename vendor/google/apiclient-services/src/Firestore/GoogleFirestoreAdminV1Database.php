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

namespace Google\Service\Firestore;

class GoogleFirestoreAdminV1Database extends \Google\Model
{
  /**
   * @var string
   */
  public $appEngineIntegrationMode;
  protected $cmekConfigType = GoogleFirestoreAdminV1CmekConfig::class;
  protected $cmekConfigDataType = '';
  /**
   * @var string
   */
  public $concurrencyMode;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $deleteProtectionState;
  /**
   * @var string
   */
  public $deleteTime;
  /**
   * @var string
   */
  public $earliestVersionTime;
  /**
   * @var string
   */
  public $etag;
  /**
   * @var string
   */
  public $keyPrefix;
  /**
   * @var string
   */
  public $locationId;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $pointInTimeRecoveryEnablement;
  /**
   * @var string
   */
  public $previousId;
  protected $sourceInfoType = GoogleFirestoreAdminV1SourceInfo::class;
  protected $sourceInfoDataType = '';
  /**
   * @var string
   */
  public $type;
  /**
   * @var string
   */
  public $uid;
  /**
   * @var string
   */
  public $updateTime;
  /**
   * @var string
   */
  public $versionRetentionPeriod;

  /**
   * @param string
   */
  public function setAppEngineIntegrationMode($appEngineIntegrationMode)
  {
    $this->appEngineIntegrationMode = $appEngineIntegrationMode;
  }
  /**
   * @return string
   */
  public function getAppEngineIntegrationMode()
  {
    return $this->appEngineIntegrationMode;
  }
  /**
   * @param GoogleFirestoreAdminV1CmekConfig
   */
  public function setCmekConfig(GoogleFirestoreAdminV1CmekConfig $cmekConfig)
  {
    $this->cmekConfig = $cmekConfig;
  }
  /**
   * @return GoogleFirestoreAdminV1CmekConfig
   */
  public function getCmekConfig()
  {
    return $this->cmekConfig;
  }
  /**
   * @param string
   */
  public function setConcurrencyMode($concurrencyMode)
  {
    $this->concurrencyMode = $concurrencyMode;
  }
  /**
   * @return string
   */
  public function getConcurrencyMode()
  {
    return $this->concurrencyMode;
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
  public function setDeleteProtectionState($deleteProtectionState)
  {
    $this->deleteProtectionState = $deleteProtectionState;
  }
  /**
   * @return string
   */
  public function getDeleteProtectionState()
  {
    return $this->deleteProtectionState;
  }
  /**
   * @param string
   */
  public function setDeleteTime($deleteTime)
  {
    $this->deleteTime = $deleteTime;
  }
  /**
   * @return string
   */
  public function getDeleteTime()
  {
    return $this->deleteTime;
  }
  /**
   * @param string
   */
  public function setEarliestVersionTime($earliestVersionTime)
  {
    $this->earliestVersionTime = $earliestVersionTime;
  }
  /**
   * @return string
   */
  public function getEarliestVersionTime()
  {
    return $this->earliestVersionTime;
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
  public function setKeyPrefix($keyPrefix)
  {
    $this->keyPrefix = $keyPrefix;
  }
  /**
   * @return string
   */
  public function getKeyPrefix()
  {
    return $this->keyPrefix;
  }
  /**
   * @param string
   */
  public function setLocationId($locationId)
  {
    $this->locationId = $locationId;
  }
  /**
   * @return string
   */
  public function getLocationId()
  {
    return $this->locationId;
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
  public function setPointInTimeRecoveryEnablement($pointInTimeRecoveryEnablement)
  {
    $this->pointInTimeRecoveryEnablement = $pointInTimeRecoveryEnablement;
  }
  /**
   * @return string
   */
  public function getPointInTimeRecoveryEnablement()
  {
    return $this->pointInTimeRecoveryEnablement;
  }
  /**
   * @param string
   */
  public function setPreviousId($previousId)
  {
    $this->previousId = $previousId;
  }
  /**
   * @return string
   */
  public function getPreviousId()
  {
    return $this->previousId;
  }
  /**
   * @param GoogleFirestoreAdminV1SourceInfo
   */
  public function setSourceInfo(GoogleFirestoreAdminV1SourceInfo $sourceInfo)
  {
    $this->sourceInfo = $sourceInfo;
  }
  /**
   * @return GoogleFirestoreAdminV1SourceInfo
   */
  public function getSourceInfo()
  {
    return $this->sourceInfo;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
  /**
   * @param string
   */
  public function setUid($uid)
  {
    $this->uid = $uid;
  }
  /**
   * @return string
   */
  public function getUid()
  {
    return $this->uid;
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
   * @param string
   */
  public function setVersionRetentionPeriod($versionRetentionPeriod)
  {
    $this->versionRetentionPeriod = $versionRetentionPeriod;
  }
  /**
   * @return string
   */
  public function getVersionRetentionPeriod()
  {
    return $this->versionRetentionPeriod;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleFirestoreAdminV1Database::class, 'Google_Service_Firestore_GoogleFirestoreAdminV1Database');
