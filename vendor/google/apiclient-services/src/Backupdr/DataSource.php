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

namespace Google\Service\Backupdr;

class DataSource extends \Google\Model
{
  protected $backupConfigInfoType = BackupConfigInfo::class;
  protected $backupConfigInfoDataType = '';
  /**
   * @var string
   */
  public $backupCount;
  /**
   * @var string
   */
  public $configState;
  /**
   * @var string
   */
  public $createTime;
  protected $dataSourceBackupApplianceApplicationType = DataSourceBackupApplianceApplication::class;
  protected $dataSourceBackupApplianceApplicationDataType = '';
  protected $dataSourceGcpResourceType = DataSourceGcpResource::class;
  protected $dataSourceGcpResourceDataType = '';
  /**
   * @var string
   */
  public $etag;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $totalStoredBytes;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param BackupConfigInfo
   */
  public function setBackupConfigInfo(BackupConfigInfo $backupConfigInfo)
  {
    $this->backupConfigInfo = $backupConfigInfo;
  }
  /**
   * @return BackupConfigInfo
   */
  public function getBackupConfigInfo()
  {
    return $this->backupConfigInfo;
  }
  /**
   * @param string
   */
  public function setBackupCount($backupCount)
  {
    $this->backupCount = $backupCount;
  }
  /**
   * @return string
   */
  public function getBackupCount()
  {
    return $this->backupCount;
  }
  /**
   * @param string
   */
  public function setConfigState($configState)
  {
    $this->configState = $configState;
  }
  /**
   * @return string
   */
  public function getConfigState()
  {
    return $this->configState;
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
   * @param DataSourceBackupApplianceApplication
   */
  public function setDataSourceBackupApplianceApplication(DataSourceBackupApplianceApplication $dataSourceBackupApplianceApplication)
  {
    $this->dataSourceBackupApplianceApplication = $dataSourceBackupApplianceApplication;
  }
  /**
   * @return DataSourceBackupApplianceApplication
   */
  public function getDataSourceBackupApplianceApplication()
  {
    return $this->dataSourceBackupApplianceApplication;
  }
  /**
   * @param DataSourceGcpResource
   */
  public function setDataSourceGcpResource(DataSourceGcpResource $dataSourceGcpResource)
  {
    $this->dataSourceGcpResource = $dataSourceGcpResource;
  }
  /**
   * @return DataSourceGcpResource
   */
  public function getDataSourceGcpResource()
  {
    return $this->dataSourceGcpResource;
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
  public function setTotalStoredBytes($totalStoredBytes)
  {
    $this->totalStoredBytes = $totalStoredBytes;
  }
  /**
   * @return string
   */
  public function getTotalStoredBytes()
  {
    return $this->totalStoredBytes;
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
class_alias(DataSource::class, 'Google_Service_Backupdr_DataSource');
