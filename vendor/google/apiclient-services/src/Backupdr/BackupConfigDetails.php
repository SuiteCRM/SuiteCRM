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

class BackupConfigDetails extends \Google\Collection
{
  protected $collection_key = 'backupLocations';
  /**
   * @var string
   */
  public $applicableResource;
  /**
   * @var string
   */
  public $backupConfigSource;
  /**
   * @var string
   */
  public $backupConfigSourceDisplayName;
  protected $backupDrPlanConfigType = BackupDrPlanConfig::class;
  protected $backupDrPlanConfigDataType = '';
  protected $backupDrTemplateConfigType = BackupDrTemplateConfig::class;
  protected $backupDrTemplateConfigDataType = '';
  protected $backupLocationsType = BackupLocation::class;
  protected $backupLocationsDataType = 'array';
  /**
   * @var string
   */
  public $backupVault;
  /**
   * @var string
   */
  public $latestSuccessfulBackupTime;
  protected $pitrSettingsType = PitrSettings::class;
  protected $pitrSettingsDataType = '';
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $type;

  /**
   * @param string
   */
  public function setApplicableResource($applicableResource)
  {
    $this->applicableResource = $applicableResource;
  }
  /**
   * @return string
   */
  public function getApplicableResource()
  {
    return $this->applicableResource;
  }
  /**
   * @param string
   */
  public function setBackupConfigSource($backupConfigSource)
  {
    $this->backupConfigSource = $backupConfigSource;
  }
  /**
   * @return string
   */
  public function getBackupConfigSource()
  {
    return $this->backupConfigSource;
  }
  /**
   * @param string
   */
  public function setBackupConfigSourceDisplayName($backupConfigSourceDisplayName)
  {
    $this->backupConfigSourceDisplayName = $backupConfigSourceDisplayName;
  }
  /**
   * @return string
   */
  public function getBackupConfigSourceDisplayName()
  {
    return $this->backupConfigSourceDisplayName;
  }
  /**
   * @param BackupDrPlanConfig
   */
  public function setBackupDrPlanConfig(BackupDrPlanConfig $backupDrPlanConfig)
  {
    $this->backupDrPlanConfig = $backupDrPlanConfig;
  }
  /**
   * @return BackupDrPlanConfig
   */
  public function getBackupDrPlanConfig()
  {
    return $this->backupDrPlanConfig;
  }
  /**
   * @param BackupDrTemplateConfig
   */
  public function setBackupDrTemplateConfig(BackupDrTemplateConfig $backupDrTemplateConfig)
  {
    $this->backupDrTemplateConfig = $backupDrTemplateConfig;
  }
  /**
   * @return BackupDrTemplateConfig
   */
  public function getBackupDrTemplateConfig()
  {
    return $this->backupDrTemplateConfig;
  }
  /**
   * @param BackupLocation[]
   */
  public function setBackupLocations($backupLocations)
  {
    $this->backupLocations = $backupLocations;
  }
  /**
   * @return BackupLocation[]
   */
  public function getBackupLocations()
  {
    return $this->backupLocations;
  }
  /**
   * @param string
   */
  public function setBackupVault($backupVault)
  {
    $this->backupVault = $backupVault;
  }
  /**
   * @return string
   */
  public function getBackupVault()
  {
    return $this->backupVault;
  }
  /**
   * @param string
   */
  public function setLatestSuccessfulBackupTime($latestSuccessfulBackupTime)
  {
    $this->latestSuccessfulBackupTime = $latestSuccessfulBackupTime;
  }
  /**
   * @return string
   */
  public function getLatestSuccessfulBackupTime()
  {
    return $this->latestSuccessfulBackupTime;
  }
  /**
   * @param PitrSettings
   */
  public function setPitrSettings(PitrSettings $pitrSettings)
  {
    $this->pitrSettings = $pitrSettings;
  }
  /**
   * @return PitrSettings
   */
  public function getPitrSettings()
  {
    return $this->pitrSettings;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BackupConfigDetails::class, 'Google_Service_Backupdr_BackupConfigDetails');
