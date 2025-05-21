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

class BackupApplianceBackupConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $applicationName;
  /**
   * @var string
   */
  public $backupApplianceId;
  /**
   * @var string
   */
  public $backupApplianceName;
  /**
   * @var string
   */
  public $hostName;
  /**
   * @var string
   */
  public $slaId;
  /**
   * @var string
   */
  public $slpName;
  /**
   * @var string
   */
  public $sltName;

  /**
   * @param string
   */
  public function setApplicationName($applicationName)
  {
    $this->applicationName = $applicationName;
  }
  /**
   * @return string
   */
  public function getApplicationName()
  {
    return $this->applicationName;
  }
  /**
   * @param string
   */
  public function setBackupApplianceId($backupApplianceId)
  {
    $this->backupApplianceId = $backupApplianceId;
  }
  /**
   * @return string
   */
  public function getBackupApplianceId()
  {
    return $this->backupApplianceId;
  }
  /**
   * @param string
   */
  public function setBackupApplianceName($backupApplianceName)
  {
    $this->backupApplianceName = $backupApplianceName;
  }
  /**
   * @return string
   */
  public function getBackupApplianceName()
  {
    return $this->backupApplianceName;
  }
  /**
   * @param string
   */
  public function setHostName($hostName)
  {
    $this->hostName = $hostName;
  }
  /**
   * @return string
   */
  public function getHostName()
  {
    return $this->hostName;
  }
  /**
   * @param string
   */
  public function setSlaId($slaId)
  {
    $this->slaId = $slaId;
  }
  /**
   * @return string
   */
  public function getSlaId()
  {
    return $this->slaId;
  }
  /**
   * @param string
   */
  public function setSlpName($slpName)
  {
    $this->slpName = $slpName;
  }
  /**
   * @return string
   */
  public function getSlpName()
  {
    return $this->slpName;
  }
  /**
   * @param string
   */
  public function setSltName($sltName)
  {
    $this->sltName = $sltName;
  }
  /**
   * @return string
   */
  public function getSltName()
  {
    return $this->sltName;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BackupApplianceBackupConfig::class, 'Google_Service_Backupdr_BackupApplianceBackupConfig');
