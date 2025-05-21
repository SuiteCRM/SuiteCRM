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

class BackupApplianceLockInfo extends \Google\Model
{
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
  public $backupImage;
  /**
   * @var string
   */
  public $jobName;
  /**
   * @var string
   */
  public $lockReason;
  /**
   * @var string
   */
  public $slaId;

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
  public function setBackupImage($backupImage)
  {
    $this->backupImage = $backupImage;
  }
  /**
   * @return string
   */
  public function getBackupImage()
  {
    return $this->backupImage;
  }
  /**
   * @param string
   */
  public function setJobName($jobName)
  {
    $this->jobName = $jobName;
  }
  /**
   * @return string
   */
  public function getJobName()
  {
    return $this->jobName;
  }
  /**
   * @param string
   */
  public function setLockReason($lockReason)
  {
    $this->lockReason = $lockReason;
  }
  /**
   * @return string
   */
  public function getLockReason()
  {
    return $this->lockReason;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BackupApplianceLockInfo::class, 'Google_Service_Backupdr_BackupApplianceLockInfo');
