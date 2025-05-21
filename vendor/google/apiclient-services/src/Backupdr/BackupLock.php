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

class BackupLock extends \Google\Model
{
  protected $backupApplianceLockInfoType = BackupApplianceLockInfo::class;
  protected $backupApplianceLockInfoDataType = '';
  /**
   * @var string
   */
  public $lockUntilTime;
  protected $serviceLockInfoType = ServiceLockInfo::class;
  protected $serviceLockInfoDataType = '';

  /**
   * @param BackupApplianceLockInfo
   */
  public function setBackupApplianceLockInfo(BackupApplianceLockInfo $backupApplianceLockInfo)
  {
    $this->backupApplianceLockInfo = $backupApplianceLockInfo;
  }
  /**
   * @return BackupApplianceLockInfo
   */
  public function getBackupApplianceLockInfo()
  {
    return $this->backupApplianceLockInfo;
  }
  /**
   * @param string
   */
  public function setLockUntilTime($lockUntilTime)
  {
    $this->lockUntilTime = $lockUntilTime;
  }
  /**
   * @return string
   */
  public function getLockUntilTime()
  {
    return $this->lockUntilTime;
  }
  /**
   * @param ServiceLockInfo
   */
  public function setServiceLockInfo(ServiceLockInfo $serviceLockInfo)
  {
    $this->serviceLockInfo = $serviceLockInfo;
  }
  /**
   * @return ServiceLockInfo
   */
  public function getServiceLockInfo()
  {
    return $this->serviceLockInfo;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BackupLock::class, 'Google_Service_Backupdr_BackupLock');
