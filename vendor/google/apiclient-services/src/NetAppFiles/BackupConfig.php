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

namespace Google\Service\NetAppFiles;

class BackupConfig extends \Google\Collection
{
  protected $collection_key = 'backupPolicies';
  /**
   * @var string
   */
  public $backupChainBytes;
  /**
   * @var string[]
   */
  public $backupPolicies;
  /**
   * @var string
   */
  public $backupVault;
  /**
   * @var bool
   */
  public $scheduledBackupEnabled;

  /**
   * @param string
   */
  public function setBackupChainBytes($backupChainBytes)
  {
    $this->backupChainBytes = $backupChainBytes;
  }
  /**
   * @return string
   */
  public function getBackupChainBytes()
  {
    return $this->backupChainBytes;
  }
  /**
   * @param string[]
   */
  public function setBackupPolicies($backupPolicies)
  {
    $this->backupPolicies = $backupPolicies;
  }
  /**
   * @return string[]
   */
  public function getBackupPolicies()
  {
    return $this->backupPolicies;
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
   * @param bool
   */
  public function setScheduledBackupEnabled($scheduledBackupEnabled)
  {
    $this->scheduledBackupEnabled = $scheduledBackupEnabled;
  }
  /**
   * @return bool
   */
  public function getScheduledBackupEnabled()
  {
    return $this->scheduledBackupEnabled;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BackupConfig::class, 'Google_Service_NetAppFiles_BackupConfig');
