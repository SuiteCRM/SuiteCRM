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

namespace Google\Service\SQLAdmin;

class BackupReencryptionConfig extends \Google\Model
{
  /**
   * @var int
   */
  public $backupLimit;
  /**
   * @var string
   */
  public $backupType;

  /**
   * @param int
   */
  public function setBackupLimit($backupLimit)
  {
    $this->backupLimit = $backupLimit;
  }
  /**
   * @return int
   */
  public function getBackupLimit()
  {
    return $this->backupLimit;
  }
  /**
   * @param string
   */
  public function setBackupType($backupType)
  {
    $this->backupType = $backupType;
  }
  /**
   * @return string
   */
  public function getBackupType()
  {
    return $this->backupType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BackupReencryptionConfig::class, 'Google_Service_SQLAdmin_BackupReencryptionConfig');
