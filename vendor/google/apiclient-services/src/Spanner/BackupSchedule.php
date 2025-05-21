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

namespace Google\Service\Spanner;

class BackupSchedule extends \Google\Model
{
  protected $encryptionConfigType = CreateBackupEncryptionConfig::class;
  protected $encryptionConfigDataType = '';
  protected $fullBackupSpecType = FullBackupSpec::class;
  protected $fullBackupSpecDataType = '';
  protected $incrementalBackupSpecType = IncrementalBackupSpec::class;
  protected $incrementalBackupSpecDataType = '';
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $retentionDuration;
  protected $specType = BackupScheduleSpec::class;
  protected $specDataType = '';
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param CreateBackupEncryptionConfig
   */
  public function setEncryptionConfig(CreateBackupEncryptionConfig $encryptionConfig)
  {
    $this->encryptionConfig = $encryptionConfig;
  }
  /**
   * @return CreateBackupEncryptionConfig
   */
  public function getEncryptionConfig()
  {
    return $this->encryptionConfig;
  }
  /**
   * @param FullBackupSpec
   */
  public function setFullBackupSpec(FullBackupSpec $fullBackupSpec)
  {
    $this->fullBackupSpec = $fullBackupSpec;
  }
  /**
   * @return FullBackupSpec
   */
  public function getFullBackupSpec()
  {
    return $this->fullBackupSpec;
  }
  /**
   * @param IncrementalBackupSpec
   */
  public function setIncrementalBackupSpec(IncrementalBackupSpec $incrementalBackupSpec)
  {
    $this->incrementalBackupSpec = $incrementalBackupSpec;
  }
  /**
   * @return IncrementalBackupSpec
   */
  public function getIncrementalBackupSpec()
  {
    return $this->incrementalBackupSpec;
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
  public function setRetentionDuration($retentionDuration)
  {
    $this->retentionDuration = $retentionDuration;
  }
  /**
   * @return string
   */
  public function getRetentionDuration()
  {
    return $this->retentionDuration;
  }
  /**
   * @param BackupScheduleSpec
   */
  public function setSpec(BackupScheduleSpec $spec)
  {
    $this->spec = $spec;
  }
  /**
   * @return BackupScheduleSpec
   */
  public function getSpec()
  {
    return $this->spec;
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
class_alias(BackupSchedule::class, 'Google_Service_Spanner_BackupSchedule');
