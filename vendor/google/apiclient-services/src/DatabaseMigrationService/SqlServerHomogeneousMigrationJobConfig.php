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

namespace Google\Service\DatabaseMigrationService;

class SqlServerHomogeneousMigrationJobConfig extends \Google\Collection
{
  protected $collection_key = 'databaseBackups';
  /**
   * @var string
   */
  public $backupFilePattern;
  protected $databaseBackupsType = SqlServerDatabaseBackup::class;
  protected $databaseBackupsDataType = 'array';
  /**
   * @var bool
   */
  public $promoteWhenReady;
  /**
   * @var bool
   */
  public $useDiffBackup;

  /**
   * @param string
   */
  public function setBackupFilePattern($backupFilePattern)
  {
    $this->backupFilePattern = $backupFilePattern;
  }
  /**
   * @return string
   */
  public function getBackupFilePattern()
  {
    return $this->backupFilePattern;
  }
  /**
   * @param SqlServerDatabaseBackup[]
   */
  public function setDatabaseBackups($databaseBackups)
  {
    $this->databaseBackups = $databaseBackups;
  }
  /**
   * @return SqlServerDatabaseBackup[]
   */
  public function getDatabaseBackups()
  {
    return $this->databaseBackups;
  }
  /**
   * @param bool
   */
  public function setPromoteWhenReady($promoteWhenReady)
  {
    $this->promoteWhenReady = $promoteWhenReady;
  }
  /**
   * @return bool
   */
  public function getPromoteWhenReady()
  {
    return $this->promoteWhenReady;
  }
  /**
   * @param bool
   */
  public function setUseDiffBackup($useDiffBackup)
  {
    $this->useDiffBackup = $useDiffBackup;
  }
  /**
   * @return bool
   */
  public function getUseDiffBackup()
  {
    return $this->useDiffBackup;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SqlServerHomogeneousMigrationJobConfig::class, 'Google_Service_DatabaseMigrationService_SqlServerHomogeneousMigrationJobConfig');
