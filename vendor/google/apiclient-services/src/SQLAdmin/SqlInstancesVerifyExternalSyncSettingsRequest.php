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

class SqlInstancesVerifyExternalSyncSettingsRequest extends \Google\Collection
{
  protected $collection_key = 'selectedObjects';
  /**
   * @var string
   */
  public $migrationType;
  protected $mysqlSyncConfigType = MySqlSyncConfig::class;
  protected $mysqlSyncConfigDataType = '';
  protected $selectedObjectsType = ExternalSyncSelectedObject::class;
  protected $selectedObjectsDataType = 'array';
  /**
   * @var string
   */
  public $syncMode;
  /**
   * @var string
   */
  public $syncParallelLevel;
  /**
   * @var bool
   */
  public $verifyConnectionOnly;
  /**
   * @var bool
   */
  public $verifyReplicationOnly;

  /**
   * @param string
   */
  public function setMigrationType($migrationType)
  {
    $this->migrationType = $migrationType;
  }
  /**
   * @return string
   */
  public function getMigrationType()
  {
    return $this->migrationType;
  }
  /**
   * @param MySqlSyncConfig
   */
  public function setMysqlSyncConfig(MySqlSyncConfig $mysqlSyncConfig)
  {
    $this->mysqlSyncConfig = $mysqlSyncConfig;
  }
  /**
   * @return MySqlSyncConfig
   */
  public function getMysqlSyncConfig()
  {
    return $this->mysqlSyncConfig;
  }
  /**
   * @param ExternalSyncSelectedObject[]
   */
  public function setSelectedObjects($selectedObjects)
  {
    $this->selectedObjects = $selectedObjects;
  }
  /**
   * @return ExternalSyncSelectedObject[]
   */
  public function getSelectedObjects()
  {
    return $this->selectedObjects;
  }
  /**
   * @param string
   */
  public function setSyncMode($syncMode)
  {
    $this->syncMode = $syncMode;
  }
  /**
   * @return string
   */
  public function getSyncMode()
  {
    return $this->syncMode;
  }
  /**
   * @param string
   */
  public function setSyncParallelLevel($syncParallelLevel)
  {
    $this->syncParallelLevel = $syncParallelLevel;
  }
  /**
   * @return string
   */
  public function getSyncParallelLevel()
  {
    return $this->syncParallelLevel;
  }
  /**
   * @param bool
   */
  public function setVerifyConnectionOnly($verifyConnectionOnly)
  {
    $this->verifyConnectionOnly = $verifyConnectionOnly;
  }
  /**
   * @return bool
   */
  public function getVerifyConnectionOnly()
  {
    return $this->verifyConnectionOnly;
  }
  /**
   * @param bool
   */
  public function setVerifyReplicationOnly($verifyReplicationOnly)
  {
    $this->verifyReplicationOnly = $verifyReplicationOnly;
  }
  /**
   * @return bool
   */
  public function getVerifyReplicationOnly()
  {
    return $this->verifyReplicationOnly;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SqlInstancesVerifyExternalSyncSettingsRequest::class, 'Google_Service_SQLAdmin_SqlInstancesVerifyExternalSyncSettingsRequest');
