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

class SqlInstancesStartExternalSyncRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $migrationType;
  protected $mysqlSyncConfigType = MySqlSyncConfig::class;
  protected $mysqlSyncConfigDataType = '';
  /**
   * @var bool
   */
  public $skipVerification;
  /**
   * @var string
   */
  public $syncMode;
  /**
   * @var string
   */
  public $syncParallelLevel;

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
   * @param bool
   */
  public function setSkipVerification($skipVerification)
  {
    $this->skipVerification = $skipVerification;
  }
  /**
   * @return bool
   */
  public function getSkipVerification()
  {
    return $this->skipVerification;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SqlInstancesStartExternalSyncRequest::class, 'Google_Service_SQLAdmin_SqlInstancesStartExternalSyncRequest');
