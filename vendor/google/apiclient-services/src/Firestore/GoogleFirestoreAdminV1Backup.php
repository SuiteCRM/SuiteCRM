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

namespace Google\Service\Firestore;

class GoogleFirestoreAdminV1Backup extends \Google\Model
{
  /**
   * @var string
   */
  public $database;
  /**
   * @var string
   */
  public $databaseUid;
  /**
   * @var string
   */
  public $expireTime;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $snapshotTime;
  /**
   * @var string
   */
  public $state;
  protected $statsType = GoogleFirestoreAdminV1Stats::class;
  protected $statsDataType = '';

  /**
   * @param string
   */
  public function setDatabase($database)
  {
    $this->database = $database;
  }
  /**
   * @return string
   */
  public function getDatabase()
  {
    return $this->database;
  }
  /**
   * @param string
   */
  public function setDatabaseUid($databaseUid)
  {
    $this->databaseUid = $databaseUid;
  }
  /**
   * @return string
   */
  public function getDatabaseUid()
  {
    return $this->databaseUid;
  }
  /**
   * @param string
   */
  public function setExpireTime($expireTime)
  {
    $this->expireTime = $expireTime;
  }
  /**
   * @return string
   */
  public function getExpireTime()
  {
    return $this->expireTime;
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
  public function setSnapshotTime($snapshotTime)
  {
    $this->snapshotTime = $snapshotTime;
  }
  /**
   * @return string
   */
  public function getSnapshotTime()
  {
    return $this->snapshotTime;
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
   * @param GoogleFirestoreAdminV1Stats
   */
  public function setStats(GoogleFirestoreAdminV1Stats $stats)
  {
    $this->stats = $stats;
  }
  /**
   * @return GoogleFirestoreAdminV1Stats
   */
  public function getStats()
  {
    return $this->stats;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleFirestoreAdminV1Backup::class, 'Google_Service_Firestore_GoogleFirestoreAdminV1Backup');
