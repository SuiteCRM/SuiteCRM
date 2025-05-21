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

class GoogleFirestoreAdminV1RestoreDatabaseMetadata extends \Google\Model
{
  /**
   * @var string
   */
  public $backup;
  /**
   * @var string
   */
  public $database;
  /**
   * @var string
   */
  public $endTime;
  /**
   * @var string
   */
  public $operationState;
  protected $progressPercentageType = GoogleFirestoreAdminV1Progress::class;
  protected $progressPercentageDataType = '';
  /**
   * @var string
   */
  public $startTime;

  /**
   * @param string
   */
  public function setBackup($backup)
  {
    $this->backup = $backup;
  }
  /**
   * @return string
   */
  public function getBackup()
  {
    return $this->backup;
  }
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
  public function setEndTime($endTime)
  {
    $this->endTime = $endTime;
  }
  /**
   * @return string
   */
  public function getEndTime()
  {
    return $this->endTime;
  }
  /**
   * @param string
   */
  public function setOperationState($operationState)
  {
    $this->operationState = $operationState;
  }
  /**
   * @return string
   */
  public function getOperationState()
  {
    return $this->operationState;
  }
  /**
   * @param GoogleFirestoreAdminV1Progress
   */
  public function setProgressPercentage(GoogleFirestoreAdminV1Progress $progressPercentage)
  {
    $this->progressPercentage = $progressPercentage;
  }
  /**
   * @return GoogleFirestoreAdminV1Progress
   */
  public function getProgressPercentage()
  {
    return $this->progressPercentage;
  }
  /**
   * @param string
   */
  public function setStartTime($startTime)
  {
    $this->startTime = $startTime;
  }
  /**
   * @return string
   */
  public function getStartTime()
  {
    return $this->startTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleFirestoreAdminV1RestoreDatabaseMetadata::class, 'Google_Service_Firestore_GoogleFirestoreAdminV1RestoreDatabaseMetadata');
