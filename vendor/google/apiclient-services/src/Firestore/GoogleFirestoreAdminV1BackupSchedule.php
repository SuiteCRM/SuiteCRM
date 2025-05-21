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

class GoogleFirestoreAdminV1BackupSchedule extends \Google\Model
{
  /**
   * @var string
   */
  public $createTime;
  protected $dailyRecurrenceType = GoogleFirestoreAdminV1DailyRecurrence::class;
  protected $dailyRecurrenceDataType = '';
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $retention;
  /**
   * @var string
   */
  public $updateTime;
  protected $weeklyRecurrenceType = GoogleFirestoreAdminV1WeeklyRecurrence::class;
  protected $weeklyRecurrenceDataType = '';

  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param GoogleFirestoreAdminV1DailyRecurrence
   */
  public function setDailyRecurrence(GoogleFirestoreAdminV1DailyRecurrence $dailyRecurrence)
  {
    $this->dailyRecurrence = $dailyRecurrence;
  }
  /**
   * @return GoogleFirestoreAdminV1DailyRecurrence
   */
  public function getDailyRecurrence()
  {
    return $this->dailyRecurrence;
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
  public function setRetention($retention)
  {
    $this->retention = $retention;
  }
  /**
   * @return string
   */
  public function getRetention()
  {
    return $this->retention;
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
  /**
   * @param GoogleFirestoreAdminV1WeeklyRecurrence
   */
  public function setWeeklyRecurrence(GoogleFirestoreAdminV1WeeklyRecurrence $weeklyRecurrence)
  {
    $this->weeklyRecurrence = $weeklyRecurrence;
  }
  /**
   * @return GoogleFirestoreAdminV1WeeklyRecurrence
   */
  public function getWeeklyRecurrence()
  {
    return $this->weeklyRecurrence;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleFirestoreAdminV1BackupSchedule::class, 'Google_Service_Firestore_GoogleFirestoreAdminV1BackupSchedule');
