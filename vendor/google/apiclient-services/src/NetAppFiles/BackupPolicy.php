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

class BackupPolicy extends \Google\Model
{
  /**
   * @var int
   */
  public $assignedVolumeCount;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var int
   */
  public $dailyBackupLimit;
  /**
   * @var string
   */
  public $description;
  /**
   * @var bool
   */
  public $enabled;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var int
   */
  public $monthlyBackupLimit;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $state;
  /**
   * @var int
   */
  public $weeklyBackupLimit;

  /**
   * @param int
   */
  public function setAssignedVolumeCount($assignedVolumeCount)
  {
    $this->assignedVolumeCount = $assignedVolumeCount;
  }
  /**
   * @return int
   */
  public function getAssignedVolumeCount()
  {
    return $this->assignedVolumeCount;
  }
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
   * @param int
   */
  public function setDailyBackupLimit($dailyBackupLimit)
  {
    $this->dailyBackupLimit = $dailyBackupLimit;
  }
  /**
   * @return int
   */
  public function getDailyBackupLimit()
  {
    return $this->dailyBackupLimit;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param bool
   */
  public function setEnabled($enabled)
  {
    $this->enabled = $enabled;
  }
  /**
   * @return bool
   */
  public function getEnabled()
  {
    return $this->enabled;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
  }
  /**
   * @param int
   */
  public function setMonthlyBackupLimit($monthlyBackupLimit)
  {
    $this->monthlyBackupLimit = $monthlyBackupLimit;
  }
  /**
   * @return int
   */
  public function getMonthlyBackupLimit()
  {
    return $this->monthlyBackupLimit;
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
   * @param int
   */
  public function setWeeklyBackupLimit($weeklyBackupLimit)
  {
    $this->weeklyBackupLimit = $weeklyBackupLimit;
  }
  /**
   * @return int
   */
  public function getWeeklyBackupLimit()
  {
    return $this->weeklyBackupLimit;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BackupPolicy::class, 'Google_Service_NetAppFiles_BackupPolicy');
