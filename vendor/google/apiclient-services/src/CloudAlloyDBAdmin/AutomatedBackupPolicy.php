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

namespace Google\Service\CloudAlloyDBAdmin;

class AutomatedBackupPolicy extends \Google\Model
{
  /**
   * @var string
   */
  public $backupWindow;
  /**
   * @var bool
   */
  public $enabled;
  protected $encryptionConfigType = EncryptionConfig::class;
  protected $encryptionConfigDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $location;
  protected $quantityBasedRetentionType = QuantityBasedRetention::class;
  protected $quantityBasedRetentionDataType = '';
  protected $timeBasedRetentionType = TimeBasedRetention::class;
  protected $timeBasedRetentionDataType = '';
  protected $weeklyScheduleType = WeeklySchedule::class;
  protected $weeklyScheduleDataType = '';

  /**
   * @param string
   */
  public function setBackupWindow($backupWindow)
  {
    $this->backupWindow = $backupWindow;
  }
  /**
   * @return string
   */
  public function getBackupWindow()
  {
    return $this->backupWindow;
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
   * @param EncryptionConfig
   */
  public function setEncryptionConfig(EncryptionConfig $encryptionConfig)
  {
    $this->encryptionConfig = $encryptionConfig;
  }
  /**
   * @return EncryptionConfig
   */
  public function getEncryptionConfig()
  {
    return $this->encryptionConfig;
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
   * @param string
   */
  public function setLocation($location)
  {
    $this->location = $location;
  }
  /**
   * @return string
   */
  public function getLocation()
  {
    return $this->location;
  }
  /**
   * @param QuantityBasedRetention
   */
  public function setQuantityBasedRetention(QuantityBasedRetention $quantityBasedRetention)
  {
    $this->quantityBasedRetention = $quantityBasedRetention;
  }
  /**
   * @return QuantityBasedRetention
   */
  public function getQuantityBasedRetention()
  {
    return $this->quantityBasedRetention;
  }
  /**
   * @param TimeBasedRetention
   */
  public function setTimeBasedRetention(TimeBasedRetention $timeBasedRetention)
  {
    $this->timeBasedRetention = $timeBasedRetention;
  }
  /**
   * @return TimeBasedRetention
   */
  public function getTimeBasedRetention()
  {
    return $this->timeBasedRetention;
  }
  /**
   * @param WeeklySchedule
   */
  public function setWeeklySchedule(WeeklySchedule $weeklySchedule)
  {
    $this->weeklySchedule = $weeklySchedule;
  }
  /**
   * @return WeeklySchedule
   */
  public function getWeeklySchedule()
  {
    return $this->weeklySchedule;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AutomatedBackupPolicy::class, 'Google_Service_CloudAlloyDBAdmin_AutomatedBackupPolicy');
