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

namespace Google\Service\CloudRedis;

class AutomatedBackupConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $automatedBackupMode;
  protected $fixedFrequencyScheduleType = FixedFrequencySchedule::class;
  protected $fixedFrequencyScheduleDataType = '';
  /**
   * @var string
   */
  public $retention;

  /**
   * @param string
   */
  public function setAutomatedBackupMode($automatedBackupMode)
  {
    $this->automatedBackupMode = $automatedBackupMode;
  }
  /**
   * @return string
   */
  public function getAutomatedBackupMode()
  {
    return $this->automatedBackupMode;
  }
  /**
   * @param FixedFrequencySchedule
   */
  public function setFixedFrequencySchedule(FixedFrequencySchedule $fixedFrequencySchedule)
  {
    $this->fixedFrequencySchedule = $fixedFrequencySchedule;
  }
  /**
   * @return FixedFrequencySchedule
   */
  public function getFixedFrequencySchedule()
  {
    return $this->fixedFrequencySchedule;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AutomatedBackupConfig::class, 'Google_Service_CloudRedis_AutomatedBackupConfig');
