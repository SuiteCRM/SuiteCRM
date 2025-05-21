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

namespace Google\Service\Backupdr;

class BackupRule extends \Google\Model
{
  /**
   * @var int
   */
  public $backupRetentionDays;
  /**
   * @var string
   */
  public $ruleId;
  protected $standardScheduleType = StandardSchedule::class;
  protected $standardScheduleDataType = '';

  /**
   * @param int
   */
  public function setBackupRetentionDays($backupRetentionDays)
  {
    $this->backupRetentionDays = $backupRetentionDays;
  }
  /**
   * @return int
   */
  public function getBackupRetentionDays()
  {
    return $this->backupRetentionDays;
  }
  /**
   * @param string
   */
  public function setRuleId($ruleId)
  {
    $this->ruleId = $ruleId;
  }
  /**
   * @return string
   */
  public function getRuleId()
  {
    return $this->ruleId;
  }
  /**
   * @param StandardSchedule
   */
  public function setStandardSchedule(StandardSchedule $standardSchedule)
  {
    $this->standardSchedule = $standardSchedule;
  }
  /**
   * @return StandardSchedule
   */
  public function getStandardSchedule()
  {
    return $this->standardSchedule;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BackupRule::class, 'Google_Service_Backupdr_BackupRule');
