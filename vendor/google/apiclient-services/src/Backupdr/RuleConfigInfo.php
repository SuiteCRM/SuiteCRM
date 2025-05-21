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

class RuleConfigInfo extends \Google\Model
{
  protected $lastBackupErrorType = Status::class;
  protected $lastBackupErrorDataType = '';
  /**
   * @var string
   */
  public $lastBackupState;
  /**
   * @var string
   */
  public $lastSuccessfulBackupConsistencyTime;
  /**
   * @var string
   */
  public $ruleId;

  /**
   * @param Status
   */
  public function setLastBackupError(Status $lastBackupError)
  {
    $this->lastBackupError = $lastBackupError;
  }
  /**
   * @return Status
   */
  public function getLastBackupError()
  {
    return $this->lastBackupError;
  }
  /**
   * @param string
   */
  public function setLastBackupState($lastBackupState)
  {
    $this->lastBackupState = $lastBackupState;
  }
  /**
   * @return string
   */
  public function getLastBackupState()
  {
    return $this->lastBackupState;
  }
  /**
   * @param string
   */
  public function setLastSuccessfulBackupConsistencyTime($lastSuccessfulBackupConsistencyTime)
  {
    $this->lastSuccessfulBackupConsistencyTime = $lastSuccessfulBackupConsistencyTime;
  }
  /**
   * @return string
   */
  public function getLastSuccessfulBackupConsistencyTime()
  {
    return $this->lastSuccessfulBackupConsistencyTime;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RuleConfigInfo::class, 'Google_Service_Backupdr_RuleConfigInfo');
