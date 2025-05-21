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

class FinalizeBackupRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $backupId;
  /**
   * @var string
   */
  public $consistencyTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $recoveryRangeEndTime;
  /**
   * @var string
   */
  public $recoveryRangeStartTime;
  /**
   * @var string
   */
  public $requestId;
  /**
   * @var string
   */
  public $retentionDuration;

  /**
   * @param string
   */
  public function setBackupId($backupId)
  {
    $this->backupId = $backupId;
  }
  /**
   * @return string
   */
  public function getBackupId()
  {
    return $this->backupId;
  }
  /**
   * @param string
   */
  public function setConsistencyTime($consistencyTime)
  {
    $this->consistencyTime = $consistencyTime;
  }
  /**
   * @return string
   */
  public function getConsistencyTime()
  {
    return $this->consistencyTime;
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
   * @param string
   */
  public function setRecoveryRangeEndTime($recoveryRangeEndTime)
  {
    $this->recoveryRangeEndTime = $recoveryRangeEndTime;
  }
  /**
   * @return string
   */
  public function getRecoveryRangeEndTime()
  {
    return $this->recoveryRangeEndTime;
  }
  /**
   * @param string
   */
  public function setRecoveryRangeStartTime($recoveryRangeStartTime)
  {
    $this->recoveryRangeStartTime = $recoveryRangeStartTime;
  }
  /**
   * @return string
   */
  public function getRecoveryRangeStartTime()
  {
    return $this->recoveryRangeStartTime;
  }
  /**
   * @param string
   */
  public function setRequestId($requestId)
  {
    $this->requestId = $requestId;
  }
  /**
   * @return string
   */
  public function getRequestId()
  {
    return $this->requestId;
  }
  /**
   * @param string
   */
  public function setRetentionDuration($retentionDuration)
  {
    $this->retentionDuration = $retentionDuration;
  }
  /**
   * @return string
   */
  public function getRetentionDuration()
  {
    return $this->retentionDuration;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(FinalizeBackupRequest::class, 'Google_Service_Backupdr_FinalizeBackupRequest');
