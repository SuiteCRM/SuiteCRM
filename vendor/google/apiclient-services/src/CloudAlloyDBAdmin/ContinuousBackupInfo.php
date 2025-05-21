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

class ContinuousBackupInfo extends \Google\Collection
{
  protected $collection_key = 'schedule';
  /**
   * @var string
   */
  public $earliestRestorableTime;
  /**
   * @var string
   */
  public $enabledTime;
  protected $encryptionInfoType = EncryptionInfo::class;
  protected $encryptionInfoDataType = '';
  /**
   * @var string[]
   */
  public $schedule;

  /**
   * @param string
   */
  public function setEarliestRestorableTime($earliestRestorableTime)
  {
    $this->earliestRestorableTime = $earliestRestorableTime;
  }
  /**
   * @return string
   */
  public function getEarliestRestorableTime()
  {
    return $this->earliestRestorableTime;
  }
  /**
   * @param string
   */
  public function setEnabledTime($enabledTime)
  {
    $this->enabledTime = $enabledTime;
  }
  /**
   * @return string
   */
  public function getEnabledTime()
  {
    return $this->enabledTime;
  }
  /**
   * @param EncryptionInfo
   */
  public function setEncryptionInfo(EncryptionInfo $encryptionInfo)
  {
    $this->encryptionInfo = $encryptionInfo;
  }
  /**
   * @return EncryptionInfo
   */
  public function getEncryptionInfo()
  {
    return $this->encryptionInfo;
  }
  /**
   * @param string[]
   */
  public function setSchedule($schedule)
  {
    $this->schedule = $schedule;
  }
  /**
   * @return string[]
   */
  public function getSchedule()
  {
    return $this->schedule;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ContinuousBackupInfo::class, 'Google_Service_CloudAlloyDBAdmin_ContinuousBackupInfo');
