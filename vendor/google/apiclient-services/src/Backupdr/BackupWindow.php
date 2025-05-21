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

class BackupWindow extends \Google\Model
{
  /**
   * @var int
   */
  public $endHourOfDay;
  /**
   * @var int
   */
  public $startHourOfDay;

  /**
   * @param int
   */
  public function setEndHourOfDay($endHourOfDay)
  {
    $this->endHourOfDay = $endHourOfDay;
  }
  /**
   * @return int
   */
  public function getEndHourOfDay()
  {
    return $this->endHourOfDay;
  }
  /**
   * @param int
   */
  public function setStartHourOfDay($startHourOfDay)
  {
    $this->startHourOfDay = $startHourOfDay;
  }
  /**
   * @return int
   */
  public function getStartHourOfDay()
  {
    return $this->startHourOfDay;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BackupWindow::class, 'Google_Service_Backupdr_BackupWindow');
