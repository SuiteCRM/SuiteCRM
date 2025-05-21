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

namespace Google\Service\Compute;

class UpcomingMaintenance extends \Google\Model
{
  /**
   * @var bool
   */
  public $canReschedule;
  /**
   * @var string
   */
  public $latestWindowStartTime;
  /**
   * @var string
   */
  public $maintenanceStatus;
  /**
   * @var string
   */
  public $type;
  /**
   * @var string
   */
  public $windowEndTime;
  /**
   * @var string
   */
  public $windowStartTime;

  /**
   * @param bool
   */
  public function setCanReschedule($canReschedule)
  {
    $this->canReschedule = $canReschedule;
  }
  /**
   * @return bool
   */
  public function getCanReschedule()
  {
    return $this->canReschedule;
  }
  /**
   * @param string
   */
  public function setLatestWindowStartTime($latestWindowStartTime)
  {
    $this->latestWindowStartTime = $latestWindowStartTime;
  }
  /**
   * @return string
   */
  public function getLatestWindowStartTime()
  {
    return $this->latestWindowStartTime;
  }
  /**
   * @param string
   */
  public function setMaintenanceStatus($maintenanceStatus)
  {
    $this->maintenanceStatus = $maintenanceStatus;
  }
  /**
   * @return string
   */
  public function getMaintenanceStatus()
  {
    return $this->maintenanceStatus;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
  /**
   * @param string
   */
  public function setWindowEndTime($windowEndTime)
  {
    $this->windowEndTime = $windowEndTime;
  }
  /**
   * @return string
   */
  public function getWindowEndTime()
  {
    return $this->windowEndTime;
  }
  /**
   * @param string
   */
  public function setWindowStartTime($windowStartTime)
  {
    $this->windowStartTime = $windowStartTime;
  }
  /**
   * @return string
   */
  public function getWindowStartTime()
  {
    return $this->windowStartTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(UpcomingMaintenance::class, 'Google_Service_Compute_UpcomingMaintenance');
