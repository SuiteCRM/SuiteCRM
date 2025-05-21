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

namespace Google\Service\OracleDatabase;

class MaintenanceWindow extends \Google\Collection
{
  protected $collection_key = 'weeksOfMonth';
  /**
   * @var int
   */
  public $customActionTimeoutMins;
  /**
   * @var string[]
   */
  public $daysOfWeek;
  /**
   * @var int[]
   */
  public $hoursOfDay;
  /**
   * @var bool
   */
  public $isCustomActionTimeoutEnabled;
  /**
   * @var int
   */
  public $leadTimeWeek;
  /**
   * @var string[]
   */
  public $months;
  /**
   * @var string
   */
  public $patchingMode;
  /**
   * @var string
   */
  public $preference;
  /**
   * @var int[]
   */
  public $weeksOfMonth;

  /**
   * @param int
   */
  public function setCustomActionTimeoutMins($customActionTimeoutMins)
  {
    $this->customActionTimeoutMins = $customActionTimeoutMins;
  }
  /**
   * @return int
   */
  public function getCustomActionTimeoutMins()
  {
    return $this->customActionTimeoutMins;
  }
  /**
   * @param string[]
   */
  public function setDaysOfWeek($daysOfWeek)
  {
    $this->daysOfWeek = $daysOfWeek;
  }
  /**
   * @return string[]
   */
  public function getDaysOfWeek()
  {
    return $this->daysOfWeek;
  }
  /**
   * @param int[]
   */
  public function setHoursOfDay($hoursOfDay)
  {
    $this->hoursOfDay = $hoursOfDay;
  }
  /**
   * @return int[]
   */
  public function getHoursOfDay()
  {
    return $this->hoursOfDay;
  }
  /**
   * @param bool
   */
  public function setIsCustomActionTimeoutEnabled($isCustomActionTimeoutEnabled)
  {
    $this->isCustomActionTimeoutEnabled = $isCustomActionTimeoutEnabled;
  }
  /**
   * @return bool
   */
  public function getIsCustomActionTimeoutEnabled()
  {
    return $this->isCustomActionTimeoutEnabled;
  }
  /**
   * @param int
   */
  public function setLeadTimeWeek($leadTimeWeek)
  {
    $this->leadTimeWeek = $leadTimeWeek;
  }
  /**
   * @return int
   */
  public function getLeadTimeWeek()
  {
    return $this->leadTimeWeek;
  }
  /**
   * @param string[]
   */
  public function setMonths($months)
  {
    $this->months = $months;
  }
  /**
   * @return string[]
   */
  public function getMonths()
  {
    return $this->months;
  }
  /**
   * @param string
   */
  public function setPatchingMode($patchingMode)
  {
    $this->patchingMode = $patchingMode;
  }
  /**
   * @return string
   */
  public function getPatchingMode()
  {
    return $this->patchingMode;
  }
  /**
   * @param string
   */
  public function setPreference($preference)
  {
    $this->preference = $preference;
  }
  /**
   * @return string
   */
  public function getPreference()
  {
    return $this->preference;
  }
  /**
   * @param int[]
   */
  public function setWeeksOfMonth($weeksOfMonth)
  {
    $this->weeksOfMonth = $weeksOfMonth;
  }
  /**
   * @return int[]
   */
  public function getWeeksOfMonth()
  {
    return $this->weeksOfMonth;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(MaintenanceWindow::class, 'Google_Service_OracleDatabase_MaintenanceWindow');
