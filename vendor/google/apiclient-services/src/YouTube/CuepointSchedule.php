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

namespace Google\Service\YouTube;

class CuepointSchedule extends \Google\Model
{
  /**
   * @var bool
   */
  public $enabled;
  /**
   * @var string
   */
  public $pauseAdsUntil;
  /**
   * @var int
   */
  public $repeatIntervalSecs;
  /**
   * @var string
   */
  public $scheduleStrategy;

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
   * @param string
   */
  public function setPauseAdsUntil($pauseAdsUntil)
  {
    $this->pauseAdsUntil = $pauseAdsUntil;
  }
  /**
   * @return string
   */
  public function getPauseAdsUntil()
  {
    return $this->pauseAdsUntil;
  }
  /**
   * @param int
   */
  public function setRepeatIntervalSecs($repeatIntervalSecs)
  {
    $this->repeatIntervalSecs = $repeatIntervalSecs;
  }
  /**
   * @return int
   */
  public function getRepeatIntervalSecs()
  {
    return $this->repeatIntervalSecs;
  }
  /**
   * @param string
   */
  public function setScheduleStrategy($scheduleStrategy)
  {
    $this->scheduleStrategy = $scheduleStrategy;
  }
  /**
   * @return string
   */
  public function getScheduleStrategy()
  {
    return $this->scheduleStrategy;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CuepointSchedule::class, 'Google_Service_YouTube_CuepointSchedule');
