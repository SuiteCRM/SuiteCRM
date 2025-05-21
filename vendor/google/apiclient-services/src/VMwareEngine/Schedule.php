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

namespace Google\Service\VMwareEngine;

class Schedule extends \Google\Collection
{
  protected $collection_key = 'weeklyWindows';
  protected $constraintsType = Constraints::class;
  protected $constraintsDataType = '';
  protected $editWindowType = Interval::class;
  protected $editWindowDataType = '';
  /**
   * @var string
   */
  public $lastEditor;
  /**
   * @var string
   */
  public $startTime;
  protected $weeklyWindowsType = TimeWindow::class;
  protected $weeklyWindowsDataType = 'array';

  /**
   * @param Constraints
   */
  public function setConstraints(Constraints $constraints)
  {
    $this->constraints = $constraints;
  }
  /**
   * @return Constraints
   */
  public function getConstraints()
  {
    return $this->constraints;
  }
  /**
   * @param Interval
   */
  public function setEditWindow(Interval $editWindow)
  {
    $this->editWindow = $editWindow;
  }
  /**
   * @return Interval
   */
  public function getEditWindow()
  {
    return $this->editWindow;
  }
  /**
   * @param string
   */
  public function setLastEditor($lastEditor)
  {
    $this->lastEditor = $lastEditor;
  }
  /**
   * @return string
   */
  public function getLastEditor()
  {
    return $this->lastEditor;
  }
  /**
   * @param string
   */
  public function setStartTime($startTime)
  {
    $this->startTime = $startTime;
  }
  /**
   * @return string
   */
  public function getStartTime()
  {
    return $this->startTime;
  }
  /**
   * @param TimeWindow[]
   */
  public function setWeeklyWindows($weeklyWindows)
  {
    $this->weeklyWindows = $weeklyWindows;
  }
  /**
   * @return TimeWindow[]
   */
  public function getWeeklyWindows()
  {
    return $this->weeklyWindows;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Schedule::class, 'Google_Service_VMwareEngine_Schedule');
