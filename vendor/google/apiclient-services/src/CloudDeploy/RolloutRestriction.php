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

namespace Google\Service\CloudDeploy;

class RolloutRestriction extends \Google\Collection
{
  protected $collection_key = 'invokers';
  /**
   * @var string[]
   */
  public $actions;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string[]
   */
  public $invokers;
  protected $timeWindowsType = TimeWindows::class;
  protected $timeWindowsDataType = '';

  /**
   * @param string[]
   */
  public function setActions($actions)
  {
    $this->actions = $actions;
  }
  /**
   * @return string[]
   */
  public function getActions()
  {
    return $this->actions;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
  }
  /**
   * @param string[]
   */
  public function setInvokers($invokers)
  {
    $this->invokers = $invokers;
  }
  /**
   * @return string[]
   */
  public function getInvokers()
  {
    return $this->invokers;
  }
  /**
   * @param TimeWindows
   */
  public function setTimeWindows(TimeWindows $timeWindows)
  {
    $this->timeWindows = $timeWindows;
  }
  /**
   * @return TimeWindows
   */
  public function getTimeWindows()
  {
    return $this->timeWindows;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RolloutRestriction::class, 'Google_Service_CloudDeploy_RolloutRestriction');
