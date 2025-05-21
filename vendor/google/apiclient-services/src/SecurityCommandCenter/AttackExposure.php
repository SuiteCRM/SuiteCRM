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

namespace Google\Service\SecurityCommandCenter;

class AttackExposure extends \Google\Model
{
  /**
   * @var string
   */
  public $attackExposureResult;
  /**
   * @var int
   */
  public $exposedHighValueResourcesCount;
  /**
   * @var int
   */
  public $exposedLowValueResourcesCount;
  /**
   * @var int
   */
  public $exposedMediumValueResourcesCount;
  /**
   * @var string
   */
  public $latestCalculationTime;
  public $score;
  /**
   * @var string
   */
  public $state;

  /**
   * @param string
   */
  public function setAttackExposureResult($attackExposureResult)
  {
    $this->attackExposureResult = $attackExposureResult;
  }
  /**
   * @return string
   */
  public function getAttackExposureResult()
  {
    return $this->attackExposureResult;
  }
  /**
   * @param int
   */
  public function setExposedHighValueResourcesCount($exposedHighValueResourcesCount)
  {
    $this->exposedHighValueResourcesCount = $exposedHighValueResourcesCount;
  }
  /**
   * @return int
   */
  public function getExposedHighValueResourcesCount()
  {
    return $this->exposedHighValueResourcesCount;
  }
  /**
   * @param int
   */
  public function setExposedLowValueResourcesCount($exposedLowValueResourcesCount)
  {
    $this->exposedLowValueResourcesCount = $exposedLowValueResourcesCount;
  }
  /**
   * @return int
   */
  public function getExposedLowValueResourcesCount()
  {
    return $this->exposedLowValueResourcesCount;
  }
  /**
   * @param int
   */
  public function setExposedMediumValueResourcesCount($exposedMediumValueResourcesCount)
  {
    $this->exposedMediumValueResourcesCount = $exposedMediumValueResourcesCount;
  }
  /**
   * @return int
   */
  public function getExposedMediumValueResourcesCount()
  {
    return $this->exposedMediumValueResourcesCount;
  }
  /**
   * @param string
   */
  public function setLatestCalculationTime($latestCalculationTime)
  {
    $this->latestCalculationTime = $latestCalculationTime;
  }
  /**
   * @return string
   */
  public function getLatestCalculationTime()
  {
    return $this->latestCalculationTime;
  }
  public function setScore($score)
  {
    $this->score = $score;
  }
  public function getScore()
  {
    return $this->score;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AttackExposure::class, 'Google_Service_SecurityCommandCenter_AttackExposure');
