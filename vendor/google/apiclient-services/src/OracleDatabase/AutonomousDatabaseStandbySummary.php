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

class AutonomousDatabaseStandbySummary extends \Google\Model
{
  /**
   * @var string
   */
  public $dataGuardRoleChangedTime;
  /**
   * @var string
   */
  public $disasterRecoveryRoleChangedTime;
  /**
   * @var string
   */
  public $lagTimeDuration;
  /**
   * @var string
   */
  public $lifecycleDetails;
  /**
   * @var string
   */
  public $state;

  /**
   * @param string
   */
  public function setDataGuardRoleChangedTime($dataGuardRoleChangedTime)
  {
    $this->dataGuardRoleChangedTime = $dataGuardRoleChangedTime;
  }
  /**
   * @return string
   */
  public function getDataGuardRoleChangedTime()
  {
    return $this->dataGuardRoleChangedTime;
  }
  /**
   * @param string
   */
  public function setDisasterRecoveryRoleChangedTime($disasterRecoveryRoleChangedTime)
  {
    $this->disasterRecoveryRoleChangedTime = $disasterRecoveryRoleChangedTime;
  }
  /**
   * @return string
   */
  public function getDisasterRecoveryRoleChangedTime()
  {
    return $this->disasterRecoveryRoleChangedTime;
  }
  /**
   * @param string
   */
  public function setLagTimeDuration($lagTimeDuration)
  {
    $this->lagTimeDuration = $lagTimeDuration;
  }
  /**
   * @return string
   */
  public function getLagTimeDuration()
  {
    return $this->lagTimeDuration;
  }
  /**
   * @param string
   */
  public function setLifecycleDetails($lifecycleDetails)
  {
    $this->lifecycleDetails = $lifecycleDetails;
  }
  /**
   * @return string
   */
  public function getLifecycleDetails()
  {
    return $this->lifecycleDetails;
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
class_alias(AutonomousDatabaseStandbySummary::class, 'Google_Service_OracleDatabase_AutonomousDatabaseStandbySummary');
