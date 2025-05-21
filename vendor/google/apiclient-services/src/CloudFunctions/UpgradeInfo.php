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

namespace Google\Service\CloudFunctions;

class UpgradeInfo extends \Google\Model
{
  protected $buildConfigType = BuildConfig::class;
  protected $buildConfigDataType = '';
  protected $eventTriggerType = EventTrigger::class;
  protected $eventTriggerDataType = '';
  protected $serviceConfigType = ServiceConfig::class;
  protected $serviceConfigDataType = '';
  /**
   * @var string
   */
  public $upgradeState;

  /**
   * @param BuildConfig
   */
  public function setBuildConfig(BuildConfig $buildConfig)
  {
    $this->buildConfig = $buildConfig;
  }
  /**
   * @return BuildConfig
   */
  public function getBuildConfig()
  {
    return $this->buildConfig;
  }
  /**
   * @param EventTrigger
   */
  public function setEventTrigger(EventTrigger $eventTrigger)
  {
    $this->eventTrigger = $eventTrigger;
  }
  /**
   * @return EventTrigger
   */
  public function getEventTrigger()
  {
    return $this->eventTrigger;
  }
  /**
   * @param ServiceConfig
   */
  public function setServiceConfig(ServiceConfig $serviceConfig)
  {
    $this->serviceConfig = $serviceConfig;
  }
  /**
   * @return ServiceConfig
   */
  public function getServiceConfig()
  {
    return $this->serviceConfig;
  }
  /**
   * @param string
   */
  public function setUpgradeState($upgradeState)
  {
    $this->upgradeState = $upgradeState;
  }
  /**
   * @return string
   */
  public function getUpgradeState()
  {
    return $this->upgradeState;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(UpgradeInfo::class, 'Google_Service_CloudFunctions_UpgradeInfo');
