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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1alphaIdentityScheduleConfig extends \Google\Model
{
  protected $nextSyncTimeType = GoogleTypeDateTime::class;
  protected $nextSyncTimeDataType = '';
  /**
   * @var string
   */
  public $refreshInterval;

  /**
   * @param GoogleTypeDateTime
   */
  public function setNextSyncTime(GoogleTypeDateTime $nextSyncTime)
  {
    $this->nextSyncTime = $nextSyncTime;
  }
  /**
   * @return GoogleTypeDateTime
   */
  public function getNextSyncTime()
  {
    return $this->nextSyncTime;
  }
  /**
   * @param string
   */
  public function setRefreshInterval($refreshInterval)
  {
    $this->refreshInterval = $refreshInterval;
  }
  /**
   * @return string
   */
  public function getRefreshInterval()
  {
    return $this->refreshInterval;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1alphaIdentityScheduleConfig::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1alphaIdentityScheduleConfig');
