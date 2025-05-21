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

namespace Google\Service\PlayIntegrity;

class AppAccessRiskVerdict extends \Google\Collection
{
  protected $collection_key = 'appsDetected';
  /**
   * @var string[]
   */
  public $appsDetected;
  /**
   * @var string
   */
  public $otherApps;
  /**
   * @var string
   */
  public $playOrSystemApps;

  /**
   * @param string[]
   */
  public function setAppsDetected($appsDetected)
  {
    $this->appsDetected = $appsDetected;
  }
  /**
   * @return string[]
   */
  public function getAppsDetected()
  {
    return $this->appsDetected;
  }
  /**
   * @param string
   */
  public function setOtherApps($otherApps)
  {
    $this->otherApps = $otherApps;
  }
  /**
   * @return string
   */
  public function getOtherApps()
  {
    return $this->otherApps;
  }
  /**
   * @param string
   */
  public function setPlayOrSystemApps($playOrSystemApps)
  {
    $this->playOrSystemApps = $playOrSystemApps;
  }
  /**
   * @return string
   */
  public function getPlayOrSystemApps()
  {
    return $this->playOrSystemApps;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AppAccessRiskVerdict::class, 'Google_Service_PlayIntegrity_AppAccessRiskVerdict');
