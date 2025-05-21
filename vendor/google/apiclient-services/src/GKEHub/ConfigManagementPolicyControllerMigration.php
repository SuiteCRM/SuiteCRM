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

namespace Google\Service\GKEHub;

class ConfigManagementPolicyControllerMigration extends \Google\Model
{
  /**
   * @var string
   */
  public $copyTime;
  /**
   * @var string
   */
  public $stage;

  /**
   * @param string
   */
  public function setCopyTime($copyTime)
  {
    $this->copyTime = $copyTime;
  }
  /**
   * @return string
   */
  public function getCopyTime()
  {
    return $this->copyTime;
  }
  /**
   * @param string
   */
  public function setStage($stage)
  {
    $this->stage = $stage;
  }
  /**
   * @return string
   */
  public function getStage()
  {
    return $this->stage;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ConfigManagementPolicyControllerMigration::class, 'Google_Service_GKEHub_ConfigManagementPolicyControllerMigration');
