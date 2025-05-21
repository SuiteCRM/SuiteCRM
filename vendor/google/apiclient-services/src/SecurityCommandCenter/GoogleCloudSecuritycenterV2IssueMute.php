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

class GoogleCloudSecuritycenterV2IssueMute extends \Google\Model
{
  /**
   * @var string
   */
  public $muteInitiator;
  /**
   * @var string
   */
  public $muteReason;
  /**
   * @var string
   */
  public $muteState;
  /**
   * @var string
   */
  public $muteUpdateTime;

  /**
   * @param string
   */
  public function setMuteInitiator($muteInitiator)
  {
    $this->muteInitiator = $muteInitiator;
  }
  /**
   * @return string
   */
  public function getMuteInitiator()
  {
    return $this->muteInitiator;
  }
  /**
   * @param string
   */
  public function setMuteReason($muteReason)
  {
    $this->muteReason = $muteReason;
  }
  /**
   * @return string
   */
  public function getMuteReason()
  {
    return $this->muteReason;
  }
  /**
   * @param string
   */
  public function setMuteState($muteState)
  {
    $this->muteState = $muteState;
  }
  /**
   * @return string
   */
  public function getMuteState()
  {
    return $this->muteState;
  }
  /**
   * @param string
   */
  public function setMuteUpdateTime($muteUpdateTime)
  {
    $this->muteUpdateTime = $muteUpdateTime;
  }
  /**
   * @return string
   */
  public function getMuteUpdateTime()
  {
    return $this->muteUpdateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudSecuritycenterV2IssueMute::class, 'Google_Service_SecurityCommandCenter_GoogleCloudSecuritycenterV2IssueMute');
