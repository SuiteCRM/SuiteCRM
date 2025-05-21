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

namespace Google\Service\Calendar;

class EventFocusTimeProperties extends \Google\Model
{
  /**
   * @var string
   */
  public $autoDeclineMode;
  /**
   * @var string
   */
  public $chatStatus;
  /**
   * @var string
   */
  public $declineMessage;

  /**
   * @param string
   */
  public function setAutoDeclineMode($autoDeclineMode)
  {
    $this->autoDeclineMode = $autoDeclineMode;
  }
  /**
   * @return string
   */
  public function getAutoDeclineMode()
  {
    return $this->autoDeclineMode;
  }
  /**
   * @param string
   */
  public function setChatStatus($chatStatus)
  {
    $this->chatStatus = $chatStatus;
  }
  /**
   * @return string
   */
  public function getChatStatus()
  {
    return $this->chatStatus;
  }
  /**
   * @param string
   */
  public function setDeclineMessage($declineMessage)
  {
    $this->declineMessage = $declineMessage;
  }
  /**
   * @return string
   */
  public function getDeclineMessage()
  {
    return $this->declineMessage;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EventFocusTimeProperties::class, 'Google_Service_Calendar_EventFocusTimeProperties');
