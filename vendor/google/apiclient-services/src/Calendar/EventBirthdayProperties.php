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

class EventBirthdayProperties extends \Google\Model
{
  /**
   * @var string
   */
  public $contact;
  /**
   * @var string
   */
  public $customTypeName;
  /**
   * @var string
   */
  public $type;

  /**
   * @param string
   */
  public function setContact($contact)
  {
    $this->contact = $contact;
  }
  /**
   * @return string
   */
  public function getContact()
  {
    return $this->contact;
  }
  /**
   * @param string
   */
  public function setCustomTypeName($customTypeName)
  {
    $this->customTypeName = $customTypeName;
  }
  /**
   * @return string
   */
  public function getCustomTypeName()
  {
    return $this->customTypeName;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EventBirthdayProperties::class, 'Google_Service_Calendar_EventBirthdayProperties');
