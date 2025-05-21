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

namespace Google\Service\HangoutsChat;

class GoogleAppsCardV1Validation extends \Google\Model
{
  /**
   * @var int
   */
  public $characterLimit;
  /**
   * @var string
   */
  public $inputType;

  /**
   * @param int
   */
  public function setCharacterLimit($characterLimit)
  {
    $this->characterLimit = $characterLimit;
  }
  /**
   * @return int
   */
  public function getCharacterLimit()
  {
    return $this->characterLimit;
  }
  /**
   * @param string
   */
  public function setInputType($inputType)
  {
    $this->inputType = $inputType;
  }
  /**
   * @return string
   */
  public function getInputType()
  {
    return $this->inputType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAppsCardV1Validation::class, 'Google_Service_HangoutsChat_GoogleAppsCardV1Validation');
