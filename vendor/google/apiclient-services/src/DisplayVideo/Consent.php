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

namespace Google\Service\DisplayVideo;

class Consent extends \Google\Model
{
  /**
   * @var string
   */
  public $adPersonalization;
  /**
   * @var string
   */
  public $adUserData;

  /**
   * @param string
   */
  public function setAdPersonalization($adPersonalization)
  {
    $this->adPersonalization = $adPersonalization;
  }
  /**
   * @return string
   */
  public function getAdPersonalization()
  {
    return $this->adPersonalization;
  }
  /**
   * @param string
   */
  public function setAdUserData($adUserData)
  {
    $this->adUserData = $adUserData;
  }
  /**
   * @return string
   */
  public function getAdUserData()
  {
    return $this->adUserData;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Consent::class, 'Google_Service_DisplayVideo_Consent');
