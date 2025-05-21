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

namespace Google\Service\GoogleMarketingPlatformAdminAPI;

class AnalyticsAccountLink extends \Google\Model
{
  /**
   * @var string
   */
  public $analyticsAccount;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $linkVerificationState;
  /**
   * @var string
   */
  public $name;

  /**
   * @param string
   */
  public function setAnalyticsAccount($analyticsAccount)
  {
    $this->analyticsAccount = $analyticsAccount;
  }
  /**
   * @return string
   */
  public function getAnalyticsAccount()
  {
    return $this->analyticsAccount;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param string
   */
  public function setLinkVerificationState($linkVerificationState)
  {
    $this->linkVerificationState = $linkVerificationState;
  }
  /**
   * @return string
   */
  public function getLinkVerificationState()
  {
    return $this->linkVerificationState;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AnalyticsAccountLink::class, 'Google_Service_GoogleMarketingPlatformAdminAPI_AnalyticsAccountLink');
