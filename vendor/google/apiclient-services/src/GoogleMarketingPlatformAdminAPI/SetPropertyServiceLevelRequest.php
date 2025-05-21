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

class SetPropertyServiceLevelRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $analyticsProperty;
  /**
   * @var string
   */
  public $serviceLevel;

  /**
   * @param string
   */
  public function setAnalyticsProperty($analyticsProperty)
  {
    $this->analyticsProperty = $analyticsProperty;
  }
  /**
   * @return string
   */
  public function getAnalyticsProperty()
  {
    return $this->analyticsProperty;
  }
  /**
   * @param string
   */
  public function setServiceLevel($serviceLevel)
  {
    $this->serviceLevel = $serviceLevel;
  }
  /**
   * @return string
   */
  public function getServiceLevel()
  {
    return $this->serviceLevel;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SetPropertyServiceLevelRequest::class, 'Google_Service_GoogleMarketingPlatformAdminAPI_SetPropertyServiceLevelRequest');
