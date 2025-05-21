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

namespace Google\Service\NetworkManagement;

class GoogleServiceInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $googleServiceType;
  /**
   * @var string
   */
  public $sourceIp;

  /**
   * @param string
   */
  public function setGoogleServiceType($googleServiceType)
  {
    $this->googleServiceType = $googleServiceType;
  }
  /**
   * @return string
   */
  public function getGoogleServiceType()
  {
    return $this->googleServiceType;
  }
  /**
   * @param string
   */
  public function setSourceIp($sourceIp)
  {
    $this->sourceIp = $sourceIp;
  }
  /**
   * @return string
   */
  public function getSourceIp()
  {
    return $this->sourceIp;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleServiceInfo::class, 'Google_Service_NetworkManagement_GoogleServiceInfo');
