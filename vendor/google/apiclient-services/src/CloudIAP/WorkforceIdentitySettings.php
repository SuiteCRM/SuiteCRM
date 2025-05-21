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

namespace Google\Service\CloudIAP;

class WorkforceIdentitySettings extends \Google\Collection
{
  protected $collection_key = 'workforcePools';
  protected $oauth2Type = OAuth2::class;
  protected $oauth2DataType = '';
  /**
   * @var string[]
   */
  public $workforcePools;

  /**
   * @param OAuth2
   */
  public function setOauth2(OAuth2 $oauth2)
  {
    $this->oauth2 = $oauth2;
  }
  /**
   * @return OAuth2
   */
  public function getOauth2()
  {
    return $this->oauth2;
  }
  /**
   * @param string[]
   */
  public function setWorkforcePools($workforcePools)
  {
    $this->workforcePools = $workforcePools;
  }
  /**
   * @return string[]
   */
  public function getWorkforcePools()
  {
    return $this->workforcePools;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(WorkforceIdentitySettings::class, 'Google_Service_CloudIAP_WorkforceIdentitySettings');
