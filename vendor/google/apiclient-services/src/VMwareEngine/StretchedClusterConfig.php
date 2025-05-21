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

namespace Google\Service\VMwareEngine;

class StretchedClusterConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $preferredLocation;
  /**
   * @var string
   */
  public $secondaryLocation;

  /**
   * @param string
   */
  public function setPreferredLocation($preferredLocation)
  {
    $this->preferredLocation = $preferredLocation;
  }
  /**
   * @return string
   */
  public function getPreferredLocation()
  {
    return $this->preferredLocation;
  }
  /**
   * @param string
   */
  public function setSecondaryLocation($secondaryLocation)
  {
    $this->secondaryLocation = $secondaryLocation;
  }
  /**
   * @return string
   */
  public function getSecondaryLocation()
  {
    return $this->secondaryLocation;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(StretchedClusterConfig::class, 'Google_Service_VMwareEngine_StretchedClusterConfig');
