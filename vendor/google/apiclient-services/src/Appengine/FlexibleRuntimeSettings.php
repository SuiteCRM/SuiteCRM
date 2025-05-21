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

namespace Google\Service\Appengine;

class FlexibleRuntimeSettings extends \Google\Model
{
  /**
   * @var string
   */
  public $operatingSystem;
  /**
   * @var string
   */
  public $runtimeVersion;

  /**
   * @param string
   */
  public function setOperatingSystem($operatingSystem)
  {
    $this->operatingSystem = $operatingSystem;
  }
  /**
   * @return string
   */
  public function getOperatingSystem()
  {
    return $this->operatingSystem;
  }
  /**
   * @param string
   */
  public function setRuntimeVersion($runtimeVersion)
  {
    $this->runtimeVersion = $runtimeVersion;
  }
  /**
   * @return string
   */
  public function getRuntimeVersion()
  {
    return $this->runtimeVersion;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(FlexibleRuntimeSettings::class, 'Google_Service_Appengine_FlexibleRuntimeSettings');
