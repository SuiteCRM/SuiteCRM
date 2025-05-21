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

namespace Google\Service\AndroidPublisher;

class RemoteInAppUpdateDataPerBundle extends \Google\Model
{
  /**
   * @var string
   */
  public $recoveredDeviceCount;
  /**
   * @var string
   */
  public $totalDeviceCount;
  /**
   * @var string
   */
  public $versionCode;

  /**
   * @param string
   */
  public function setRecoveredDeviceCount($recoveredDeviceCount)
  {
    $this->recoveredDeviceCount = $recoveredDeviceCount;
  }
  /**
   * @return string
   */
  public function getRecoveredDeviceCount()
  {
    return $this->recoveredDeviceCount;
  }
  /**
   * @param string
   */
  public function setTotalDeviceCount($totalDeviceCount)
  {
    $this->totalDeviceCount = $totalDeviceCount;
  }
  /**
   * @return string
   */
  public function getTotalDeviceCount()
  {
    return $this->totalDeviceCount;
  }
  /**
   * @param string
   */
  public function setVersionCode($versionCode)
  {
    $this->versionCode = $versionCode;
  }
  /**
   * @return string
   */
  public function getVersionCode()
  {
    return $this->versionCode;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RemoteInAppUpdateDataPerBundle::class, 'Google_Service_AndroidPublisher_RemoteInAppUpdateDataPerBundle');
