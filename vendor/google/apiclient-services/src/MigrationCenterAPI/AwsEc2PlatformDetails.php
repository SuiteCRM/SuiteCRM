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

namespace Google\Service\MigrationCenterAPI;

class AwsEc2PlatformDetails extends \Google\Model
{
  /**
   * @var string
   */
  public $hyperthreading;
  /**
   * @var string
   */
  public $location;
  /**
   * @var string
   */
  public $machineTypeLabel;

  /**
   * @param string
   */
  public function setHyperthreading($hyperthreading)
  {
    $this->hyperthreading = $hyperthreading;
  }
  /**
   * @return string
   */
  public function getHyperthreading()
  {
    return $this->hyperthreading;
  }
  /**
   * @param string
   */
  public function setLocation($location)
  {
    $this->location = $location;
  }
  /**
   * @return string
   */
  public function getLocation()
  {
    return $this->location;
  }
  /**
   * @param string
   */
  public function setMachineTypeLabel($machineTypeLabel)
  {
    $this->machineTypeLabel = $machineTypeLabel;
  }
  /**
   * @return string
   */
  public function getMachineTypeLabel()
  {
    return $this->machineTypeLabel;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AwsEc2PlatformDetails::class, 'Google_Service_MigrationCenterAPI_AwsEc2PlatformDetails');
