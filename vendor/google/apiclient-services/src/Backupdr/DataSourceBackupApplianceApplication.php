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

namespace Google\Service\Backupdr;

class DataSourceBackupApplianceApplication extends \Google\Model
{
  /**
   * @var string
   */
  public $applianceId;
  /**
   * @var string
   */
  public $applicationId;
  /**
   * @var string
   */
  public $applicationName;
  /**
   * @var string
   */
  public $backupAppliance;
  /**
   * @var string
   */
  public $hostId;
  /**
   * @var string
   */
  public $hostname;
  /**
   * @var string
   */
  public $type;

  /**
   * @param string
   */
  public function setApplianceId($applianceId)
  {
    $this->applianceId = $applianceId;
  }
  /**
   * @return string
   */
  public function getApplianceId()
  {
    return $this->applianceId;
  }
  /**
   * @param string
   */
  public function setApplicationId($applicationId)
  {
    $this->applicationId = $applicationId;
  }
  /**
   * @return string
   */
  public function getApplicationId()
  {
    return $this->applicationId;
  }
  /**
   * @param string
   */
  public function setApplicationName($applicationName)
  {
    $this->applicationName = $applicationName;
  }
  /**
   * @return string
   */
  public function getApplicationName()
  {
    return $this->applicationName;
  }
  /**
   * @param string
   */
  public function setBackupAppliance($backupAppliance)
  {
    $this->backupAppliance = $backupAppliance;
  }
  /**
   * @return string
   */
  public function getBackupAppliance()
  {
    return $this->backupAppliance;
  }
  /**
   * @param string
   */
  public function setHostId($hostId)
  {
    $this->hostId = $hostId;
  }
  /**
   * @return string
   */
  public function getHostId()
  {
    return $this->hostId;
  }
  /**
   * @param string
   */
  public function setHostname($hostname)
  {
    $this->hostname = $hostname;
  }
  /**
   * @return string
   */
  public function getHostname()
  {
    return $this->hostname;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DataSourceBackupApplianceApplication::class, 'Google_Service_Backupdr_DataSourceBackupApplianceApplication');
