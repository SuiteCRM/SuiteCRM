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

namespace Google\Service\ChromeManagement;

class GoogleChromeManagementV1TelemetryNotificationFilter extends \Google\Model
{
  /**
   * @var string
   */
  public $deviceId;
  /**
   * @var string
   */
  public $deviceOrgUnitId;
  protected $telemetryEventNotificationFilterType = GoogleChromeManagementV1TelemetryEventNotificationFilter::class;
  protected $telemetryEventNotificationFilterDataType = '';
  /**
   * @var string
   */
  public $userEmail;
  /**
   * @var string
   */
  public $userOrgUnitId;

  /**
   * @param string
   */
  public function setDeviceId($deviceId)
  {
    $this->deviceId = $deviceId;
  }
  /**
   * @return string
   */
  public function getDeviceId()
  {
    return $this->deviceId;
  }
  /**
   * @param string
   */
  public function setDeviceOrgUnitId($deviceOrgUnitId)
  {
    $this->deviceOrgUnitId = $deviceOrgUnitId;
  }
  /**
   * @return string
   */
  public function getDeviceOrgUnitId()
  {
    return $this->deviceOrgUnitId;
  }
  /**
   * @param GoogleChromeManagementV1TelemetryEventNotificationFilter
   */
  public function setTelemetryEventNotificationFilter(GoogleChromeManagementV1TelemetryEventNotificationFilter $telemetryEventNotificationFilter)
  {
    $this->telemetryEventNotificationFilter = $telemetryEventNotificationFilter;
  }
  /**
   * @return GoogleChromeManagementV1TelemetryEventNotificationFilter
   */
  public function getTelemetryEventNotificationFilter()
  {
    return $this->telemetryEventNotificationFilter;
  }
  /**
   * @param string
   */
  public function setUserEmail($userEmail)
  {
    $this->userEmail = $userEmail;
  }
  /**
   * @return string
   */
  public function getUserEmail()
  {
    return $this->userEmail;
  }
  /**
   * @param string
   */
  public function setUserOrgUnitId($userOrgUnitId)
  {
    $this->userOrgUnitId = $userOrgUnitId;
  }
  /**
   * @return string
   */
  public function getUserOrgUnitId()
  {
    return $this->userOrgUnitId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleChromeManagementV1TelemetryNotificationFilter::class, 'Google_Service_ChromeManagement_GoogleChromeManagementV1TelemetryNotificationFilter');
