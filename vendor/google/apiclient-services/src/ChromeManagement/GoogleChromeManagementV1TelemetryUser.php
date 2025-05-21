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

class GoogleChromeManagementV1TelemetryUser extends \Google\Collection
{
  protected $collection_key = 'userDevice';
  /**
   * @var string
   */
  public $customer;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $orgUnitId;
  protected $userDeviceType = GoogleChromeManagementV1TelemetryUserDevice::class;
  protected $userDeviceDataType = 'array';
  /**
   * @var string
   */
  public $userEmail;
  /**
   * @var string
   */
  public $userId;

  /**
   * @param string
   */
  public function setCustomer($customer)
  {
    $this->customer = $customer;
  }
  /**
   * @return string
   */
  public function getCustomer()
  {
    return $this->customer;
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
  /**
   * @param string
   */
  public function setOrgUnitId($orgUnitId)
  {
    $this->orgUnitId = $orgUnitId;
  }
  /**
   * @return string
   */
  public function getOrgUnitId()
  {
    return $this->orgUnitId;
  }
  /**
   * @param GoogleChromeManagementV1TelemetryUserDevice[]
   */
  public function setUserDevice($userDevice)
  {
    $this->userDevice = $userDevice;
  }
  /**
   * @return GoogleChromeManagementV1TelemetryUserDevice[]
   */
  public function getUserDevice()
  {
    return $this->userDevice;
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
  public function setUserId($userId)
  {
    $this->userId = $userId;
  }
  /**
   * @return string
   */
  public function getUserId()
  {
    return $this->userId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleChromeManagementV1TelemetryUser::class, 'Google_Service_ChromeManagement_GoogleChromeManagementV1TelemetryUser');
