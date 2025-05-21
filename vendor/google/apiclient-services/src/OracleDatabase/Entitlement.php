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

namespace Google\Service\OracleDatabase;

class Entitlement extends \Google\Model
{
  protected $cloudAccountDetailsType = CloudAccountDetails::class;
  protected $cloudAccountDetailsDataType = '';
  /**
   * @var string
   */
  public $entitlementId;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $state;

  /**
   * @param CloudAccountDetails
   */
  public function setCloudAccountDetails(CloudAccountDetails $cloudAccountDetails)
  {
    $this->cloudAccountDetails = $cloudAccountDetails;
  }
  /**
   * @return CloudAccountDetails
   */
  public function getCloudAccountDetails()
  {
    return $this->cloudAccountDetails;
  }
  /**
   * @param string
   */
  public function setEntitlementId($entitlementId)
  {
    $this->entitlementId = $entitlementId;
  }
  /**
   * @return string
   */
  public function getEntitlementId()
  {
    return $this->entitlementId;
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
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Entitlement::class, 'Google_Service_OracleDatabase_Entitlement');
