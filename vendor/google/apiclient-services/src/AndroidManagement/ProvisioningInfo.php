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

namespace Google\Service\AndroidManagement;

class ProvisioningInfo extends \Google\Model
{
  /**
   * @var int
   */
  public $apiLevel;
  /**
   * @var string
   */
  public $authenticatedUserEmail;
  /**
   * @var string
   */
  public $brand;
  /**
   * @var string
   */
  public $enterprise;
  /**
   * @var string
   */
  public $imei;
  /**
   * @var string
   */
  public $managementMode;
  /**
   * @var string
   */
  public $meid;
  /**
   * @var string
   */
  public $model;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $ownership;
  /**
   * @var string
   */
  public $serialNumber;

  /**
   * @param int
   */
  public function setApiLevel($apiLevel)
  {
    $this->apiLevel = $apiLevel;
  }
  /**
   * @return int
   */
  public function getApiLevel()
  {
    return $this->apiLevel;
  }
  /**
   * @param string
   */
  public function setAuthenticatedUserEmail($authenticatedUserEmail)
  {
    $this->authenticatedUserEmail = $authenticatedUserEmail;
  }
  /**
   * @return string
   */
  public function getAuthenticatedUserEmail()
  {
    return $this->authenticatedUserEmail;
  }
  /**
   * @param string
   */
  public function setBrand($brand)
  {
    $this->brand = $brand;
  }
  /**
   * @return string
   */
  public function getBrand()
  {
    return $this->brand;
  }
  /**
   * @param string
   */
  public function setEnterprise($enterprise)
  {
    $this->enterprise = $enterprise;
  }
  /**
   * @return string
   */
  public function getEnterprise()
  {
    return $this->enterprise;
  }
  /**
   * @param string
   */
  public function setImei($imei)
  {
    $this->imei = $imei;
  }
  /**
   * @return string
   */
  public function getImei()
  {
    return $this->imei;
  }
  /**
   * @param string
   */
  public function setManagementMode($managementMode)
  {
    $this->managementMode = $managementMode;
  }
  /**
   * @return string
   */
  public function getManagementMode()
  {
    return $this->managementMode;
  }
  /**
   * @param string
   */
  public function setMeid($meid)
  {
    $this->meid = $meid;
  }
  /**
   * @return string
   */
  public function getMeid()
  {
    return $this->meid;
  }
  /**
   * @param string
   */
  public function setModel($model)
  {
    $this->model = $model;
  }
  /**
   * @return string
   */
  public function getModel()
  {
    return $this->model;
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
  public function setOwnership($ownership)
  {
    $this->ownership = $ownership;
  }
  /**
   * @return string
   */
  public function getOwnership()
  {
    return $this->ownership;
  }
  /**
   * @param string
   */
  public function setSerialNumber($serialNumber)
  {
    $this->serialNumber = $serialNumber;
  }
  /**
   * @return string
   */
  public function getSerialNumber()
  {
    return $this->serialNumber;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProvisioningInfo::class, 'Google_Service_AndroidManagement_ProvisioningInfo');
