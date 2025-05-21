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

namespace Google\Service\AndroidEnterprise;

class Device extends \Google\Model
{
  /**
   * @var string
   */
  public $androidId;
  /**
   * @var string
   */
  public $device;
  /**
   * @var string
   */
  public $latestBuildFingerprint;
  /**
   * @var string
   */
  public $maker;
  /**
   * @var string
   */
  public $managementType;
  /**
   * @var string
   */
  public $model;
  protected $policyType = Policy::class;
  protected $policyDataType = '';
  /**
   * @var string
   */
  public $product;
  protected $reportType = DeviceReport::class;
  protected $reportDataType = '';
  /**
   * @var string
   */
  public $retailBrand;
  /**
   * @var int
   */
  public $sdkVersion;

  /**
   * @param string
   */
  public function setAndroidId($androidId)
  {
    $this->androidId = $androidId;
  }
  /**
   * @return string
   */
  public function getAndroidId()
  {
    return $this->androidId;
  }
  /**
   * @param string
   */
  public function setDevice($device)
  {
    $this->device = $device;
  }
  /**
   * @return string
   */
  public function getDevice()
  {
    return $this->device;
  }
  /**
   * @param string
   */
  public function setLatestBuildFingerprint($latestBuildFingerprint)
  {
    $this->latestBuildFingerprint = $latestBuildFingerprint;
  }
  /**
   * @return string
   */
  public function getLatestBuildFingerprint()
  {
    return $this->latestBuildFingerprint;
  }
  /**
   * @param string
   */
  public function setMaker($maker)
  {
    $this->maker = $maker;
  }
  /**
   * @return string
   */
  public function getMaker()
  {
    return $this->maker;
  }
  /**
   * @param string
   */
  public function setManagementType($managementType)
  {
    $this->managementType = $managementType;
  }
  /**
   * @return string
   */
  public function getManagementType()
  {
    return $this->managementType;
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
   * @param Policy
   */
  public function setPolicy(Policy $policy)
  {
    $this->policy = $policy;
  }
  /**
   * @return Policy
   */
  public function getPolicy()
  {
    return $this->policy;
  }
  /**
   * @param string
   */
  public function setProduct($product)
  {
    $this->product = $product;
  }
  /**
   * @return string
   */
  public function getProduct()
  {
    return $this->product;
  }
  /**
   * @param DeviceReport
   */
  public function setReport(DeviceReport $report)
  {
    $this->report = $report;
  }
  /**
   * @return DeviceReport
   */
  public function getReport()
  {
    return $this->report;
  }
  /**
   * @param string
   */
  public function setRetailBrand($retailBrand)
  {
    $this->retailBrand = $retailBrand;
  }
  /**
   * @return string
   */
  public function getRetailBrand()
  {
    return $this->retailBrand;
  }
  /**
   * @param int
   */
  public function setSdkVersion($sdkVersion)
  {
    $this->sdkVersion = $sdkVersion;
  }
  /**
   * @return int
   */
  public function getSdkVersion()
  {
    return $this->sdkVersion;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Device::class, 'Google_Service_AndroidEnterprise_Device');
