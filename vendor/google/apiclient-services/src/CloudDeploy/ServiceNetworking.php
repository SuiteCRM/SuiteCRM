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

namespace Google\Service\CloudDeploy;

class ServiceNetworking extends \Google\Model
{
  /**
   * @var string
   */
  public $deployment;
  /**
   * @var bool
   */
  public $disablePodOverprovisioning;
  /**
   * @var string
   */
  public $podSelectorLabel;
  /**
   * @var string
   */
  public $service;

  /**
   * @param string
   */
  public function setDeployment($deployment)
  {
    $this->deployment = $deployment;
  }
  /**
   * @return string
   */
  public function getDeployment()
  {
    return $this->deployment;
  }
  /**
   * @param bool
   */
  public function setDisablePodOverprovisioning($disablePodOverprovisioning)
  {
    $this->disablePodOverprovisioning = $disablePodOverprovisioning;
  }
  /**
   * @return bool
   */
  public function getDisablePodOverprovisioning()
  {
    return $this->disablePodOverprovisioning;
  }
  /**
   * @param string
   */
  public function setPodSelectorLabel($podSelectorLabel)
  {
    $this->podSelectorLabel = $podSelectorLabel;
  }
  /**
   * @return string
   */
  public function getPodSelectorLabel()
  {
    return $this->podSelectorLabel;
  }
  /**
   * @param string
   */
  public function setService($service)
  {
    $this->service = $service;
  }
  /**
   * @return string
   */
  public function getService()
  {
    return $this->service;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ServiceNetworking::class, 'Google_Service_CloudDeploy_ServiceNetworking');
