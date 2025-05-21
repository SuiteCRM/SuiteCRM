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

namespace Google\Service\Integrations;

class GoogleCloudIntegrationsV1alphaProjectProperties extends \Google\Collection
{
  protected $collection_key = 'provisionedRegions';
  /**
   * @var string
   */
  public $billingType;
  /**
   * @var string
   */
  public $ipEnablementState;
  /**
   * @var string[]
   */
  public $provisionedRegions;

  /**
   * @param string
   */
  public function setBillingType($billingType)
  {
    $this->billingType = $billingType;
  }
  /**
   * @return string
   */
  public function getBillingType()
  {
    return $this->billingType;
  }
  /**
   * @param string
   */
  public function setIpEnablementState($ipEnablementState)
  {
    $this->ipEnablementState = $ipEnablementState;
  }
  /**
   * @return string
   */
  public function getIpEnablementState()
  {
    return $this->ipEnablementState;
  }
  /**
   * @param string[]
   */
  public function setProvisionedRegions($provisionedRegions)
  {
    $this->provisionedRegions = $provisionedRegions;
  }
  /**
   * @return string[]
   */
  public function getProvisionedRegions()
  {
    return $this->provisionedRegions;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudIntegrationsV1alphaProjectProperties::class, 'Google_Service_Integrations_GoogleCloudIntegrationsV1alphaProjectProperties');
