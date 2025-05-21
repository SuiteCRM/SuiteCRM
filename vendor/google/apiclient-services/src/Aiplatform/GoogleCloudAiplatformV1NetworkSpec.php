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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1NetworkSpec extends \Google\Model
{
  /**
   * @var bool
   */
  public $enableInternetAccess;
  /**
   * @var string
   */
  public $network;
  /**
   * @var string
   */
  public $subnetwork;

  /**
   * @param bool
   */
  public function setEnableInternetAccess($enableInternetAccess)
  {
    $this->enableInternetAccess = $enableInternetAccess;
  }
  /**
   * @return bool
   */
  public function getEnableInternetAccess()
  {
    return $this->enableInternetAccess;
  }
  /**
   * @param string
   */
  public function setNetwork($network)
  {
    $this->network = $network;
  }
  /**
   * @return string
   */
  public function getNetwork()
  {
    return $this->network;
  }
  /**
   * @param string
   */
  public function setSubnetwork($subnetwork)
  {
    $this->subnetwork = $subnetwork;
  }
  /**
   * @return string
   */
  public function getSubnetwork()
  {
    return $this->subnetwork;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1NetworkSpec::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1NetworkSpec');
