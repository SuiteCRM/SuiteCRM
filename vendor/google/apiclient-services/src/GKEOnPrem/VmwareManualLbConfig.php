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

namespace Google\Service\GKEOnPrem;

class VmwareManualLbConfig extends \Google\Model
{
  /**
   * @var int
   */
  public $controlPlaneNodePort;
  /**
   * @var int
   */
  public $ingressHttpNodePort;
  /**
   * @var int
   */
  public $ingressHttpsNodePort;
  /**
   * @var int
   */
  public $konnectivityServerNodePort;

  /**
   * @param int
   */
  public function setControlPlaneNodePort($controlPlaneNodePort)
  {
    $this->controlPlaneNodePort = $controlPlaneNodePort;
  }
  /**
   * @return int
   */
  public function getControlPlaneNodePort()
  {
    return $this->controlPlaneNodePort;
  }
  /**
   * @param int
   */
  public function setIngressHttpNodePort($ingressHttpNodePort)
  {
    $this->ingressHttpNodePort = $ingressHttpNodePort;
  }
  /**
   * @return int
   */
  public function getIngressHttpNodePort()
  {
    return $this->ingressHttpNodePort;
  }
  /**
   * @param int
   */
  public function setIngressHttpsNodePort($ingressHttpsNodePort)
  {
    $this->ingressHttpsNodePort = $ingressHttpsNodePort;
  }
  /**
   * @return int
   */
  public function getIngressHttpsNodePort()
  {
    return $this->ingressHttpsNodePort;
  }
  /**
   * @param int
   */
  public function setKonnectivityServerNodePort($konnectivityServerNodePort)
  {
    $this->konnectivityServerNodePort = $konnectivityServerNodePort;
  }
  /**
   * @return int
   */
  public function getKonnectivityServerNodePort()
  {
    return $this->konnectivityServerNodePort;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(VmwareManualLbConfig::class, 'Google_Service_GKEOnPrem_VmwareManualLbConfig');
