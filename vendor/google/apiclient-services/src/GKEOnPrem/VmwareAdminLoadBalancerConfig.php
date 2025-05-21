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

class VmwareAdminLoadBalancerConfig extends \Google\Model
{
  protected $f5ConfigType = VmwareAdminF5BigIpConfig::class;
  protected $f5ConfigDataType = '';
  protected $manualLbConfigType = VmwareAdminManualLbConfig::class;
  protected $manualLbConfigDataType = '';
  protected $metalLbConfigType = VmwareAdminMetalLbConfig::class;
  protected $metalLbConfigDataType = '';
  protected $seesawConfigType = VmwareAdminSeesawConfig::class;
  protected $seesawConfigDataType = '';
  protected $vipConfigType = VmwareAdminVipConfig::class;
  protected $vipConfigDataType = '';

  /**
   * @param VmwareAdminF5BigIpConfig
   */
  public function setF5Config(VmwareAdminF5BigIpConfig $f5Config)
  {
    $this->f5Config = $f5Config;
  }
  /**
   * @return VmwareAdminF5BigIpConfig
   */
  public function getF5Config()
  {
    return $this->f5Config;
  }
  /**
   * @param VmwareAdminManualLbConfig
   */
  public function setManualLbConfig(VmwareAdminManualLbConfig $manualLbConfig)
  {
    $this->manualLbConfig = $manualLbConfig;
  }
  /**
   * @return VmwareAdminManualLbConfig
   */
  public function getManualLbConfig()
  {
    return $this->manualLbConfig;
  }
  /**
   * @param VmwareAdminMetalLbConfig
   */
  public function setMetalLbConfig(VmwareAdminMetalLbConfig $metalLbConfig)
  {
    $this->metalLbConfig = $metalLbConfig;
  }
  /**
   * @return VmwareAdminMetalLbConfig
   */
  public function getMetalLbConfig()
  {
    return $this->metalLbConfig;
  }
  /**
   * @param VmwareAdminSeesawConfig
   */
  public function setSeesawConfig(VmwareAdminSeesawConfig $seesawConfig)
  {
    $this->seesawConfig = $seesawConfig;
  }
  /**
   * @return VmwareAdminSeesawConfig
   */
  public function getSeesawConfig()
  {
    return $this->seesawConfig;
  }
  /**
   * @param VmwareAdminVipConfig
   */
  public function setVipConfig(VmwareAdminVipConfig $vipConfig)
  {
    $this->vipConfig = $vipConfig;
  }
  /**
   * @return VmwareAdminVipConfig
   */
  public function getVipConfig()
  {
    return $this->vipConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(VmwareAdminLoadBalancerConfig::class, 'Google_Service_GKEOnPrem_VmwareAdminLoadBalancerConfig');
