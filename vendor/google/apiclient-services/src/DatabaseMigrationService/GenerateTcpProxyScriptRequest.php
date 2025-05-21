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

namespace Google\Service\DatabaseMigrationService;

class GenerateTcpProxyScriptRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $vmMachineType;
  /**
   * @var string
   */
  public $vmName;
  /**
   * @var string
   */
  public $vmSubnet;
  /**
   * @var string
   */
  public $vmZone;

  /**
   * @param string
   */
  public function setVmMachineType($vmMachineType)
  {
    $this->vmMachineType = $vmMachineType;
  }
  /**
   * @return string
   */
  public function getVmMachineType()
  {
    return $this->vmMachineType;
  }
  /**
   * @param string
   */
  public function setVmName($vmName)
  {
    $this->vmName = $vmName;
  }
  /**
   * @return string
   */
  public function getVmName()
  {
    return $this->vmName;
  }
  /**
   * @param string
   */
  public function setVmSubnet($vmSubnet)
  {
    $this->vmSubnet = $vmSubnet;
  }
  /**
   * @return string
   */
  public function getVmSubnet()
  {
    return $this->vmSubnet;
  }
  /**
   * @param string
   */
  public function setVmZone($vmZone)
  {
    $this->vmZone = $vmZone;
  }
  /**
   * @return string
   */
  public function getVmZone()
  {
    return $this->vmZone;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GenerateTcpProxyScriptRequest::class, 'Google_Service_DatabaseMigrationService_GenerateTcpProxyScriptRequest');
