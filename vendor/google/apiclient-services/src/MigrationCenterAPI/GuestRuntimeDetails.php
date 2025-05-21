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

namespace Google\Service\MigrationCenterAPI;

class GuestRuntimeDetails extends \Google\Model
{
  /**
   * @var string
   */
  public $domain;
  protected $installedAppsType = GuestInstalledApplicationList::class;
  protected $installedAppsDataType = '';
  /**
   * @var string
   */
  public $lastBootTime;
  /**
   * @var string
   */
  public $machineName;
  protected $networkType = RuntimeNetworkInfo::class;
  protected $networkDataType = '';
  protected $openFileListType = OpenFileList::class;
  protected $openFileListDataType = '';
  protected $processesType = RunningProcessList::class;
  protected $processesDataType = '';
  protected $servicesType = RunningServiceList::class;
  protected $servicesDataType = '';

  /**
   * @param string
   */
  public function setDomain($domain)
  {
    $this->domain = $domain;
  }
  /**
   * @return string
   */
  public function getDomain()
  {
    return $this->domain;
  }
  /**
   * @param GuestInstalledApplicationList
   */
  public function setInstalledApps(GuestInstalledApplicationList $installedApps)
  {
    $this->installedApps = $installedApps;
  }
  /**
   * @return GuestInstalledApplicationList
   */
  public function getInstalledApps()
  {
    return $this->installedApps;
  }
  /**
   * @param string
   */
  public function setLastBootTime($lastBootTime)
  {
    $this->lastBootTime = $lastBootTime;
  }
  /**
   * @return string
   */
  public function getLastBootTime()
  {
    return $this->lastBootTime;
  }
  /**
   * @param string
   */
  public function setMachineName($machineName)
  {
    $this->machineName = $machineName;
  }
  /**
   * @return string
   */
  public function getMachineName()
  {
    return $this->machineName;
  }
  /**
   * @param RuntimeNetworkInfo
   */
  public function setNetwork(RuntimeNetworkInfo $network)
  {
    $this->network = $network;
  }
  /**
   * @return RuntimeNetworkInfo
   */
  public function getNetwork()
  {
    return $this->network;
  }
  /**
   * @param OpenFileList
   */
  public function setOpenFileList(OpenFileList $openFileList)
  {
    $this->openFileList = $openFileList;
  }
  /**
   * @return OpenFileList
   */
  public function getOpenFileList()
  {
    return $this->openFileList;
  }
  /**
   * @param RunningProcessList
   */
  public function setProcesses(RunningProcessList $processes)
  {
    $this->processes = $processes;
  }
  /**
   * @return RunningProcessList
   */
  public function getProcesses()
  {
    return $this->processes;
  }
  /**
   * @param RunningServiceList
   */
  public function setServices(RunningServiceList $services)
  {
    $this->services = $services;
  }
  /**
   * @return RunningServiceList
   */
  public function getServices()
  {
    return $this->services;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GuestRuntimeDetails::class, 'Google_Service_MigrationCenterAPI_GuestRuntimeDetails');
