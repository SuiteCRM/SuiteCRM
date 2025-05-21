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

namespace Google\Service\WorkloadManager;

class SapDiscoveryResourceInstanceProperties extends \Google\Collection
{
  protected $collection_key = 'diskMounts';
  protected $appInstancesType = SapDiscoveryResourceInstancePropertiesAppInstance::class;
  protected $appInstancesDataType = 'array';
  /**
   * @var string[]
   */
  public $clusterInstances;
  protected $diskMountsType = SapDiscoveryResourceInstancePropertiesDiskMount::class;
  protected $diskMountsDataType = 'array';
  /**
   * @var string
   */
  public $instanceNumber;
  /**
   * @var string
   */
  public $instanceRole;
  /**
   * @var bool
   */
  public $isDrSite;
  /**
   * @var string
   */
  public $virtualHostname;

  /**
   * @param SapDiscoveryResourceInstancePropertiesAppInstance[]
   */
  public function setAppInstances($appInstances)
  {
    $this->appInstances = $appInstances;
  }
  /**
   * @return SapDiscoveryResourceInstancePropertiesAppInstance[]
   */
  public function getAppInstances()
  {
    return $this->appInstances;
  }
  /**
   * @param string[]
   */
  public function setClusterInstances($clusterInstances)
  {
    $this->clusterInstances = $clusterInstances;
  }
  /**
   * @return string[]
   */
  public function getClusterInstances()
  {
    return $this->clusterInstances;
  }
  /**
   * @param SapDiscoveryResourceInstancePropertiesDiskMount[]
   */
  public function setDiskMounts($diskMounts)
  {
    $this->diskMounts = $diskMounts;
  }
  /**
   * @return SapDiscoveryResourceInstancePropertiesDiskMount[]
   */
  public function getDiskMounts()
  {
    return $this->diskMounts;
  }
  /**
   * @param string
   */
  public function setInstanceNumber($instanceNumber)
  {
    $this->instanceNumber = $instanceNumber;
  }
  /**
   * @return string
   */
  public function getInstanceNumber()
  {
    return $this->instanceNumber;
  }
  /**
   * @param string
   */
  public function setInstanceRole($instanceRole)
  {
    $this->instanceRole = $instanceRole;
  }
  /**
   * @return string
   */
  public function getInstanceRole()
  {
    return $this->instanceRole;
  }
  /**
   * @param bool
   */
  public function setIsDrSite($isDrSite)
  {
    $this->isDrSite = $isDrSite;
  }
  /**
   * @return bool
   */
  public function getIsDrSite()
  {
    return $this->isDrSite;
  }
  /**
   * @param string
   */
  public function setVirtualHostname($virtualHostname)
  {
    $this->virtualHostname = $virtualHostname;
  }
  /**
   * @return string
   */
  public function getVirtualHostname()
  {
    return $this->virtualHostname;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SapDiscoveryResourceInstanceProperties::class, 'Google_Service_WorkloadManager_SapDiscoveryResourceInstanceProperties');
