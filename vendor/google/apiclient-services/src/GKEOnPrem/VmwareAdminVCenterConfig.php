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

class VmwareAdminVCenterConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $address;
  /**
   * @var string
   */
  public $caCertData;
  /**
   * @var string
   */
  public $cluster;
  /**
   * @var string
   */
  public $dataDisk;
  /**
   * @var string
   */
  public $datacenter;
  /**
   * @var string
   */
  public $datastore;
  /**
   * @var string
   */
  public $folder;
  /**
   * @var string
   */
  public $resourcePool;
  /**
   * @var string
   */
  public $storagePolicyName;

  /**
   * @param string
   */
  public function setAddress($address)
  {
    $this->address = $address;
  }
  /**
   * @return string
   */
  public function getAddress()
  {
    return $this->address;
  }
  /**
   * @param string
   */
  public function setCaCertData($caCertData)
  {
    $this->caCertData = $caCertData;
  }
  /**
   * @return string
   */
  public function getCaCertData()
  {
    return $this->caCertData;
  }
  /**
   * @param string
   */
  public function setCluster($cluster)
  {
    $this->cluster = $cluster;
  }
  /**
   * @return string
   */
  public function getCluster()
  {
    return $this->cluster;
  }
  /**
   * @param string
   */
  public function setDataDisk($dataDisk)
  {
    $this->dataDisk = $dataDisk;
  }
  /**
   * @return string
   */
  public function getDataDisk()
  {
    return $this->dataDisk;
  }
  /**
   * @param string
   */
  public function setDatacenter($datacenter)
  {
    $this->datacenter = $datacenter;
  }
  /**
   * @return string
   */
  public function getDatacenter()
  {
    return $this->datacenter;
  }
  /**
   * @param string
   */
  public function setDatastore($datastore)
  {
    $this->datastore = $datastore;
  }
  /**
   * @return string
   */
  public function getDatastore()
  {
    return $this->datastore;
  }
  /**
   * @param string
   */
  public function setFolder($folder)
  {
    $this->folder = $folder;
  }
  /**
   * @return string
   */
  public function getFolder()
  {
    return $this->folder;
  }
  /**
   * @param string
   */
  public function setResourcePool($resourcePool)
  {
    $this->resourcePool = $resourcePool;
  }
  /**
   * @return string
   */
  public function getResourcePool()
  {
    return $this->resourcePool;
  }
  /**
   * @param string
   */
  public function setStoragePolicyName($storagePolicyName)
  {
    $this->storagePolicyName = $storagePolicyName;
  }
  /**
   * @return string
   */
  public function getStoragePolicyName()
  {
    return $this->storagePolicyName;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(VmwareAdminVCenterConfig::class, 'Google_Service_GKEOnPrem_VmwareAdminVCenterConfig');
