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

namespace Google\Service\Batch;

class InstancePolicy extends \Google\Collection
{
  protected $collection_key = 'disks';
  protected $acceleratorsType = Accelerator::class;
  protected $acceleratorsDataType = 'array';
  protected $bootDiskType = Disk::class;
  protected $bootDiskDataType = '';
  protected $disksType = AttachedDisk::class;
  protected $disksDataType = 'array';
  /**
   * @var string
   */
  public $machineType;
  /**
   * @var string
   */
  public $minCpuPlatform;
  /**
   * @var string
   */
  public $provisioningModel;
  /**
   * @var string
   */
  public $reservation;

  /**
   * @param Accelerator[]
   */
  public function setAccelerators($accelerators)
  {
    $this->accelerators = $accelerators;
  }
  /**
   * @return Accelerator[]
   */
  public function getAccelerators()
  {
    return $this->accelerators;
  }
  /**
   * @param Disk
   */
  public function setBootDisk(Disk $bootDisk)
  {
    $this->bootDisk = $bootDisk;
  }
  /**
   * @return Disk
   */
  public function getBootDisk()
  {
    return $this->bootDisk;
  }
  /**
   * @param AttachedDisk[]
   */
  public function setDisks($disks)
  {
    $this->disks = $disks;
  }
  /**
   * @return AttachedDisk[]
   */
  public function getDisks()
  {
    return $this->disks;
  }
  /**
   * @param string
   */
  public function setMachineType($machineType)
  {
    $this->machineType = $machineType;
  }
  /**
   * @return string
   */
  public function getMachineType()
  {
    return $this->machineType;
  }
  /**
   * @param string
   */
  public function setMinCpuPlatform($minCpuPlatform)
  {
    $this->minCpuPlatform = $minCpuPlatform;
  }
  /**
   * @return string
   */
  public function getMinCpuPlatform()
  {
    return $this->minCpuPlatform;
  }
  /**
   * @param string
   */
  public function setProvisioningModel($provisioningModel)
  {
    $this->provisioningModel = $provisioningModel;
  }
  /**
   * @return string
   */
  public function getProvisioningModel()
  {
    return $this->provisioningModel;
  }
  /**
   * @param string
   */
  public function setReservation($reservation)
  {
    $this->reservation = $reservation;
  }
  /**
   * @return string
   */
  public function getReservation()
  {
    return $this->reservation;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(InstancePolicy::class, 'Google_Service_Batch_InstancePolicy');
