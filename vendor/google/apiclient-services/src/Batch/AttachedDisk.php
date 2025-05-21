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

class AttachedDisk extends \Google\Model
{
  /**
   * @var string
   */
  public $deviceName;
  /**
   * @var string
   */
  public $existingDisk;
  protected $newDiskType = Disk::class;
  protected $newDiskDataType = '';

  /**
   * @param string
   */
  public function setDeviceName($deviceName)
  {
    $this->deviceName = $deviceName;
  }
  /**
   * @return string
   */
  public function getDeviceName()
  {
    return $this->deviceName;
  }
  /**
   * @param string
   */
  public function setExistingDisk($existingDisk)
  {
    $this->existingDisk = $existingDisk;
  }
  /**
   * @return string
   */
  public function getExistingDisk()
  {
    return $this->existingDisk;
  }
  /**
   * @param Disk
   */
  public function setNewDisk(Disk $newDisk)
  {
    $this->newDisk = $newDisk;
  }
  /**
   * @return Disk
   */
  public function getNewDisk()
  {
    return $this->newDisk;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AttachedDisk::class, 'Google_Service_Batch_AttachedDisk');
