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

namespace Google\Service\VMMigrationService;

class BootDiskDefaults extends \Google\Model
{
  /**
   * @var string
   */
  public $deviceName;
  /**
   * @var string
   */
  public $diskName;
  /**
   * @var string
   */
  public $diskType;
  protected $encryptionType = Encryption::class;
  protected $encryptionDataType = '';
  protected $imageType = DiskImageDefaults::class;
  protected $imageDataType = '';

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
  public function setDiskName($diskName)
  {
    $this->diskName = $diskName;
  }
  /**
   * @return string
   */
  public function getDiskName()
  {
    return $this->diskName;
  }
  /**
   * @param string
   */
  public function setDiskType($diskType)
  {
    $this->diskType = $diskType;
  }
  /**
   * @return string
   */
  public function getDiskType()
  {
    return $this->diskType;
  }
  /**
   * @param Encryption
   */
  public function setEncryption(Encryption $encryption)
  {
    $this->encryption = $encryption;
  }
  /**
   * @return Encryption
   */
  public function getEncryption()
  {
    return $this->encryption;
  }
  /**
   * @param DiskImageDefaults
   */
  public function setImage(DiskImageDefaults $image)
  {
    $this->image = $image;
  }
  /**
   * @return DiskImageDefaults
   */
  public function getImage()
  {
    return $this->image;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BootDiskDefaults::class, 'Google_Service_VMMigrationService_BootDiskDefaults');
