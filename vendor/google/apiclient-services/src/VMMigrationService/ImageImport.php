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

class ImageImport extends \Google\Collection
{
  protected $collection_key = 'recentImageImportJobs';
  /**
   * @var string
   */
  public $cloudStorageUri;
  /**
   * @var string
   */
  public $createTime;
  protected $diskImageTargetDefaultsType = DiskImageTargetDetails::class;
  protected $diskImageTargetDefaultsDataType = '';
  protected $encryptionType = Encryption::class;
  protected $encryptionDataType = '';
  protected $machineImageTargetDefaultsType = MachineImageTargetDetails::class;
  protected $machineImageTargetDefaultsDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $recentImageImportJobsType = ImageImportJob::class;
  protected $recentImageImportJobsDataType = 'array';

  /**
   * @param string
   */
  public function setCloudStorageUri($cloudStorageUri)
  {
    $this->cloudStorageUri = $cloudStorageUri;
  }
  /**
   * @return string
   */
  public function getCloudStorageUri()
  {
    return $this->cloudStorageUri;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param DiskImageTargetDetails
   */
  public function setDiskImageTargetDefaults(DiskImageTargetDetails $diskImageTargetDefaults)
  {
    $this->diskImageTargetDefaults = $diskImageTargetDefaults;
  }
  /**
   * @return DiskImageTargetDetails
   */
  public function getDiskImageTargetDefaults()
  {
    return $this->diskImageTargetDefaults;
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
   * @param MachineImageTargetDetails
   */
  public function setMachineImageTargetDefaults(MachineImageTargetDetails $machineImageTargetDefaults)
  {
    $this->machineImageTargetDefaults = $machineImageTargetDefaults;
  }
  /**
   * @return MachineImageTargetDetails
   */
  public function getMachineImageTargetDefaults()
  {
    return $this->machineImageTargetDefaults;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param ImageImportJob[]
   */
  public function setRecentImageImportJobs($recentImageImportJobs)
  {
    $this->recentImageImportJobs = $recentImageImportJobs;
  }
  /**
   * @return ImageImportJob[]
   */
  public function getRecentImageImportJobs()
  {
    return $this->recentImageImportJobs;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ImageImport::class, 'Google_Service_VMMigrationService_ImageImport');
