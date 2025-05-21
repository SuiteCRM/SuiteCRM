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

class ImageImportJob extends \Google\Collection
{
  protected $collection_key = 'warnings';
  /**
   * @var string
   */
  public $cloudStorageUri;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string[]
   */
  public $createdResources;
  protected $diskImageTargetDetailsType = DiskImageTargetDetails::class;
  protected $diskImageTargetDetailsDataType = '';
  /**
   * @var string
   */
  public $endTime;
  protected $errorsType = Status::class;
  protected $errorsDataType = 'array';
  protected $machineImageTargetDetailsType = MachineImageTargetDetails::class;
  protected $machineImageTargetDetailsDataType = '';
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $state;
  protected $stepsType = ImageImportStep::class;
  protected $stepsDataType = 'array';
  protected $warningsType = MigrationWarning::class;
  protected $warningsDataType = 'array';

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
   * @param string[]
   */
  public function setCreatedResources($createdResources)
  {
    $this->createdResources = $createdResources;
  }
  /**
   * @return string[]
   */
  public function getCreatedResources()
  {
    return $this->createdResources;
  }
  /**
   * @param DiskImageTargetDetails
   */
  public function setDiskImageTargetDetails(DiskImageTargetDetails $diskImageTargetDetails)
  {
    $this->diskImageTargetDetails = $diskImageTargetDetails;
  }
  /**
   * @return DiskImageTargetDetails
   */
  public function getDiskImageTargetDetails()
  {
    return $this->diskImageTargetDetails;
  }
  /**
   * @param string
   */
  public function setEndTime($endTime)
  {
    $this->endTime = $endTime;
  }
  /**
   * @return string
   */
  public function getEndTime()
  {
    return $this->endTime;
  }
  /**
   * @param Status[]
   */
  public function setErrors($errors)
  {
    $this->errors = $errors;
  }
  /**
   * @return Status[]
   */
  public function getErrors()
  {
    return $this->errors;
  }
  /**
   * @param MachineImageTargetDetails
   */
  public function setMachineImageTargetDetails(MachineImageTargetDetails $machineImageTargetDetails)
  {
    $this->machineImageTargetDetails = $machineImageTargetDetails;
  }
  /**
   * @return MachineImageTargetDetails
   */
  public function getMachineImageTargetDetails()
  {
    return $this->machineImageTargetDetails;
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
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param ImageImportStep[]
   */
  public function setSteps($steps)
  {
    $this->steps = $steps;
  }
  /**
   * @return ImageImportStep[]
   */
  public function getSteps()
  {
    return $this->steps;
  }
  /**
   * @param MigrationWarning[]
   */
  public function setWarnings($warnings)
  {
    $this->warnings = $warnings;
  }
  /**
   * @return MigrationWarning[]
   */
  public function getWarnings()
  {
    return $this->warnings;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ImageImportJob::class, 'Google_Service_VMMigrationService_ImageImportJob');
