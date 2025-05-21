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

class CloneStep extends \Google\Model
{
  protected $adaptingOsType = AdaptingOSStep::class;
  protected $adaptingOsDataType = '';
  /**
   * @var string
   */
  public $endTime;
  protected $instantiatingMigratedVmType = InstantiatingMigratedVMStep::class;
  protected $instantiatingMigratedVmDataType = '';
  protected $preparingVmDisksType = PreparingVMDisksStep::class;
  protected $preparingVmDisksDataType = '';
  /**
   * @var string
   */
  public $startTime;

  /**
   * @param AdaptingOSStep
   */
  public function setAdaptingOs(AdaptingOSStep $adaptingOs)
  {
    $this->adaptingOs = $adaptingOs;
  }
  /**
   * @return AdaptingOSStep
   */
  public function getAdaptingOs()
  {
    return $this->adaptingOs;
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
   * @param InstantiatingMigratedVMStep
   */
  public function setInstantiatingMigratedVm(InstantiatingMigratedVMStep $instantiatingMigratedVm)
  {
    $this->instantiatingMigratedVm = $instantiatingMigratedVm;
  }
  /**
   * @return InstantiatingMigratedVMStep
   */
  public function getInstantiatingMigratedVm()
  {
    return $this->instantiatingMigratedVm;
  }
  /**
   * @param PreparingVMDisksStep
   */
  public function setPreparingVmDisks(PreparingVMDisksStep $preparingVmDisks)
  {
    $this->preparingVmDisks = $preparingVmDisks;
  }
  /**
   * @return PreparingVMDisksStep
   */
  public function getPreparingVmDisks()
  {
    return $this->preparingVmDisks;
  }
  /**
   * @param string
   */
  public function setStartTime($startTime)
  {
    $this->startTime = $startTime;
  }
  /**
   * @return string
   */
  public function getStartTime()
  {
    return $this->startTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CloneStep::class, 'Google_Service_VMMigrationService_CloneStep');
