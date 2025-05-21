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

class CutoverStep extends \Google\Model
{
  /**
   * @var string
   */
  public $endTime;
  protected $finalSyncType = ReplicationCycle::class;
  protected $finalSyncDataType = '';
  protected $instantiatingMigratedVmType = InstantiatingMigratedVMStep::class;
  protected $instantiatingMigratedVmDataType = '';
  protected $preparingVmDisksType = PreparingVMDisksStep::class;
  protected $preparingVmDisksDataType = '';
  protected $previousReplicationCycleType = ReplicationCycle::class;
  protected $previousReplicationCycleDataType = '';
  protected $shuttingDownSourceVmType = ShuttingDownSourceVMStep::class;
  protected $shuttingDownSourceVmDataType = '';
  /**
   * @var string
   */
  public $startTime;

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
   * @param ReplicationCycle
   */
  public function setFinalSync(ReplicationCycle $finalSync)
  {
    $this->finalSync = $finalSync;
  }
  /**
   * @return ReplicationCycle
   */
  public function getFinalSync()
  {
    return $this->finalSync;
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
   * @param ReplicationCycle
   */
  public function setPreviousReplicationCycle(ReplicationCycle $previousReplicationCycle)
  {
    $this->previousReplicationCycle = $previousReplicationCycle;
  }
  /**
   * @return ReplicationCycle
   */
  public function getPreviousReplicationCycle()
  {
    return $this->previousReplicationCycle;
  }
  /**
   * @param ShuttingDownSourceVMStep
   */
  public function setShuttingDownSourceVm(ShuttingDownSourceVMStep $shuttingDownSourceVm)
  {
    $this->shuttingDownSourceVm = $shuttingDownSourceVm;
  }
  /**
   * @return ShuttingDownSourceVMStep
   */
  public function getShuttingDownSourceVm()
  {
    return $this->shuttingDownSourceVm;
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
class_alias(CutoverStep::class, 'Google_Service_VMMigrationService_CutoverStep');
