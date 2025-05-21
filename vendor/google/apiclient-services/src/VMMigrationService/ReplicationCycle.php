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

class ReplicationCycle extends \Google\Collection
{
  protected $collection_key = 'warnings';
  /**
   * @var int
   */
  public $cycleNumber;
  /**
   * @var string
   */
  public $endTime;
  protected $errorType = Status::class;
  protected $errorDataType = '';
  /**
   * @var string
   */
  public $name;
  /**
   * @var int
   */
  public $progressPercent;
  /**
   * @var string
   */
  public $startTime;
  /**
   * @var string
   */
  public $state;
  protected $stepsType = CycleStep::class;
  protected $stepsDataType = 'array';
  /**
   * @var string
   */
  public $totalPauseDuration;
  protected $warningsType = MigrationWarning::class;
  protected $warningsDataType = 'array';

  /**
   * @param int
   */
  public function setCycleNumber($cycleNumber)
  {
    $this->cycleNumber = $cycleNumber;
  }
  /**
   * @return int
   */
  public function getCycleNumber()
  {
    return $this->cycleNumber;
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
   * @param Status
   */
  public function setError(Status $error)
  {
    $this->error = $error;
  }
  /**
   * @return Status
   */
  public function getError()
  {
    return $this->error;
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
   * @param int
   */
  public function setProgressPercent($progressPercent)
  {
    $this->progressPercent = $progressPercent;
  }
  /**
   * @return int
   */
  public function getProgressPercent()
  {
    return $this->progressPercent;
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
   * @param CycleStep[]
   */
  public function setSteps($steps)
  {
    $this->steps = $steps;
  }
  /**
   * @return CycleStep[]
   */
  public function getSteps()
  {
    return $this->steps;
  }
  /**
   * @param string
   */
  public function setTotalPauseDuration($totalPauseDuration)
  {
    $this->totalPauseDuration = $totalPauseDuration;
  }
  /**
   * @return string
   */
  public function getTotalPauseDuration()
  {
    return $this->totalPauseDuration;
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
class_alias(ReplicationCycle::class, 'Google_Service_VMMigrationService_ReplicationCycle');
