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

namespace Google\Service\WorkflowExecutions;

class StepEntry extends \Google\Model
{
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $entryId;
  protected $exceptionType = Exception::class;
  protected $exceptionDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $navigationInfoType = NavigationInfo::class;
  protected $navigationInfoDataType = '';
  /**
   * @var string
   */
  public $routine;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $step;
  protected $stepEntryMetadataType = StepEntryMetadata::class;
  protected $stepEntryMetadataDataType = '';
  /**
   * @var string
   */
  public $stepType;
  /**
   * @var string
   */
  public $updateTime;
  protected $variableDataType = VariableData::class;
  protected $variableDataDataType = '';

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
   * @param string
   */
  public function setEntryId($entryId)
  {
    $this->entryId = $entryId;
  }
  /**
   * @return string
   */
  public function getEntryId()
  {
    return $this->entryId;
  }
  /**
   * @param Exception
   */
  public function setException(Exception $exception)
  {
    $this->exception = $exception;
  }
  /**
   * @return Exception
   */
  public function getException()
  {
    return $this->exception;
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
   * @param NavigationInfo
   */
  public function setNavigationInfo(NavigationInfo $navigationInfo)
  {
    $this->navigationInfo = $navigationInfo;
  }
  /**
   * @return NavigationInfo
   */
  public function getNavigationInfo()
  {
    return $this->navigationInfo;
  }
  /**
   * @param string
   */
  public function setRoutine($routine)
  {
    $this->routine = $routine;
  }
  /**
   * @return string
   */
  public function getRoutine()
  {
    return $this->routine;
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
   * @param string
   */
  public function setStep($step)
  {
    $this->step = $step;
  }
  /**
   * @return string
   */
  public function getStep()
  {
    return $this->step;
  }
  /**
   * @param StepEntryMetadata
   */
  public function setStepEntryMetadata(StepEntryMetadata $stepEntryMetadata)
  {
    $this->stepEntryMetadata = $stepEntryMetadata;
  }
  /**
   * @return StepEntryMetadata
   */
  public function getStepEntryMetadata()
  {
    return $this->stepEntryMetadata;
  }
  /**
   * @param string
   */
  public function setStepType($stepType)
  {
    $this->stepType = $stepType;
  }
  /**
   * @return string
   */
  public function getStepType()
  {
    return $this->stepType;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
  /**
   * @param VariableData
   */
  public function setVariableData(VariableData $variableData)
  {
    $this->variableData = $variableData;
  }
  /**
   * @return VariableData
   */
  public function getVariableData()
  {
    return $this->variableData;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(StepEntry::class, 'Google_Service_WorkflowExecutions_StepEntry');
