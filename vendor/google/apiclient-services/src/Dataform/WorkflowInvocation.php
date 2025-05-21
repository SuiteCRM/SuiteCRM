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

namespace Google\Service\Dataform;

class WorkflowInvocation extends \Google\Model
{
  /**
   * @var string
   */
  public $compilationResult;
  protected $dataEncryptionStateType = DataEncryptionState::class;
  protected $dataEncryptionStateDataType = '';
  /**
   * @var string
   */
  public $internalMetadata;
  protected $invocationConfigType = InvocationConfig::class;
  protected $invocationConfigDataType = '';
  protected $invocationTimingType = Interval::class;
  protected $invocationTimingDataType = '';
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $resolvedCompilationResult;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $workflowConfig;

  /**
   * @param string
   */
  public function setCompilationResult($compilationResult)
  {
    $this->compilationResult = $compilationResult;
  }
  /**
   * @return string
   */
  public function getCompilationResult()
  {
    return $this->compilationResult;
  }
  /**
   * @param DataEncryptionState
   */
  public function setDataEncryptionState(DataEncryptionState $dataEncryptionState)
  {
    $this->dataEncryptionState = $dataEncryptionState;
  }
  /**
   * @return DataEncryptionState
   */
  public function getDataEncryptionState()
  {
    return $this->dataEncryptionState;
  }
  /**
   * @param string
   */
  public function setInternalMetadata($internalMetadata)
  {
    $this->internalMetadata = $internalMetadata;
  }
  /**
   * @return string
   */
  public function getInternalMetadata()
  {
    return $this->internalMetadata;
  }
  /**
   * @param InvocationConfig
   */
  public function setInvocationConfig(InvocationConfig $invocationConfig)
  {
    $this->invocationConfig = $invocationConfig;
  }
  /**
   * @return InvocationConfig
   */
  public function getInvocationConfig()
  {
    return $this->invocationConfig;
  }
  /**
   * @param Interval
   */
  public function setInvocationTiming(Interval $invocationTiming)
  {
    $this->invocationTiming = $invocationTiming;
  }
  /**
   * @return Interval
   */
  public function getInvocationTiming()
  {
    return $this->invocationTiming;
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
  public function setResolvedCompilationResult($resolvedCompilationResult)
  {
    $this->resolvedCompilationResult = $resolvedCompilationResult;
  }
  /**
   * @return string
   */
  public function getResolvedCompilationResult()
  {
    return $this->resolvedCompilationResult;
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
  public function setWorkflowConfig($workflowConfig)
  {
    $this->workflowConfig = $workflowConfig;
  }
  /**
   * @return string
   */
  public function getWorkflowConfig()
  {
    return $this->workflowConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(WorkflowInvocation::class, 'Google_Service_Dataform_WorkflowInvocation');
