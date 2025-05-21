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

class WorkflowInvocationAction extends \Google\Model
{
  protected $bigqueryActionType = BigQueryAction::class;
  protected $bigqueryActionDataType = '';
  protected $canonicalTargetType = Target::class;
  protected $canonicalTargetDataType = '';
  /**
   * @var string
   */
  public $failureReason;
  /**
   * @var string
   */
  public $internalMetadata;
  protected $invocationTimingType = Interval::class;
  protected $invocationTimingDataType = '';
  protected $notebookActionType = NotebookAction::class;
  protected $notebookActionDataType = '';
  /**
   * @var string
   */
  public $state;
  protected $targetType = Target::class;
  protected $targetDataType = '';

  /**
   * @param BigQueryAction
   */
  public function setBigqueryAction(BigQueryAction $bigqueryAction)
  {
    $this->bigqueryAction = $bigqueryAction;
  }
  /**
   * @return BigQueryAction
   */
  public function getBigqueryAction()
  {
    return $this->bigqueryAction;
  }
  /**
   * @param Target
   */
  public function setCanonicalTarget(Target $canonicalTarget)
  {
    $this->canonicalTarget = $canonicalTarget;
  }
  /**
   * @return Target
   */
  public function getCanonicalTarget()
  {
    return $this->canonicalTarget;
  }
  /**
   * @param string
   */
  public function setFailureReason($failureReason)
  {
    $this->failureReason = $failureReason;
  }
  /**
   * @return string
   */
  public function getFailureReason()
  {
    return $this->failureReason;
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
   * @param NotebookAction
   */
  public function setNotebookAction(NotebookAction $notebookAction)
  {
    $this->notebookAction = $notebookAction;
  }
  /**
   * @return NotebookAction
   */
  public function getNotebookAction()
  {
    return $this->notebookAction;
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
   * @param Target
   */
  public function setTarget(Target $target)
  {
    $this->target = $target;
  }
  /**
   * @return Target
   */
  public function getTarget()
  {
    return $this->target;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(WorkflowInvocationAction::class, 'Google_Service_Dataform_WorkflowInvocationAction');
