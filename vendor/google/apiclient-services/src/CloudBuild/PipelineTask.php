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

namespace Google\Service\CloudBuild;

class PipelineTask extends \Google\Collection
{
  protected $collection_key = 'workspaces';
  /**
   * @var string
   */
  public $name;
  protected $paramsType = Param::class;
  protected $paramsDataType = 'array';
  /**
   * @var int
   */
  public $retries;
  /**
   * @var string[]
   */
  public $runAfter;
  protected $taskRefType = TaskRef::class;
  protected $taskRefDataType = '';
  protected $taskSpecType = EmbeddedTask::class;
  protected $taskSpecDataType = '';
  /**
   * @var string
   */
  public $timeout;
  protected $whenExpressionsType = WhenExpression::class;
  protected $whenExpressionsDataType = 'array';
  protected $workspacesType = WorkspacePipelineTaskBinding::class;
  protected $workspacesDataType = 'array';

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
   * @param Param[]
   */
  public function setParams($params)
  {
    $this->params = $params;
  }
  /**
   * @return Param[]
   */
  public function getParams()
  {
    return $this->params;
  }
  /**
   * @param int
   */
  public function setRetries($retries)
  {
    $this->retries = $retries;
  }
  /**
   * @return int
   */
  public function getRetries()
  {
    return $this->retries;
  }
  /**
   * @param string[]
   */
  public function setRunAfter($runAfter)
  {
    $this->runAfter = $runAfter;
  }
  /**
   * @return string[]
   */
  public function getRunAfter()
  {
    return $this->runAfter;
  }
  /**
   * @param TaskRef
   */
  public function setTaskRef(TaskRef $taskRef)
  {
    $this->taskRef = $taskRef;
  }
  /**
   * @return TaskRef
   */
  public function getTaskRef()
  {
    return $this->taskRef;
  }
  /**
   * @param EmbeddedTask
   */
  public function setTaskSpec(EmbeddedTask $taskSpec)
  {
    $this->taskSpec = $taskSpec;
  }
  /**
   * @return EmbeddedTask
   */
  public function getTaskSpec()
  {
    return $this->taskSpec;
  }
  /**
   * @param string
   */
  public function setTimeout($timeout)
  {
    $this->timeout = $timeout;
  }
  /**
   * @return string
   */
  public function getTimeout()
  {
    return $this->timeout;
  }
  /**
   * @param WhenExpression[]
   */
  public function setWhenExpressions($whenExpressions)
  {
    $this->whenExpressions = $whenExpressions;
  }
  /**
   * @return WhenExpression[]
   */
  public function getWhenExpressions()
  {
    return $this->whenExpressions;
  }
  /**
   * @param WorkspacePipelineTaskBinding[]
   */
  public function setWorkspaces($workspaces)
  {
    $this->workspaces = $workspaces;
  }
  /**
   * @return WorkspacePipelineTaskBinding[]
   */
  public function getWorkspaces()
  {
    return $this->workspaces;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PipelineTask::class, 'Google_Service_CloudBuild_PipelineTask');
