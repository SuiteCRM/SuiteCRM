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

namespace Google\Service\Batch;

class TaskGroup extends \Google\Collection
{
  protected $collection_key = 'taskEnvironments';
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $parallelism;
  /**
   * @var bool
   */
  public $permissiveSsh;
  /**
   * @var bool
   */
  public $requireHostsFile;
  /**
   * @var bool
   */
  public $runAsNonRoot;
  /**
   * @var string
   */
  public $schedulingPolicy;
  /**
   * @var string
   */
  public $taskCount;
  /**
   * @var string
   */
  public $taskCountPerNode;
  protected $taskEnvironmentsType = Environment::class;
  protected $taskEnvironmentsDataType = 'array';
  protected $taskSpecType = TaskSpec::class;
  protected $taskSpecDataType = '';

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
  public function setParallelism($parallelism)
  {
    $this->parallelism = $parallelism;
  }
  /**
   * @return string
   */
  public function getParallelism()
  {
    return $this->parallelism;
  }
  /**
   * @param bool
   */
  public function setPermissiveSsh($permissiveSsh)
  {
    $this->permissiveSsh = $permissiveSsh;
  }
  /**
   * @return bool
   */
  public function getPermissiveSsh()
  {
    return $this->permissiveSsh;
  }
  /**
   * @param bool
   */
  public function setRequireHostsFile($requireHostsFile)
  {
    $this->requireHostsFile = $requireHostsFile;
  }
  /**
   * @return bool
   */
  public function getRequireHostsFile()
  {
    return $this->requireHostsFile;
  }
  /**
   * @param bool
   */
  public function setRunAsNonRoot($runAsNonRoot)
  {
    $this->runAsNonRoot = $runAsNonRoot;
  }
  /**
   * @return bool
   */
  public function getRunAsNonRoot()
  {
    return $this->runAsNonRoot;
  }
  /**
   * @param string
   */
  public function setSchedulingPolicy($schedulingPolicy)
  {
    $this->schedulingPolicy = $schedulingPolicy;
  }
  /**
   * @return string
   */
  public function getSchedulingPolicy()
  {
    return $this->schedulingPolicy;
  }
  /**
   * @param string
   */
  public function setTaskCount($taskCount)
  {
    $this->taskCount = $taskCount;
  }
  /**
   * @return string
   */
  public function getTaskCount()
  {
    return $this->taskCount;
  }
  /**
   * @param string
   */
  public function setTaskCountPerNode($taskCountPerNode)
  {
    $this->taskCountPerNode = $taskCountPerNode;
  }
  /**
   * @return string
   */
  public function getTaskCountPerNode()
  {
    return $this->taskCountPerNode;
  }
  /**
   * @param Environment[]
   */
  public function setTaskEnvironments($taskEnvironments)
  {
    $this->taskEnvironments = $taskEnvironments;
  }
  /**
   * @return Environment[]
   */
  public function getTaskEnvironments()
  {
    return $this->taskEnvironments;
  }
  /**
   * @param TaskSpec
   */
  public function setTaskSpec(TaskSpec $taskSpec)
  {
    $this->taskSpec = $taskSpec;
  }
  /**
   * @return TaskSpec
   */
  public function getTaskSpec()
  {
    return $this->taskSpec;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TaskGroup::class, 'Google_Service_Batch_TaskGroup');
