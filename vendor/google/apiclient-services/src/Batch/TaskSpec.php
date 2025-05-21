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

class TaskSpec extends \Google\Collection
{
  protected $collection_key = 'volumes';
  protected $computeResourceType = ComputeResource::class;
  protected $computeResourceDataType = '';
  protected $environmentType = Environment::class;
  protected $environmentDataType = '';
  /**
   * @var string[]
   */
  public $environments;
  protected $lifecyclePoliciesType = LifecyclePolicy::class;
  protected $lifecyclePoliciesDataType = 'array';
  /**
   * @var int
   */
  public $maxRetryCount;
  /**
   * @var string
   */
  public $maxRunDuration;
  protected $runnablesType = Runnable::class;
  protected $runnablesDataType = 'array';
  protected $volumesType = Volume::class;
  protected $volumesDataType = 'array';

  /**
   * @param ComputeResource
   */
  public function setComputeResource(ComputeResource $computeResource)
  {
    $this->computeResource = $computeResource;
  }
  /**
   * @return ComputeResource
   */
  public function getComputeResource()
  {
    return $this->computeResource;
  }
  /**
   * @param Environment
   */
  public function setEnvironment(Environment $environment)
  {
    $this->environment = $environment;
  }
  /**
   * @return Environment
   */
  public function getEnvironment()
  {
    return $this->environment;
  }
  /**
   * @param string[]
   */
  public function setEnvironments($environments)
  {
    $this->environments = $environments;
  }
  /**
   * @return string[]
   */
  public function getEnvironments()
  {
    return $this->environments;
  }
  /**
   * @param LifecyclePolicy[]
   */
  public function setLifecyclePolicies($lifecyclePolicies)
  {
    $this->lifecyclePolicies = $lifecyclePolicies;
  }
  /**
   * @return LifecyclePolicy[]
   */
  public function getLifecyclePolicies()
  {
    return $this->lifecyclePolicies;
  }
  /**
   * @param int
   */
  public function setMaxRetryCount($maxRetryCount)
  {
    $this->maxRetryCount = $maxRetryCount;
  }
  /**
   * @return int
   */
  public function getMaxRetryCount()
  {
    return $this->maxRetryCount;
  }
  /**
   * @param string
   */
  public function setMaxRunDuration($maxRunDuration)
  {
    $this->maxRunDuration = $maxRunDuration;
  }
  /**
   * @return string
   */
  public function getMaxRunDuration()
  {
    return $this->maxRunDuration;
  }
  /**
   * @param Runnable[]
   */
  public function setRunnables($runnables)
  {
    $this->runnables = $runnables;
  }
  /**
   * @return Runnable[]
   */
  public function getRunnables()
  {
    return $this->runnables;
  }
  /**
   * @param Volume[]
   */
  public function setVolumes($volumes)
  {
    $this->volumes = $volumes;
  }
  /**
   * @return Volume[]
   */
  public function getVolumes()
  {
    return $this->volumes;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TaskSpec::class, 'Google_Service_Batch_TaskSpec');
