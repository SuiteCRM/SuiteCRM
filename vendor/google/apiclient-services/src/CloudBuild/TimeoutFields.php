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

class TimeoutFields extends \Google\Model
{
  /**
   * @var string
   */
  public $finally;
  /**
   * @var string
   */
  public $pipeline;
  /**
   * @var string
   */
  public $tasks;

  /**
   * @param string
   */
  public function setFinally($finally)
  {
    $this->finally = $finally;
  }
  /**
   * @return string
   */
  public function getFinally()
  {
    return $this->finally;
  }
  /**
   * @param string
   */
  public function setPipeline($pipeline)
  {
    $this->pipeline = $pipeline;
  }
  /**
   * @return string
   */
  public function getPipeline()
  {
    return $this->pipeline;
  }
  /**
   * @param string
   */
  public function setTasks($tasks)
  {
    $this->tasks = $tasks;
  }
  /**
   * @return string
   */
  public function getTasks()
  {
    return $this->tasks;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TimeoutFields::class, 'Google_Service_CloudBuild_TimeoutFields');
