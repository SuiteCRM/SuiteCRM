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

class Callback extends \Google\Collection
{
  protected $collection_key = 'availablePayloads';
  /**
   * @var string[]
   */
  public $availablePayloads;
  /**
   * @var string
   */
  public $method;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $waiters;

  /**
   * @param string[]
   */
  public function setAvailablePayloads($availablePayloads)
  {
    $this->availablePayloads = $availablePayloads;
  }
  /**
   * @return string[]
   */
  public function getAvailablePayloads()
  {
    return $this->availablePayloads;
  }
  /**
   * @param string
   */
  public function setMethod($method)
  {
    $this->method = $method;
  }
  /**
   * @return string
   */
  public function getMethod()
  {
    return $this->method;
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
  public function setWaiters($waiters)
  {
    $this->waiters = $waiters;
  }
  /**
   * @return string
   */
  public function getWaiters()
  {
    return $this->waiters;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Callback::class, 'Google_Service_WorkflowExecutions_Callback');
