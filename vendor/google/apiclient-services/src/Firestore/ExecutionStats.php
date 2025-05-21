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

namespace Google\Service\Firestore;

class ExecutionStats extends \Google\Model
{
  /**
   * @var array[]
   */
  public $debugStats;
  /**
   * @var string
   */
  public $executionDuration;
  /**
   * @var string
   */
  public $readOperations;
  /**
   * @var string
   */
  public $resultsReturned;

  /**
   * @param array[]
   */
  public function setDebugStats($debugStats)
  {
    $this->debugStats = $debugStats;
  }
  /**
   * @return array[]
   */
  public function getDebugStats()
  {
    return $this->debugStats;
  }
  /**
   * @param string
   */
  public function setExecutionDuration($executionDuration)
  {
    $this->executionDuration = $executionDuration;
  }
  /**
   * @return string
   */
  public function getExecutionDuration()
  {
    return $this->executionDuration;
  }
  /**
   * @param string
   */
  public function setReadOperations($readOperations)
  {
    $this->readOperations = $readOperations;
  }
  /**
   * @return string
   */
  public function getReadOperations()
  {
    return $this->readOperations;
  }
  /**
   * @param string
   */
  public function setResultsReturned($resultsReturned)
  {
    $this->resultsReturned = $resultsReturned;
  }
  /**
   * @return string
   */
  public function getResultsReturned()
  {
    return $this->resultsReturned;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ExecutionStats::class, 'Google_Service_Firestore_ExecutionStats');
