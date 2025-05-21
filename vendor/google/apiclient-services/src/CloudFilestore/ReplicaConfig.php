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

namespace Google\Service\CloudFilestore;

class ReplicaConfig extends \Google\Collection
{
  protected $collection_key = 'stateReasons';
  /**
   * @var string
   */
  public $lastActiveSyncTime;
  /**
   * @var string
   */
  public $peerInstance;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string[]
   */
  public $stateReasons;

  /**
   * @param string
   */
  public function setLastActiveSyncTime($lastActiveSyncTime)
  {
    $this->lastActiveSyncTime = $lastActiveSyncTime;
  }
  /**
   * @return string
   */
  public function getLastActiveSyncTime()
  {
    return $this->lastActiveSyncTime;
  }
  /**
   * @param string
   */
  public function setPeerInstance($peerInstance)
  {
    $this->peerInstance = $peerInstance;
  }
  /**
   * @return string
   */
  public function getPeerInstance()
  {
    return $this->peerInstance;
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
   * @param string[]
   */
  public function setStateReasons($stateReasons)
  {
    $this->stateReasons = $stateReasons;
  }
  /**
   * @return string[]
   */
  public function getStateReasons()
  {
    return $this->stateReasons;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ReplicaConfig::class, 'Google_Service_CloudFilestore_ReplicaConfig');
