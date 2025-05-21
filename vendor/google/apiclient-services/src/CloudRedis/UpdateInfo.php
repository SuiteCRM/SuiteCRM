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

namespace Google\Service\CloudRedis;

class UpdateInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $targetNodeType;
  /**
   * @var int
   */
  public $targetReplicaCount;
  /**
   * @var int
   */
  public $targetShardCount;

  /**
   * @param string
   */
  public function setTargetNodeType($targetNodeType)
  {
    $this->targetNodeType = $targetNodeType;
  }
  /**
   * @return string
   */
  public function getTargetNodeType()
  {
    return $this->targetNodeType;
  }
  /**
   * @param int
   */
  public function setTargetReplicaCount($targetReplicaCount)
  {
    $this->targetReplicaCount = $targetReplicaCount;
  }
  /**
   * @return int
   */
  public function getTargetReplicaCount()
  {
    return $this->targetReplicaCount;
  }
  /**
   * @param int
   */
  public function setTargetShardCount($targetShardCount)
  {
    $this->targetShardCount = $targetShardCount;
  }
  /**
   * @return int
   */
  public function getTargetShardCount()
  {
    return $this->targetShardCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(UpdateInfo::class, 'Google_Service_CloudRedis_UpdateInfo');
