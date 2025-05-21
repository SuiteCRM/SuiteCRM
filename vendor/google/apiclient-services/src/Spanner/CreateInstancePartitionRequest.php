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

namespace Google\Service\Spanner;

class CreateInstancePartitionRequest extends \Google\Model
{
  protected $instancePartitionType = InstancePartition::class;
  protected $instancePartitionDataType = '';
  /**
   * @var string
   */
  public $instancePartitionId;

  /**
   * @param InstancePartition
   */
  public function setInstancePartition(InstancePartition $instancePartition)
  {
    $this->instancePartition = $instancePartition;
  }
  /**
   * @return InstancePartition
   */
  public function getInstancePartition()
  {
    return $this->instancePartition;
  }
  /**
   * @param string
   */
  public function setInstancePartitionId($instancePartitionId)
  {
    $this->instancePartitionId = $instancePartitionId;
  }
  /**
   * @return string
   */
  public function getInstancePartitionId()
  {
    return $this->instancePartitionId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CreateInstancePartitionRequest::class, 'Google_Service_Spanner_CreateInstancePartitionRequest');
