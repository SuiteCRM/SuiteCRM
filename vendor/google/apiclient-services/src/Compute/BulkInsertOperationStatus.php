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

namespace Google\Service\Compute;

class BulkInsertOperationStatus extends \Google\Model
{
  /**
   * @var int
   */
  public $createdVmCount;
  /**
   * @var int
   */
  public $deletedVmCount;
  /**
   * @var int
   */
  public $failedToCreateVmCount;
  /**
   * @var string
   */
  public $status;
  /**
   * @var int
   */
  public $targetVmCount;

  /**
   * @param int
   */
  public function setCreatedVmCount($createdVmCount)
  {
    $this->createdVmCount = $createdVmCount;
  }
  /**
   * @return int
   */
  public function getCreatedVmCount()
  {
    return $this->createdVmCount;
  }
  /**
   * @param int
   */
  public function setDeletedVmCount($deletedVmCount)
  {
    $this->deletedVmCount = $deletedVmCount;
  }
  /**
   * @return int
   */
  public function getDeletedVmCount()
  {
    return $this->deletedVmCount;
  }
  /**
   * @param int
   */
  public function setFailedToCreateVmCount($failedToCreateVmCount)
  {
    $this->failedToCreateVmCount = $failedToCreateVmCount;
  }
  /**
   * @return int
   */
  public function getFailedToCreateVmCount()
  {
    return $this->failedToCreateVmCount;
  }
  /**
   * @param string
   */
  public function setStatus($status)
  {
    $this->status = $status;
  }
  /**
   * @return string
   */
  public function getStatus()
  {
    return $this->status;
  }
  /**
   * @param int
   */
  public function setTargetVmCount($targetVmCount)
  {
    $this->targetVmCount = $targetVmCount;
  }
  /**
   * @return int
   */
  public function getTargetVmCount()
  {
    return $this->targetVmCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BulkInsertOperationStatus::class, 'Google_Service_Compute_BulkInsertOperationStatus');
