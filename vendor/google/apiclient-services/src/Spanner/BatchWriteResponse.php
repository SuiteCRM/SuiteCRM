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

class BatchWriteResponse extends \Google\Collection
{
  protected $collection_key = 'indexes';
  /**
   * @var string
   */
  public $commitTimestamp;
  /**
   * @var int[]
   */
  public $indexes;
  protected $statusType = Status::class;
  protected $statusDataType = '';

  /**
   * @param string
   */
  public function setCommitTimestamp($commitTimestamp)
  {
    $this->commitTimestamp = $commitTimestamp;
  }
  /**
   * @return string
   */
  public function getCommitTimestamp()
  {
    return $this->commitTimestamp;
  }
  /**
   * @param int[]
   */
  public function setIndexes($indexes)
  {
    $this->indexes = $indexes;
  }
  /**
   * @return int[]
   */
  public function getIndexes()
  {
    return $this->indexes;
  }
  /**
   * @param Status
   */
  public function setStatus(Status $status)
  {
    $this->status = $status;
  }
  /**
   * @return Status
   */
  public function getStatus()
  {
    return $this->status;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BatchWriteResponse::class, 'Google_Service_Spanner_BatchWriteResponse');
