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

namespace Google\Service\NetAppFiles;

class TransferStats extends \Google\Model
{
  /**
   * @var string
   */
  public $lagDuration;
  /**
   * @var string
   */
  public $lastTransferBytes;
  /**
   * @var string
   */
  public $lastTransferDuration;
  /**
   * @var string
   */
  public $lastTransferEndTime;
  /**
   * @var string
   */
  public $lastTransferError;
  /**
   * @var string
   */
  public $totalTransferDuration;
  /**
   * @var string
   */
  public $transferBytes;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string
   */
  public function setLagDuration($lagDuration)
  {
    $this->lagDuration = $lagDuration;
  }
  /**
   * @return string
   */
  public function getLagDuration()
  {
    return $this->lagDuration;
  }
  /**
   * @param string
   */
  public function setLastTransferBytes($lastTransferBytes)
  {
    $this->lastTransferBytes = $lastTransferBytes;
  }
  /**
   * @return string
   */
  public function getLastTransferBytes()
  {
    return $this->lastTransferBytes;
  }
  /**
   * @param string
   */
  public function setLastTransferDuration($lastTransferDuration)
  {
    $this->lastTransferDuration = $lastTransferDuration;
  }
  /**
   * @return string
   */
  public function getLastTransferDuration()
  {
    return $this->lastTransferDuration;
  }
  /**
   * @param string
   */
  public function setLastTransferEndTime($lastTransferEndTime)
  {
    $this->lastTransferEndTime = $lastTransferEndTime;
  }
  /**
   * @return string
   */
  public function getLastTransferEndTime()
  {
    return $this->lastTransferEndTime;
  }
  /**
   * @param string
   */
  public function setLastTransferError($lastTransferError)
  {
    $this->lastTransferError = $lastTransferError;
  }
  /**
   * @return string
   */
  public function getLastTransferError()
  {
    return $this->lastTransferError;
  }
  /**
   * @param string
   */
  public function setTotalTransferDuration($totalTransferDuration)
  {
    $this->totalTransferDuration = $totalTransferDuration;
  }
  /**
   * @return string
   */
  public function getTotalTransferDuration()
  {
    return $this->totalTransferDuration;
  }
  /**
   * @param string
   */
  public function setTransferBytes($transferBytes)
  {
    $this->transferBytes = $transferBytes;
  }
  /**
   * @return string
   */
  public function getTransferBytes()
  {
    return $this->transferBytes;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TransferStats::class, 'Google_Service_NetAppFiles_TransferStats');
