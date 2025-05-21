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

namespace Google\Service\Dataflow;

class StreamingScalingReport extends \Google\Model
{
  /**
   * @var int
   */
  public $activeBundleCount;
  /**
   * @var int
   */
  public $activeThreadCount;
  /**
   * @var int
   */
  public $maximumBundleCount;
  /**
   * @var string
   */
  public $maximumBytes;
  /**
   * @var int
   */
  public $maximumBytesCount;
  /**
   * @var int
   */
  public $maximumThreadCount;
  /**
   * @var int
   */
  public $outstandingBundleCount;
  /**
   * @var string
   */
  public $outstandingBytes;
  /**
   * @var int
   */
  public $outstandingBytesCount;

  /**
   * @param int
   */
  public function setActiveBundleCount($activeBundleCount)
  {
    $this->activeBundleCount = $activeBundleCount;
  }
  /**
   * @return int
   */
  public function getActiveBundleCount()
  {
    return $this->activeBundleCount;
  }
  /**
   * @param int
   */
  public function setActiveThreadCount($activeThreadCount)
  {
    $this->activeThreadCount = $activeThreadCount;
  }
  /**
   * @return int
   */
  public function getActiveThreadCount()
  {
    return $this->activeThreadCount;
  }
  /**
   * @param int
   */
  public function setMaximumBundleCount($maximumBundleCount)
  {
    $this->maximumBundleCount = $maximumBundleCount;
  }
  /**
   * @return int
   */
  public function getMaximumBundleCount()
  {
    return $this->maximumBundleCount;
  }
  /**
   * @param string
   */
  public function setMaximumBytes($maximumBytes)
  {
    $this->maximumBytes = $maximumBytes;
  }
  /**
   * @return string
   */
  public function getMaximumBytes()
  {
    return $this->maximumBytes;
  }
  /**
   * @param int
   */
  public function setMaximumBytesCount($maximumBytesCount)
  {
    $this->maximumBytesCount = $maximumBytesCount;
  }
  /**
   * @return int
   */
  public function getMaximumBytesCount()
  {
    return $this->maximumBytesCount;
  }
  /**
   * @param int
   */
  public function setMaximumThreadCount($maximumThreadCount)
  {
    $this->maximumThreadCount = $maximumThreadCount;
  }
  /**
   * @return int
   */
  public function getMaximumThreadCount()
  {
    return $this->maximumThreadCount;
  }
  /**
   * @param int
   */
  public function setOutstandingBundleCount($outstandingBundleCount)
  {
    $this->outstandingBundleCount = $outstandingBundleCount;
  }
  /**
   * @return int
   */
  public function getOutstandingBundleCount()
  {
    return $this->outstandingBundleCount;
  }
  /**
   * @param string
   */
  public function setOutstandingBytes($outstandingBytes)
  {
    $this->outstandingBytes = $outstandingBytes;
  }
  /**
   * @return string
   */
  public function getOutstandingBytes()
  {
    return $this->outstandingBytes;
  }
  /**
   * @param int
   */
  public function setOutstandingBytesCount($outstandingBytesCount)
  {
    $this->outstandingBytesCount = $outstandingBytesCount;
  }
  /**
   * @return int
   */
  public function getOutstandingBytesCount()
  {
    return $this->outstandingBytesCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(StreamingScalingReport::class, 'Google_Service_Dataflow_StreamingScalingReport');
