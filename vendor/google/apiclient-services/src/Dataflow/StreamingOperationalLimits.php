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

class StreamingOperationalLimits extends \Google\Model
{
  /**
   * @var string
   */
  public $maxBagElementBytes;
  /**
   * @var string
   */
  public $maxGlobalDataBytes;
  /**
   * @var string
   */
  public $maxKeyBytes;
  /**
   * @var string
   */
  public $maxProductionOutputBytes;
  /**
   * @var string
   */
  public $maxSortedListElementBytes;
  /**
   * @var string
   */
  public $maxSourceStateBytes;
  /**
   * @var string
   */
  public $maxTagBytes;
  /**
   * @var string
   */
  public $maxValueBytes;

  /**
   * @param string
   */
  public function setMaxBagElementBytes($maxBagElementBytes)
  {
    $this->maxBagElementBytes = $maxBagElementBytes;
  }
  /**
   * @return string
   */
  public function getMaxBagElementBytes()
  {
    return $this->maxBagElementBytes;
  }
  /**
   * @param string
   */
  public function setMaxGlobalDataBytes($maxGlobalDataBytes)
  {
    $this->maxGlobalDataBytes = $maxGlobalDataBytes;
  }
  /**
   * @return string
   */
  public function getMaxGlobalDataBytes()
  {
    return $this->maxGlobalDataBytes;
  }
  /**
   * @param string
   */
  public function setMaxKeyBytes($maxKeyBytes)
  {
    $this->maxKeyBytes = $maxKeyBytes;
  }
  /**
   * @return string
   */
  public function getMaxKeyBytes()
  {
    return $this->maxKeyBytes;
  }
  /**
   * @param string
   */
  public function setMaxProductionOutputBytes($maxProductionOutputBytes)
  {
    $this->maxProductionOutputBytes = $maxProductionOutputBytes;
  }
  /**
   * @return string
   */
  public function getMaxProductionOutputBytes()
  {
    return $this->maxProductionOutputBytes;
  }
  /**
   * @param string
   */
  public function setMaxSortedListElementBytes($maxSortedListElementBytes)
  {
    $this->maxSortedListElementBytes = $maxSortedListElementBytes;
  }
  /**
   * @return string
   */
  public function getMaxSortedListElementBytes()
  {
    return $this->maxSortedListElementBytes;
  }
  /**
   * @param string
   */
  public function setMaxSourceStateBytes($maxSourceStateBytes)
  {
    $this->maxSourceStateBytes = $maxSourceStateBytes;
  }
  /**
   * @return string
   */
  public function getMaxSourceStateBytes()
  {
    return $this->maxSourceStateBytes;
  }
  /**
   * @param string
   */
  public function setMaxTagBytes($maxTagBytes)
  {
    $this->maxTagBytes = $maxTagBytes;
  }
  /**
   * @return string
   */
  public function getMaxTagBytes()
  {
    return $this->maxTagBytes;
  }
  /**
   * @param string
   */
  public function setMaxValueBytes($maxValueBytes)
  {
    $this->maxValueBytes = $maxValueBytes;
  }
  /**
   * @return string
   */
  public function getMaxValueBytes()
  {
    return $this->maxValueBytes;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(StreamingOperationalLimits::class, 'Google_Service_Dataflow_StreamingOperationalLimits');
