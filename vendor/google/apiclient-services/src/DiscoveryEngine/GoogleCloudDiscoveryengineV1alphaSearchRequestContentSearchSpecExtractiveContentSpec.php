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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecExtractiveContentSpec extends \Google\Model
{
  /**
   * @var int
   */
  public $maxExtractiveAnswerCount;
  /**
   * @var int
   */
  public $maxExtractiveSegmentCount;
  /**
   * @var int
   */
  public $numNextSegments;
  /**
   * @var int
   */
  public $numPreviousSegments;
  /**
   * @var bool
   */
  public $returnExtractiveSegmentScore;

  /**
   * @param int
   */
  public function setMaxExtractiveAnswerCount($maxExtractiveAnswerCount)
  {
    $this->maxExtractiveAnswerCount = $maxExtractiveAnswerCount;
  }
  /**
   * @return int
   */
  public function getMaxExtractiveAnswerCount()
  {
    return $this->maxExtractiveAnswerCount;
  }
  /**
   * @param int
   */
  public function setMaxExtractiveSegmentCount($maxExtractiveSegmentCount)
  {
    $this->maxExtractiveSegmentCount = $maxExtractiveSegmentCount;
  }
  /**
   * @return int
   */
  public function getMaxExtractiveSegmentCount()
  {
    return $this->maxExtractiveSegmentCount;
  }
  /**
   * @param int
   */
  public function setNumNextSegments($numNextSegments)
  {
    $this->numNextSegments = $numNextSegments;
  }
  /**
   * @return int
   */
  public function getNumNextSegments()
  {
    return $this->numNextSegments;
  }
  /**
   * @param int
   */
  public function setNumPreviousSegments($numPreviousSegments)
  {
    $this->numPreviousSegments = $numPreviousSegments;
  }
  /**
   * @return int
   */
  public function getNumPreviousSegments()
  {
    return $this->numPreviousSegments;
  }
  /**
   * @param bool
   */
  public function setReturnExtractiveSegmentScore($returnExtractiveSegmentScore)
  {
    $this->returnExtractiveSegmentScore = $returnExtractiveSegmentScore;
  }
  /**
   * @return bool
   */
  public function getReturnExtractiveSegmentScore()
  {
    return $this->returnExtractiveSegmentScore;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecExtractiveContentSpec::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecExtractiveContentSpec');
