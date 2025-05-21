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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1SafetyRating extends \Google\Model
{
  /**
   * @var bool
   */
  public $blocked;
  /**
   * @var string
   */
  public $category;
  /**
   * @var string
   */
  public $probability;
  /**
   * @var float
   */
  public $probabilityScore;
  /**
   * @var string
   */
  public $severity;
  /**
   * @var float
   */
  public $severityScore;

  /**
   * @param bool
   */
  public function setBlocked($blocked)
  {
    $this->blocked = $blocked;
  }
  /**
   * @return bool
   */
  public function getBlocked()
  {
    return $this->blocked;
  }
  /**
   * @param string
   */
  public function setCategory($category)
  {
    $this->category = $category;
  }
  /**
   * @return string
   */
  public function getCategory()
  {
    return $this->category;
  }
  /**
   * @param string
   */
  public function setProbability($probability)
  {
    $this->probability = $probability;
  }
  /**
   * @return string
   */
  public function getProbability()
  {
    return $this->probability;
  }
  /**
   * @param float
   */
  public function setProbabilityScore($probabilityScore)
  {
    $this->probabilityScore = $probabilityScore;
  }
  /**
   * @return float
   */
  public function getProbabilityScore()
  {
    return $this->probabilityScore;
  }
  /**
   * @param string
   */
  public function setSeverity($severity)
  {
    $this->severity = $severity;
  }
  /**
   * @return string
   */
  public function getSeverity()
  {
    return $this->severity;
  }
  /**
   * @param float
   */
  public function setSeverityScore($severityScore)
  {
    $this->severityScore = $severityScore;
  }
  /**
   * @return float
   */
  public function getSeverityScore()
  {
    return $this->severityScore;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SafetyRating::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SafetyRating');
