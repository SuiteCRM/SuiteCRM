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

class GoogleCloudAiplatformV1FeatureStatsAnomaly extends \Google\Model
{
  public $anomalyDetectionThreshold;
  /**
   * @var string
   */
  public $anomalyUri;
  public $distributionDeviation;
  /**
   * @var string
   */
  public $endTime;
  public $score;
  /**
   * @var string
   */
  public $startTime;
  /**
   * @var string
   */
  public $statsUri;

  public function setAnomalyDetectionThreshold($anomalyDetectionThreshold)
  {
    $this->anomalyDetectionThreshold = $anomalyDetectionThreshold;
  }
  public function getAnomalyDetectionThreshold()
  {
    return $this->anomalyDetectionThreshold;
  }
  /**
   * @param string
   */
  public function setAnomalyUri($anomalyUri)
  {
    $this->anomalyUri = $anomalyUri;
  }
  /**
   * @return string
   */
  public function getAnomalyUri()
  {
    return $this->anomalyUri;
  }
  public function setDistributionDeviation($distributionDeviation)
  {
    $this->distributionDeviation = $distributionDeviation;
  }
  public function getDistributionDeviation()
  {
    return $this->distributionDeviation;
  }
  /**
   * @param string
   */
  public function setEndTime($endTime)
  {
    $this->endTime = $endTime;
  }
  /**
   * @return string
   */
  public function getEndTime()
  {
    return $this->endTime;
  }
  public function setScore($score)
  {
    $this->score = $score;
  }
  public function getScore()
  {
    return $this->score;
  }
  /**
   * @param string
   */
  public function setStartTime($startTime)
  {
    $this->startTime = $startTime;
  }
  /**
   * @return string
   */
  public function getStartTime()
  {
    return $this->startTime;
  }
  /**
   * @param string
   */
  public function setStatsUri($statsUri)
  {
    $this->statsUri = $statsUri;
  }
  /**
   * @return string
   */
  public function getStatsUri()
  {
    return $this->statsUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1FeatureStatsAnomaly::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1FeatureStatsAnomaly');
