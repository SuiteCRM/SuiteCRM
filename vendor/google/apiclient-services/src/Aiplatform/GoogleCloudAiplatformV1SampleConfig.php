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

class GoogleCloudAiplatformV1SampleConfig extends \Google\Model
{
  /**
   * @var int
   */
  public $followingBatchSamplePercentage;
  /**
   * @var int
   */
  public $initialBatchSamplePercentage;
  /**
   * @var string
   */
  public $sampleStrategy;

  /**
   * @param int
   */
  public function setFollowingBatchSamplePercentage($followingBatchSamplePercentage)
  {
    $this->followingBatchSamplePercentage = $followingBatchSamplePercentage;
  }
  /**
   * @return int
   */
  public function getFollowingBatchSamplePercentage()
  {
    return $this->followingBatchSamplePercentage;
  }
  /**
   * @param int
   */
  public function setInitialBatchSamplePercentage($initialBatchSamplePercentage)
  {
    $this->initialBatchSamplePercentage = $initialBatchSamplePercentage;
  }
  /**
   * @return int
   */
  public function getInitialBatchSamplePercentage()
  {
    return $this->initialBatchSamplePercentage;
  }
  /**
   * @param string
   */
  public function setSampleStrategy($sampleStrategy)
  {
    $this->sampleStrategy = $sampleStrategy;
  }
  /**
   * @return string
   */
  public function getSampleStrategy()
  {
    return $this->sampleStrategy;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SampleConfig::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SampleConfig');
