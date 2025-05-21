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

class GoogleCloudAiplatformV1StudySpecConvexAutomatedStoppingSpec extends \Google\Model
{
  /**
   * @var string
   */
  public $learningRateParameterName;
  /**
   * @var string
   */
  public $maxStepCount;
  /**
   * @var string
   */
  public $minMeasurementCount;
  /**
   * @var string
   */
  public $minStepCount;
  /**
   * @var bool
   */
  public $updateAllStoppedTrials;
  /**
   * @var bool
   */
  public $useElapsedDuration;

  /**
   * @param string
   */
  public function setLearningRateParameterName($learningRateParameterName)
  {
    $this->learningRateParameterName = $learningRateParameterName;
  }
  /**
   * @return string
   */
  public function getLearningRateParameterName()
  {
    return $this->learningRateParameterName;
  }
  /**
   * @param string
   */
  public function setMaxStepCount($maxStepCount)
  {
    $this->maxStepCount = $maxStepCount;
  }
  /**
   * @return string
   */
  public function getMaxStepCount()
  {
    return $this->maxStepCount;
  }
  /**
   * @param string
   */
  public function setMinMeasurementCount($minMeasurementCount)
  {
    $this->minMeasurementCount = $minMeasurementCount;
  }
  /**
   * @return string
   */
  public function getMinMeasurementCount()
  {
    return $this->minMeasurementCount;
  }
  /**
   * @param string
   */
  public function setMinStepCount($minStepCount)
  {
    $this->minStepCount = $minStepCount;
  }
  /**
   * @return string
   */
  public function getMinStepCount()
  {
    return $this->minStepCount;
  }
  /**
   * @param bool
   */
  public function setUpdateAllStoppedTrials($updateAllStoppedTrials)
  {
    $this->updateAllStoppedTrials = $updateAllStoppedTrials;
  }
  /**
   * @return bool
   */
  public function getUpdateAllStoppedTrials()
  {
    return $this->updateAllStoppedTrials;
  }
  /**
   * @param bool
   */
  public function setUseElapsedDuration($useElapsedDuration)
  {
    $this->useElapsedDuration = $useElapsedDuration;
  }
  /**
   * @return bool
   */
  public function getUseElapsedDuration()
  {
    return $this->useElapsedDuration;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1StudySpecConvexAutomatedStoppingSpec::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1StudySpecConvexAutomatedStoppingSpec');
