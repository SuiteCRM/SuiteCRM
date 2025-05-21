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

class GoogleCloudAiplatformV1StudySpecStudyStoppingConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $maxDurationNoProgress;
  /**
   * @var int
   */
  public $maxNumTrials;
  /**
   * @var int
   */
  public $maxNumTrialsNoProgress;
  protected $maximumRuntimeConstraintType = GoogleCloudAiplatformV1StudyTimeConstraint::class;
  protected $maximumRuntimeConstraintDataType = '';
  /**
   * @var int
   */
  public $minNumTrials;
  protected $minimumRuntimeConstraintType = GoogleCloudAiplatformV1StudyTimeConstraint::class;
  protected $minimumRuntimeConstraintDataType = '';
  /**
   * @var bool
   */
  public $shouldStopAsap;

  /**
   * @param string
   */
  public function setMaxDurationNoProgress($maxDurationNoProgress)
  {
    $this->maxDurationNoProgress = $maxDurationNoProgress;
  }
  /**
   * @return string
   */
  public function getMaxDurationNoProgress()
  {
    return $this->maxDurationNoProgress;
  }
  /**
   * @param int
   */
  public function setMaxNumTrials($maxNumTrials)
  {
    $this->maxNumTrials = $maxNumTrials;
  }
  /**
   * @return int
   */
  public function getMaxNumTrials()
  {
    return $this->maxNumTrials;
  }
  /**
   * @param int
   */
  public function setMaxNumTrialsNoProgress($maxNumTrialsNoProgress)
  {
    $this->maxNumTrialsNoProgress = $maxNumTrialsNoProgress;
  }
  /**
   * @return int
   */
  public function getMaxNumTrialsNoProgress()
  {
    return $this->maxNumTrialsNoProgress;
  }
  /**
   * @param GoogleCloudAiplatformV1StudyTimeConstraint
   */
  public function setMaximumRuntimeConstraint(GoogleCloudAiplatformV1StudyTimeConstraint $maximumRuntimeConstraint)
  {
    $this->maximumRuntimeConstraint = $maximumRuntimeConstraint;
  }
  /**
   * @return GoogleCloudAiplatformV1StudyTimeConstraint
   */
  public function getMaximumRuntimeConstraint()
  {
    return $this->maximumRuntimeConstraint;
  }
  /**
   * @param int
   */
  public function setMinNumTrials($minNumTrials)
  {
    $this->minNumTrials = $minNumTrials;
  }
  /**
   * @return int
   */
  public function getMinNumTrials()
  {
    return $this->minNumTrials;
  }
  /**
   * @param GoogleCloudAiplatformV1StudyTimeConstraint
   */
  public function setMinimumRuntimeConstraint(GoogleCloudAiplatformV1StudyTimeConstraint $minimumRuntimeConstraint)
  {
    $this->minimumRuntimeConstraint = $minimumRuntimeConstraint;
  }
  /**
   * @return GoogleCloudAiplatformV1StudyTimeConstraint
   */
  public function getMinimumRuntimeConstraint()
  {
    return $this->minimumRuntimeConstraint;
  }
  /**
   * @param bool
   */
  public function setShouldStopAsap($shouldStopAsap)
  {
    $this->shouldStopAsap = $shouldStopAsap;
  }
  /**
   * @return bool
   */
  public function getShouldStopAsap()
  {
    return $this->shouldStopAsap;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1StudySpecStudyStoppingConfig::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1StudySpecStudyStoppingConfig');
