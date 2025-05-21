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

namespace Google\Service\Apigee;

class GoogleCloudApigeeV1SecurityAssessmentResultScoringResult extends \Google\Model
{
  protected $assessmentRecommendationsType = GoogleCloudApigeeV1SecurityAssessmentResultScoringResultAssessmentRecommendation::class;
  protected $assessmentRecommendationsDataType = 'map';
  /**
   * @var string
   */
  public $dataUpdateTime;
  /**
   * @var int[]
   */
  public $failedAssessmentPerWeight;
  /**
   * @var int
   */
  public $score;
  /**
   * @var string
   */
  public $severity;

  /**
   * @param GoogleCloudApigeeV1SecurityAssessmentResultScoringResultAssessmentRecommendation[]
   */
  public function setAssessmentRecommendations($assessmentRecommendations)
  {
    $this->assessmentRecommendations = $assessmentRecommendations;
  }
  /**
   * @return GoogleCloudApigeeV1SecurityAssessmentResultScoringResultAssessmentRecommendation[]
   */
  public function getAssessmentRecommendations()
  {
    return $this->assessmentRecommendations;
  }
  /**
   * @param string
   */
  public function setDataUpdateTime($dataUpdateTime)
  {
    $this->dataUpdateTime = $dataUpdateTime;
  }
  /**
   * @return string
   */
  public function getDataUpdateTime()
  {
    return $this->dataUpdateTime;
  }
  /**
   * @param int[]
   */
  public function setFailedAssessmentPerWeight($failedAssessmentPerWeight)
  {
    $this->failedAssessmentPerWeight = $failedAssessmentPerWeight;
  }
  /**
   * @return int[]
   */
  public function getFailedAssessmentPerWeight()
  {
    return $this->failedAssessmentPerWeight;
  }
  /**
   * @param int
   */
  public function setScore($score)
  {
    $this->score = $score;
  }
  /**
   * @return int
   */
  public function getScore()
  {
    return $this->score;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudApigeeV1SecurityAssessmentResultScoringResult::class, 'Google_Service_Apigee_GoogleCloudApigeeV1SecurityAssessmentResultScoringResult');
