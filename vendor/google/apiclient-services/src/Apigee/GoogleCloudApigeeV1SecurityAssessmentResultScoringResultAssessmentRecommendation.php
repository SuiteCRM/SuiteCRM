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

class GoogleCloudApigeeV1SecurityAssessmentResultScoringResultAssessmentRecommendation extends \Google\Collection
{
  protected $collection_key = 'recommendations';
  /**
   * @var string
   */
  public $displayName;
  protected $recommendationsType = GoogleCloudApigeeV1SecurityAssessmentResultScoringResultAssessmentRecommendationRecommendation::class;
  protected $recommendationsDataType = 'array';
  /**
   * @var int
   */
  public $scoreImpact;
  /**
   * @var string
   */
  public $verdict;
  /**
   * @var string
   */
  public $weight;

  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param GoogleCloudApigeeV1SecurityAssessmentResultScoringResultAssessmentRecommendationRecommendation[]
   */
  public function setRecommendations($recommendations)
  {
    $this->recommendations = $recommendations;
  }
  /**
   * @return GoogleCloudApigeeV1SecurityAssessmentResultScoringResultAssessmentRecommendationRecommendation[]
   */
  public function getRecommendations()
  {
    return $this->recommendations;
  }
  /**
   * @param int
   */
  public function setScoreImpact($scoreImpact)
  {
    $this->scoreImpact = $scoreImpact;
  }
  /**
   * @return int
   */
  public function getScoreImpact()
  {
    return $this->scoreImpact;
  }
  /**
   * @param string
   */
  public function setVerdict($verdict)
  {
    $this->verdict = $verdict;
  }
  /**
   * @return string
   */
  public function getVerdict()
  {
    return $this->verdict;
  }
  /**
   * @param string
   */
  public function setWeight($weight)
  {
    $this->weight = $weight;
  }
  /**
   * @return string
   */
  public function getWeight()
  {
    return $this->weight;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudApigeeV1SecurityAssessmentResultScoringResultAssessmentRecommendation::class, 'Google_Service_Apigee_GoogleCloudApigeeV1SecurityAssessmentResultScoringResultAssessmentRecommendation');
