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

namespace Google\Service\Contactcenterinsights;

class GoogleCloudContactcenterinsightsV1alpha1QaScorecardResultScoreSource extends \Google\Collection
{
  protected $collection_key = 'qaTagResults';
  public $normalizedScore;
  public $potentialScore;
  protected $qaTagResultsType = GoogleCloudContactcenterinsightsV1alpha1QaScorecardResultQaTagResult::class;
  protected $qaTagResultsDataType = 'array';
  public $score;
  /**
   * @var string
   */
  public $sourceType;

  public function setNormalizedScore($normalizedScore)
  {
    $this->normalizedScore = $normalizedScore;
  }
  public function getNormalizedScore()
  {
    return $this->normalizedScore;
  }
  public function setPotentialScore($potentialScore)
  {
    $this->potentialScore = $potentialScore;
  }
  public function getPotentialScore()
  {
    return $this->potentialScore;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1alpha1QaScorecardResultQaTagResult[]
   */
  public function setQaTagResults($qaTagResults)
  {
    $this->qaTagResults = $qaTagResults;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1alpha1QaScorecardResultQaTagResult[]
   */
  public function getQaTagResults()
  {
    return $this->qaTagResults;
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
  public function setSourceType($sourceType)
  {
    $this->sourceType = $sourceType;
  }
  /**
   * @return string
   */
  public function getSourceType()
  {
    return $this->sourceType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1alpha1QaScorecardResultScoreSource::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1alpha1QaScorecardResultScoreSource');
