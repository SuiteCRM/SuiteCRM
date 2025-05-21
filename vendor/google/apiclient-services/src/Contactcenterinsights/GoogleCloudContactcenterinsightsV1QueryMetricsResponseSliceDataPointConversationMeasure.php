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

class GoogleCloudContactcenterinsightsV1QueryMetricsResponseSliceDataPointConversationMeasure extends \Google\Collection
{
  protected $collection_key = 'qaTagScores';
  /**
   * @var float
   */
  public $averageAgentSentimentScore;
  /**
   * @var float
   */
  public $averageClientSentimentScore;
  public $averageCustomerSatisfactionRating;
  /**
   * @var string
   */
  public $averageDuration;
  public $averageQaNormalizedScore;
  public $averageQaQuestionNormalizedScore;
  /**
   * @var float
   */
  public $averageSilencePercentage;
  /**
   * @var float
   */
  public $averageTurnCount;
  /**
   * @var int
   */
  public $conversationCount;
  protected $qaTagScoresType = GoogleCloudContactcenterinsightsV1QueryMetricsResponseSliceDataPointConversationMeasureQaTagScore::class;
  protected $qaTagScoresDataType = 'array';

  /**
   * @param float
   */
  public function setAverageAgentSentimentScore($averageAgentSentimentScore)
  {
    $this->averageAgentSentimentScore = $averageAgentSentimentScore;
  }
  /**
   * @return float
   */
  public function getAverageAgentSentimentScore()
  {
    return $this->averageAgentSentimentScore;
  }
  /**
   * @param float
   */
  public function setAverageClientSentimentScore($averageClientSentimentScore)
  {
    $this->averageClientSentimentScore = $averageClientSentimentScore;
  }
  /**
   * @return float
   */
  public function getAverageClientSentimentScore()
  {
    return $this->averageClientSentimentScore;
  }
  public function setAverageCustomerSatisfactionRating($averageCustomerSatisfactionRating)
  {
    $this->averageCustomerSatisfactionRating = $averageCustomerSatisfactionRating;
  }
  public function getAverageCustomerSatisfactionRating()
  {
    return $this->averageCustomerSatisfactionRating;
  }
  /**
   * @param string
   */
  public function setAverageDuration($averageDuration)
  {
    $this->averageDuration = $averageDuration;
  }
  /**
   * @return string
   */
  public function getAverageDuration()
  {
    return $this->averageDuration;
  }
  public function setAverageQaNormalizedScore($averageQaNormalizedScore)
  {
    $this->averageQaNormalizedScore = $averageQaNormalizedScore;
  }
  public function getAverageQaNormalizedScore()
  {
    return $this->averageQaNormalizedScore;
  }
  public function setAverageQaQuestionNormalizedScore($averageQaQuestionNormalizedScore)
  {
    $this->averageQaQuestionNormalizedScore = $averageQaQuestionNormalizedScore;
  }
  public function getAverageQaQuestionNormalizedScore()
  {
    return $this->averageQaQuestionNormalizedScore;
  }
  /**
   * @param float
   */
  public function setAverageSilencePercentage($averageSilencePercentage)
  {
    $this->averageSilencePercentage = $averageSilencePercentage;
  }
  /**
   * @return float
   */
  public function getAverageSilencePercentage()
  {
    return $this->averageSilencePercentage;
  }
  /**
   * @param float
   */
  public function setAverageTurnCount($averageTurnCount)
  {
    $this->averageTurnCount = $averageTurnCount;
  }
  /**
   * @return float
   */
  public function getAverageTurnCount()
  {
    return $this->averageTurnCount;
  }
  /**
   * @param int
   */
  public function setConversationCount($conversationCount)
  {
    $this->conversationCount = $conversationCount;
  }
  /**
   * @return int
   */
  public function getConversationCount()
  {
    return $this->conversationCount;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1QueryMetricsResponseSliceDataPointConversationMeasureQaTagScore[]
   */
  public function setQaTagScores($qaTagScores)
  {
    $this->qaTagScores = $qaTagScores;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1QueryMetricsResponseSliceDataPointConversationMeasureQaTagScore[]
   */
  public function getQaTagScores()
  {
    return $this->qaTagScores;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1QueryMetricsResponseSliceDataPointConversationMeasure::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1QueryMetricsResponseSliceDataPointConversationMeasure');
