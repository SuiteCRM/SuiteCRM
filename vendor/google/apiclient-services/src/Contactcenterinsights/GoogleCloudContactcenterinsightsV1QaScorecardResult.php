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

class GoogleCloudContactcenterinsightsV1QaScorecardResult extends \Google\Collection
{
  protected $collection_key = 'scoreSources';
  /**
   * @var string
   */
  public $agentId;
  /**
   * @var string
   */
  public $conversation;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $name;
  public $normalizedScore;
  public $potentialScore;
  protected $qaAnswersType = GoogleCloudContactcenterinsightsV1QaAnswer::class;
  protected $qaAnswersDataType = 'array';
  /**
   * @var string
   */
  public $qaScorecardRevision;
  protected $qaTagResultsType = GoogleCloudContactcenterinsightsV1QaScorecardResultQaTagResult::class;
  protected $qaTagResultsDataType = 'array';
  public $score;
  protected $scoreSourcesType = GoogleCloudContactcenterinsightsV1QaScorecardResultScoreSource::class;
  protected $scoreSourcesDataType = 'array';

  /**
   * @param string
   */
  public function setAgentId($agentId)
  {
    $this->agentId = $agentId;
  }
  /**
   * @return string
   */
  public function getAgentId()
  {
    return $this->agentId;
  }
  /**
   * @param string
   */
  public function setConversation($conversation)
  {
    $this->conversation = $conversation;
  }
  /**
   * @return string
   */
  public function getConversation()
  {
    return $this->conversation;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
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
   * @param GoogleCloudContactcenterinsightsV1QaAnswer[]
   */
  public function setQaAnswers($qaAnswers)
  {
    $this->qaAnswers = $qaAnswers;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1QaAnswer[]
   */
  public function getQaAnswers()
  {
    return $this->qaAnswers;
  }
  /**
   * @param string
   */
  public function setQaScorecardRevision($qaScorecardRevision)
  {
    $this->qaScorecardRevision = $qaScorecardRevision;
  }
  /**
   * @return string
   */
  public function getQaScorecardRevision()
  {
    return $this->qaScorecardRevision;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1QaScorecardResultQaTagResult[]
   */
  public function setQaTagResults($qaTagResults)
  {
    $this->qaTagResults = $qaTagResults;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1QaScorecardResultQaTagResult[]
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
   * @param GoogleCloudContactcenterinsightsV1QaScorecardResultScoreSource[]
   */
  public function setScoreSources($scoreSources)
  {
    $this->scoreSources = $scoreSources;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1QaScorecardResultScoreSource[]
   */
  public function getScoreSources()
  {
    return $this->scoreSources;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1QaScorecardResult::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1QaScorecardResult');
