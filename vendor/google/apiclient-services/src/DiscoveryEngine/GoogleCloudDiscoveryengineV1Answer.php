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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1Answer extends \Google\Collection
{
  protected $collection_key = 'steps';
  /**
   * @var string[]
   */
  public $answerSkippedReasons;
  /**
   * @var string
   */
  public $answerText;
  protected $citationsType = GoogleCloudDiscoveryengineV1AnswerCitation::class;
  protected $citationsDataType = 'array';
  /**
   * @var string
   */
  public $completeTime;
  /**
   * @var string
   */
  public $createTime;
  public $groundingScore;
  protected $groundingSupportsType = GoogleCloudDiscoveryengineV1AnswerGroundingSupport::class;
  protected $groundingSupportsDataType = 'array';
  /**
   * @var string
   */
  public $name;
  protected $queryUnderstandingInfoType = GoogleCloudDiscoveryengineV1AnswerQueryUnderstandingInfo::class;
  protected $queryUnderstandingInfoDataType = '';
  protected $referencesType = GoogleCloudDiscoveryengineV1AnswerReference::class;
  protected $referencesDataType = 'array';
  /**
   * @var string[]
   */
  public $relatedQuestions;
  protected $safetyRatingsType = GoogleCloudDiscoveryengineV1SafetyRating::class;
  protected $safetyRatingsDataType = 'array';
  /**
   * @var string
   */
  public $state;
  protected $stepsType = GoogleCloudDiscoveryengineV1AnswerStep::class;
  protected $stepsDataType = 'array';

  /**
   * @param string[]
   */
  public function setAnswerSkippedReasons($answerSkippedReasons)
  {
    $this->answerSkippedReasons = $answerSkippedReasons;
  }
  /**
   * @return string[]
   */
  public function getAnswerSkippedReasons()
  {
    return $this->answerSkippedReasons;
  }
  /**
   * @param string
   */
  public function setAnswerText($answerText)
  {
    $this->answerText = $answerText;
  }
  /**
   * @return string
   */
  public function getAnswerText()
  {
    return $this->answerText;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1AnswerCitation[]
   */
  public function setCitations($citations)
  {
    $this->citations = $citations;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1AnswerCitation[]
   */
  public function getCitations()
  {
    return $this->citations;
  }
  /**
   * @param string
   */
  public function setCompleteTime($completeTime)
  {
    $this->completeTime = $completeTime;
  }
  /**
   * @return string
   */
  public function getCompleteTime()
  {
    return $this->completeTime;
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
  public function setGroundingScore($groundingScore)
  {
    $this->groundingScore = $groundingScore;
  }
  public function getGroundingScore()
  {
    return $this->groundingScore;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1AnswerGroundingSupport[]
   */
  public function setGroundingSupports($groundingSupports)
  {
    $this->groundingSupports = $groundingSupports;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1AnswerGroundingSupport[]
   */
  public function getGroundingSupports()
  {
    return $this->groundingSupports;
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
  /**
   * @param GoogleCloudDiscoveryengineV1AnswerQueryUnderstandingInfo
   */
  public function setQueryUnderstandingInfo(GoogleCloudDiscoveryengineV1AnswerQueryUnderstandingInfo $queryUnderstandingInfo)
  {
    $this->queryUnderstandingInfo = $queryUnderstandingInfo;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1AnswerQueryUnderstandingInfo
   */
  public function getQueryUnderstandingInfo()
  {
    return $this->queryUnderstandingInfo;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1AnswerReference[]
   */
  public function setReferences($references)
  {
    $this->references = $references;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1AnswerReference[]
   */
  public function getReferences()
  {
    return $this->references;
  }
  /**
   * @param string[]
   */
  public function setRelatedQuestions($relatedQuestions)
  {
    $this->relatedQuestions = $relatedQuestions;
  }
  /**
   * @return string[]
   */
  public function getRelatedQuestions()
  {
    return $this->relatedQuestions;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SafetyRating[]
   */
  public function setSafetyRatings($safetyRatings)
  {
    $this->safetyRatings = $safetyRatings;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SafetyRating[]
   */
  public function getSafetyRatings()
  {
    return $this->safetyRatings;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1AnswerStep[]
   */
  public function setSteps($steps)
  {
    $this->steps = $steps;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1AnswerStep[]
   */
  public function getSteps()
  {
    return $this->steps;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1Answer::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1Answer');
