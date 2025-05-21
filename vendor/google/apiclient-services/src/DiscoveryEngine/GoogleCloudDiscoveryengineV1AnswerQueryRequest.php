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

class GoogleCloudDiscoveryengineV1AnswerQueryRequest extends \Google\Model
{
  protected $answerGenerationSpecType = GoogleCloudDiscoveryengineV1AnswerQueryRequestAnswerGenerationSpec::class;
  protected $answerGenerationSpecDataType = '';
  /**
   * @var bool
   */
  public $asynchronousMode;
  protected $endUserSpecType = GoogleCloudDiscoveryengineV1AnswerQueryRequestEndUserSpec::class;
  protected $endUserSpecDataType = '';
  protected $groundingSpecType = GoogleCloudDiscoveryengineV1AnswerQueryRequestGroundingSpec::class;
  protected $groundingSpecDataType = '';
  protected $queryType = GoogleCloudDiscoveryengineV1Query::class;
  protected $queryDataType = '';
  protected $queryUnderstandingSpecType = GoogleCloudDiscoveryengineV1AnswerQueryRequestQueryUnderstandingSpec::class;
  protected $queryUnderstandingSpecDataType = '';
  protected $relatedQuestionsSpecType = GoogleCloudDiscoveryengineV1AnswerQueryRequestRelatedQuestionsSpec::class;
  protected $relatedQuestionsSpecDataType = '';
  protected $safetySpecType = GoogleCloudDiscoveryengineV1AnswerQueryRequestSafetySpec::class;
  protected $safetySpecDataType = '';
  protected $searchSpecType = GoogleCloudDiscoveryengineV1AnswerQueryRequestSearchSpec::class;
  protected $searchSpecDataType = '';
  /**
   * @var string
   */
  public $session;
  /**
   * @var string[]
   */
  public $userLabels;
  /**
   * @var string
   */
  public $userPseudoId;

  /**
   * @param GoogleCloudDiscoveryengineV1AnswerQueryRequestAnswerGenerationSpec
   */
  public function setAnswerGenerationSpec(GoogleCloudDiscoveryengineV1AnswerQueryRequestAnswerGenerationSpec $answerGenerationSpec)
  {
    $this->answerGenerationSpec = $answerGenerationSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1AnswerQueryRequestAnswerGenerationSpec
   */
  public function getAnswerGenerationSpec()
  {
    return $this->answerGenerationSpec;
  }
  /**
   * @param bool
   */
  public function setAsynchronousMode($asynchronousMode)
  {
    $this->asynchronousMode = $asynchronousMode;
  }
  /**
   * @return bool
   */
  public function getAsynchronousMode()
  {
    return $this->asynchronousMode;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1AnswerQueryRequestEndUserSpec
   */
  public function setEndUserSpec(GoogleCloudDiscoveryengineV1AnswerQueryRequestEndUserSpec $endUserSpec)
  {
    $this->endUserSpec = $endUserSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1AnswerQueryRequestEndUserSpec
   */
  public function getEndUserSpec()
  {
    return $this->endUserSpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1AnswerQueryRequestGroundingSpec
   */
  public function setGroundingSpec(GoogleCloudDiscoveryengineV1AnswerQueryRequestGroundingSpec $groundingSpec)
  {
    $this->groundingSpec = $groundingSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1AnswerQueryRequestGroundingSpec
   */
  public function getGroundingSpec()
  {
    return $this->groundingSpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1Query
   */
  public function setQuery(GoogleCloudDiscoveryengineV1Query $query)
  {
    $this->query = $query;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1Query
   */
  public function getQuery()
  {
    return $this->query;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1AnswerQueryRequestQueryUnderstandingSpec
   */
  public function setQueryUnderstandingSpec(GoogleCloudDiscoveryengineV1AnswerQueryRequestQueryUnderstandingSpec $queryUnderstandingSpec)
  {
    $this->queryUnderstandingSpec = $queryUnderstandingSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1AnswerQueryRequestQueryUnderstandingSpec
   */
  public function getQueryUnderstandingSpec()
  {
    return $this->queryUnderstandingSpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1AnswerQueryRequestRelatedQuestionsSpec
   */
  public function setRelatedQuestionsSpec(GoogleCloudDiscoveryengineV1AnswerQueryRequestRelatedQuestionsSpec $relatedQuestionsSpec)
  {
    $this->relatedQuestionsSpec = $relatedQuestionsSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1AnswerQueryRequestRelatedQuestionsSpec
   */
  public function getRelatedQuestionsSpec()
  {
    return $this->relatedQuestionsSpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1AnswerQueryRequestSafetySpec
   */
  public function setSafetySpec(GoogleCloudDiscoveryengineV1AnswerQueryRequestSafetySpec $safetySpec)
  {
    $this->safetySpec = $safetySpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1AnswerQueryRequestSafetySpec
   */
  public function getSafetySpec()
  {
    return $this->safetySpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1AnswerQueryRequestSearchSpec
   */
  public function setSearchSpec(GoogleCloudDiscoveryengineV1AnswerQueryRequestSearchSpec $searchSpec)
  {
    $this->searchSpec = $searchSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1AnswerQueryRequestSearchSpec
   */
  public function getSearchSpec()
  {
    return $this->searchSpec;
  }
  /**
   * @param string
   */
  public function setSession($session)
  {
    $this->session = $session;
  }
  /**
   * @return string
   */
  public function getSession()
  {
    return $this->session;
  }
  /**
   * @param string[]
   */
  public function setUserLabels($userLabels)
  {
    $this->userLabels = $userLabels;
  }
  /**
   * @return string[]
   */
  public function getUserLabels()
  {
    return $this->userLabels;
  }
  /**
   * @param string
   */
  public function setUserPseudoId($userPseudoId)
  {
    $this->userPseudoId = $userPseudoId;
  }
  /**
   * @return string
   */
  public function getUserPseudoId()
  {
    return $this->userPseudoId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1AnswerQueryRequest::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1AnswerQueryRequest');
