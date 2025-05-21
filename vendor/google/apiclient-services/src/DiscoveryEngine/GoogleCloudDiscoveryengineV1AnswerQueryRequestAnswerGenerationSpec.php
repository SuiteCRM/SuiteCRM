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

class GoogleCloudDiscoveryengineV1AnswerQueryRequestAnswerGenerationSpec extends \Google\Model
{
  /**
   * @var string
   */
  public $answerLanguageCode;
  /**
   * @var bool
   */
  public $ignoreAdversarialQuery;
  /**
   * @var bool
   */
  public $ignoreJailBreakingQuery;
  /**
   * @var bool
   */
  public $ignoreLowRelevantContent;
  /**
   * @var bool
   */
  public $ignoreNonAnswerSeekingQuery;
  /**
   * @var bool
   */
  public $includeCitations;
  protected $modelSpecType = GoogleCloudDiscoveryengineV1AnswerQueryRequestAnswerGenerationSpecModelSpec::class;
  protected $modelSpecDataType = '';
  protected $promptSpecType = GoogleCloudDiscoveryengineV1AnswerQueryRequestAnswerGenerationSpecPromptSpec::class;
  protected $promptSpecDataType = '';

  /**
   * @param string
   */
  public function setAnswerLanguageCode($answerLanguageCode)
  {
    $this->answerLanguageCode = $answerLanguageCode;
  }
  /**
   * @return string
   */
  public function getAnswerLanguageCode()
  {
    return $this->answerLanguageCode;
  }
  /**
   * @param bool
   */
  public function setIgnoreAdversarialQuery($ignoreAdversarialQuery)
  {
    $this->ignoreAdversarialQuery = $ignoreAdversarialQuery;
  }
  /**
   * @return bool
   */
  public function getIgnoreAdversarialQuery()
  {
    return $this->ignoreAdversarialQuery;
  }
  /**
   * @param bool
   */
  public function setIgnoreJailBreakingQuery($ignoreJailBreakingQuery)
  {
    $this->ignoreJailBreakingQuery = $ignoreJailBreakingQuery;
  }
  /**
   * @return bool
   */
  public function getIgnoreJailBreakingQuery()
  {
    return $this->ignoreJailBreakingQuery;
  }
  /**
   * @param bool
   */
  public function setIgnoreLowRelevantContent($ignoreLowRelevantContent)
  {
    $this->ignoreLowRelevantContent = $ignoreLowRelevantContent;
  }
  /**
   * @return bool
   */
  public function getIgnoreLowRelevantContent()
  {
    return $this->ignoreLowRelevantContent;
  }
  /**
   * @param bool
   */
  public function setIgnoreNonAnswerSeekingQuery($ignoreNonAnswerSeekingQuery)
  {
    $this->ignoreNonAnswerSeekingQuery = $ignoreNonAnswerSeekingQuery;
  }
  /**
   * @return bool
   */
  public function getIgnoreNonAnswerSeekingQuery()
  {
    return $this->ignoreNonAnswerSeekingQuery;
  }
  /**
   * @param bool
   */
  public function setIncludeCitations($includeCitations)
  {
    $this->includeCitations = $includeCitations;
  }
  /**
   * @return bool
   */
  public function getIncludeCitations()
  {
    return $this->includeCitations;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1AnswerQueryRequestAnswerGenerationSpecModelSpec
   */
  public function setModelSpec(GoogleCloudDiscoveryengineV1AnswerQueryRequestAnswerGenerationSpecModelSpec $modelSpec)
  {
    $this->modelSpec = $modelSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1AnswerQueryRequestAnswerGenerationSpecModelSpec
   */
  public function getModelSpec()
  {
    return $this->modelSpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1AnswerQueryRequestAnswerGenerationSpecPromptSpec
   */
  public function setPromptSpec(GoogleCloudDiscoveryengineV1AnswerQueryRequestAnswerGenerationSpecPromptSpec $promptSpec)
  {
    $this->promptSpec = $promptSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1AnswerQueryRequestAnswerGenerationSpecPromptSpec
   */
  public function getPromptSpec()
  {
    return $this->promptSpec;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1AnswerQueryRequestAnswerGenerationSpec::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1AnswerQueryRequestAnswerGenerationSpec');
