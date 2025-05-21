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

class GoogleCloudDiscoveryengineV1betaSearchRequestContentSearchSpecSummarySpec extends \Google\Model
{
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
  public $ignoreNonSummarySeekingQuery;
  /**
   * @var bool
   */
  public $includeCitations;
  /**
   * @var string
   */
  public $languageCode;
  protected $modelPromptSpecType = GoogleCloudDiscoveryengineV1betaSearchRequestContentSearchSpecSummarySpecModelPromptSpec::class;
  protected $modelPromptSpecDataType = '';
  protected $modelSpecType = GoogleCloudDiscoveryengineV1betaSearchRequestContentSearchSpecSummarySpecModelSpec::class;
  protected $modelSpecDataType = '';
  /**
   * @var int
   */
  public $summaryResultCount;
  /**
   * @var bool
   */
  public $useSemanticChunks;

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
  public function setIgnoreNonSummarySeekingQuery($ignoreNonSummarySeekingQuery)
  {
    $this->ignoreNonSummarySeekingQuery = $ignoreNonSummarySeekingQuery;
  }
  /**
   * @return bool
   */
  public function getIgnoreNonSummarySeekingQuery()
  {
    return $this->ignoreNonSummarySeekingQuery;
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
   * @param string
   */
  public function setLanguageCode($languageCode)
  {
    $this->languageCode = $languageCode;
  }
  /**
   * @return string
   */
  public function getLanguageCode()
  {
    return $this->languageCode;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1betaSearchRequestContentSearchSpecSummarySpecModelPromptSpec
   */
  public function setModelPromptSpec(GoogleCloudDiscoveryengineV1betaSearchRequestContentSearchSpecSummarySpecModelPromptSpec $modelPromptSpec)
  {
    $this->modelPromptSpec = $modelPromptSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaSearchRequestContentSearchSpecSummarySpecModelPromptSpec
   */
  public function getModelPromptSpec()
  {
    return $this->modelPromptSpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1betaSearchRequestContentSearchSpecSummarySpecModelSpec
   */
  public function setModelSpec(GoogleCloudDiscoveryengineV1betaSearchRequestContentSearchSpecSummarySpecModelSpec $modelSpec)
  {
    $this->modelSpec = $modelSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaSearchRequestContentSearchSpecSummarySpecModelSpec
   */
  public function getModelSpec()
  {
    return $this->modelSpec;
  }
  /**
   * @param int
   */
  public function setSummaryResultCount($summaryResultCount)
  {
    $this->summaryResultCount = $summaryResultCount;
  }
  /**
   * @return int
   */
  public function getSummaryResultCount()
  {
    return $this->summaryResultCount;
  }
  /**
   * @param bool
   */
  public function setUseSemanticChunks($useSemanticChunks)
  {
    $this->useSemanticChunks = $useSemanticChunks;
  }
  /**
   * @return bool
   */
  public function getUseSemanticChunks()
  {
    return $this->useSemanticChunks;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1betaSearchRequestContentSearchSpecSummarySpec::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1betaSearchRequestContentSearchSpecSummarySpec');
