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

class GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpec extends \Google\Model
{
  protected $chunkSpecType = GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpecChunkSpec::class;
  protected $chunkSpecDataType = '';
  protected $extractiveContentSpecType = GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpecExtractiveContentSpec::class;
  protected $extractiveContentSpecDataType = '';
  /**
   * @var string
   */
  public $searchResultMode;
  protected $snippetSpecType = GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpecSnippetSpec::class;
  protected $snippetSpecDataType = '';
  protected $summarySpecType = GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpecSummarySpec::class;
  protected $summarySpecDataType = '';

  /**
   * @param GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpecChunkSpec
   */
  public function setChunkSpec(GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpecChunkSpec $chunkSpec)
  {
    $this->chunkSpec = $chunkSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpecChunkSpec
   */
  public function getChunkSpec()
  {
    return $this->chunkSpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpecExtractiveContentSpec
   */
  public function setExtractiveContentSpec(GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpecExtractiveContentSpec $extractiveContentSpec)
  {
    $this->extractiveContentSpec = $extractiveContentSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpecExtractiveContentSpec
   */
  public function getExtractiveContentSpec()
  {
    return $this->extractiveContentSpec;
  }
  /**
   * @param string
   */
  public function setSearchResultMode($searchResultMode)
  {
    $this->searchResultMode = $searchResultMode;
  }
  /**
   * @return string
   */
  public function getSearchResultMode()
  {
    return $this->searchResultMode;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpecSnippetSpec
   */
  public function setSnippetSpec(GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpecSnippetSpec $snippetSpec)
  {
    $this->snippetSpec = $snippetSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpecSnippetSpec
   */
  public function getSnippetSpec()
  {
    return $this->snippetSpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpecSummarySpec
   */
  public function setSummarySpec(GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpecSummarySpec $summarySpec)
  {
    $this->summarySpec = $summarySpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpecSummarySpec
   */
  public function getSummarySpec()
  {
    return $this->summarySpec;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpec::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpec');
