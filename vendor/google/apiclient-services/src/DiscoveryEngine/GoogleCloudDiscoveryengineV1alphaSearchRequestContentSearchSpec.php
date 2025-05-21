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

class GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpec extends \Google\Model
{
  protected $chunkSpecType = GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecChunkSpec::class;
  protected $chunkSpecDataType = '';
  protected $extractiveContentSpecType = GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecExtractiveContentSpec::class;
  protected $extractiveContentSpecDataType = '';
  /**
   * @var string
   */
  public $searchResultMode;
  protected $snippetSpecType = GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecSnippetSpec::class;
  protected $snippetSpecDataType = '';
  protected $summarySpecType = GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecSummarySpec::class;
  protected $summarySpecDataType = '';

  /**
   * @param GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecChunkSpec
   */
  public function setChunkSpec(GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecChunkSpec $chunkSpec)
  {
    $this->chunkSpec = $chunkSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecChunkSpec
   */
  public function getChunkSpec()
  {
    return $this->chunkSpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecExtractiveContentSpec
   */
  public function setExtractiveContentSpec(GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecExtractiveContentSpec $extractiveContentSpec)
  {
    $this->extractiveContentSpec = $extractiveContentSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecExtractiveContentSpec
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
   * @param GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecSnippetSpec
   */
  public function setSnippetSpec(GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecSnippetSpec $snippetSpec)
  {
    $this->snippetSpec = $snippetSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecSnippetSpec
   */
  public function getSnippetSpec()
  {
    return $this->snippetSpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecSummarySpec
   */
  public function setSummarySpec(GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecSummarySpec $summarySpec)
  {
    $this->summarySpec = $summarySpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpecSummarySpec
   */
  public function getSummarySpec()
  {
    return $this->summarySpec;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpec::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpec');
