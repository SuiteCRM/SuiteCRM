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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1Tool extends \Google\Collection
{
  protected $collection_key = 'functionDeclarations';
  protected $codeExecutionType = GoogleCloudAiplatformV1ToolCodeExecution::class;
  protected $codeExecutionDataType = '';
  protected $functionDeclarationsType = GoogleCloudAiplatformV1FunctionDeclaration::class;
  protected $functionDeclarationsDataType = 'array';
  protected $googleSearchType = GoogleCloudAiplatformV1ToolGoogleSearch::class;
  protected $googleSearchDataType = '';
  protected $googleSearchRetrievalType = GoogleCloudAiplatformV1GoogleSearchRetrieval::class;
  protected $googleSearchRetrievalDataType = '';
  protected $retrievalType = GoogleCloudAiplatformV1Retrieval::class;
  protected $retrievalDataType = '';

  /**
   * @param GoogleCloudAiplatformV1ToolCodeExecution
   */
  public function setCodeExecution(GoogleCloudAiplatformV1ToolCodeExecution $codeExecution)
  {
    $this->codeExecution = $codeExecution;
  }
  /**
   * @return GoogleCloudAiplatformV1ToolCodeExecution
   */
  public function getCodeExecution()
  {
    return $this->codeExecution;
  }
  /**
   * @param GoogleCloudAiplatformV1FunctionDeclaration[]
   */
  public function setFunctionDeclarations($functionDeclarations)
  {
    $this->functionDeclarations = $functionDeclarations;
  }
  /**
   * @return GoogleCloudAiplatformV1FunctionDeclaration[]
   */
  public function getFunctionDeclarations()
  {
    return $this->functionDeclarations;
  }
  /**
   * @param GoogleCloudAiplatformV1ToolGoogleSearch
   */
  public function setGoogleSearch(GoogleCloudAiplatformV1ToolGoogleSearch $googleSearch)
  {
    $this->googleSearch = $googleSearch;
  }
  /**
   * @return GoogleCloudAiplatformV1ToolGoogleSearch
   */
  public function getGoogleSearch()
  {
    return $this->googleSearch;
  }
  /**
   * @param GoogleCloudAiplatformV1GoogleSearchRetrieval
   */
  public function setGoogleSearchRetrieval(GoogleCloudAiplatformV1GoogleSearchRetrieval $googleSearchRetrieval)
  {
    $this->googleSearchRetrieval = $googleSearchRetrieval;
  }
  /**
   * @return GoogleCloudAiplatformV1GoogleSearchRetrieval
   */
  public function getGoogleSearchRetrieval()
  {
    return $this->googleSearchRetrieval;
  }
  /**
   * @param GoogleCloudAiplatformV1Retrieval
   */
  public function setRetrieval(GoogleCloudAiplatformV1Retrieval $retrieval)
  {
    $this->retrieval = $retrieval;
  }
  /**
   * @return GoogleCloudAiplatformV1Retrieval
   */
  public function getRetrieval()
  {
    return $this->retrieval;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1Tool::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1Tool');
