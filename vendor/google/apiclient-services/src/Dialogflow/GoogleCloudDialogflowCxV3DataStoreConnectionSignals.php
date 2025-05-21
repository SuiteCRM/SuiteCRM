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

namespace Google\Service\Dialogflow;

class GoogleCloudDialogflowCxV3DataStoreConnectionSignals extends \Google\Collection
{
  protected $collection_key = 'searchSnippets';
  /**
   * @var string
   */
  public $answer;
  protected $answerGenerationModelCallSignalsType = GoogleCloudDialogflowCxV3DataStoreConnectionSignalsAnswerGenerationModelCallSignals::class;
  protected $answerGenerationModelCallSignalsDataType = '';
  protected $answerPartsType = GoogleCloudDialogflowCxV3DataStoreConnectionSignalsAnswerPart::class;
  protected $answerPartsDataType = 'array';
  protected $citedSnippetsType = GoogleCloudDialogflowCxV3DataStoreConnectionSignalsCitedSnippet::class;
  protected $citedSnippetsDataType = 'array';
  protected $groundingSignalsType = GoogleCloudDialogflowCxV3DataStoreConnectionSignalsGroundingSignals::class;
  protected $groundingSignalsDataType = '';
  protected $rewriterModelCallSignalsType = GoogleCloudDialogflowCxV3DataStoreConnectionSignalsRewriterModelCallSignals::class;
  protected $rewriterModelCallSignalsDataType = '';
  /**
   * @var string
   */
  public $rewrittenQuery;
  protected $safetySignalsType = GoogleCloudDialogflowCxV3DataStoreConnectionSignalsSafetySignals::class;
  protected $safetySignalsDataType = '';
  protected $searchSnippetsType = GoogleCloudDialogflowCxV3DataStoreConnectionSignalsSearchSnippet::class;
  protected $searchSnippetsDataType = 'array';

  /**
   * @param string
   */
  public function setAnswer($answer)
  {
    $this->answer = $answer;
  }
  /**
   * @return string
   */
  public function getAnswer()
  {
    return $this->answer;
  }
  /**
   * @param GoogleCloudDialogflowCxV3DataStoreConnectionSignalsAnswerGenerationModelCallSignals
   */
  public function setAnswerGenerationModelCallSignals(GoogleCloudDialogflowCxV3DataStoreConnectionSignalsAnswerGenerationModelCallSignals $answerGenerationModelCallSignals)
  {
    $this->answerGenerationModelCallSignals = $answerGenerationModelCallSignals;
  }
  /**
   * @return GoogleCloudDialogflowCxV3DataStoreConnectionSignalsAnswerGenerationModelCallSignals
   */
  public function getAnswerGenerationModelCallSignals()
  {
    return $this->answerGenerationModelCallSignals;
  }
  /**
   * @param GoogleCloudDialogflowCxV3DataStoreConnectionSignalsAnswerPart[]
   */
  public function setAnswerParts($answerParts)
  {
    $this->answerParts = $answerParts;
  }
  /**
   * @return GoogleCloudDialogflowCxV3DataStoreConnectionSignalsAnswerPart[]
   */
  public function getAnswerParts()
  {
    return $this->answerParts;
  }
  /**
   * @param GoogleCloudDialogflowCxV3DataStoreConnectionSignalsCitedSnippet[]
   */
  public function setCitedSnippets($citedSnippets)
  {
    $this->citedSnippets = $citedSnippets;
  }
  /**
   * @return GoogleCloudDialogflowCxV3DataStoreConnectionSignalsCitedSnippet[]
   */
  public function getCitedSnippets()
  {
    return $this->citedSnippets;
  }
  /**
   * @param GoogleCloudDialogflowCxV3DataStoreConnectionSignalsGroundingSignals
   */
  public function setGroundingSignals(GoogleCloudDialogflowCxV3DataStoreConnectionSignalsGroundingSignals $groundingSignals)
  {
    $this->groundingSignals = $groundingSignals;
  }
  /**
   * @return GoogleCloudDialogflowCxV3DataStoreConnectionSignalsGroundingSignals
   */
  public function getGroundingSignals()
  {
    return $this->groundingSignals;
  }
  /**
   * @param GoogleCloudDialogflowCxV3DataStoreConnectionSignalsRewriterModelCallSignals
   */
  public function setRewriterModelCallSignals(GoogleCloudDialogflowCxV3DataStoreConnectionSignalsRewriterModelCallSignals $rewriterModelCallSignals)
  {
    $this->rewriterModelCallSignals = $rewriterModelCallSignals;
  }
  /**
   * @return GoogleCloudDialogflowCxV3DataStoreConnectionSignalsRewriterModelCallSignals
   */
  public function getRewriterModelCallSignals()
  {
    return $this->rewriterModelCallSignals;
  }
  /**
   * @param string
   */
  public function setRewrittenQuery($rewrittenQuery)
  {
    $this->rewrittenQuery = $rewrittenQuery;
  }
  /**
   * @return string
   */
  public function getRewrittenQuery()
  {
    return $this->rewrittenQuery;
  }
  /**
   * @param GoogleCloudDialogflowCxV3DataStoreConnectionSignalsSafetySignals
   */
  public function setSafetySignals(GoogleCloudDialogflowCxV3DataStoreConnectionSignalsSafetySignals $safetySignals)
  {
    $this->safetySignals = $safetySignals;
  }
  /**
   * @return GoogleCloudDialogflowCxV3DataStoreConnectionSignalsSafetySignals
   */
  public function getSafetySignals()
  {
    return $this->safetySignals;
  }
  /**
   * @param GoogleCloudDialogflowCxV3DataStoreConnectionSignalsSearchSnippet[]
   */
  public function setSearchSnippets($searchSnippets)
  {
    $this->searchSnippets = $searchSnippets;
  }
  /**
   * @return GoogleCloudDialogflowCxV3DataStoreConnectionSignalsSearchSnippet[]
   */
  public function getSearchSnippets()
  {
    return $this->searchSnippets;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDialogflowCxV3DataStoreConnectionSignals::class, 'Google_Service_Dialogflow_GoogleCloudDialogflowCxV3DataStoreConnectionSignals');
