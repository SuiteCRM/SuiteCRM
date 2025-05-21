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

class GoogleCloudDialogflowV2beta1DialogflowAssistAnswer extends \Google\Model
{
  /**
   * @var string
   */
  public $answerRecord;
  protected $intentSuggestionType = GoogleCloudDialogflowV2beta1IntentSuggestion::class;
  protected $intentSuggestionDataType = '';
  protected $queryResultType = GoogleCloudDialogflowV2beta1QueryResult::class;
  protected $queryResultDataType = '';

  /**
   * @param string
   */
  public function setAnswerRecord($answerRecord)
  {
    $this->answerRecord = $answerRecord;
  }
  /**
   * @return string
   */
  public function getAnswerRecord()
  {
    return $this->answerRecord;
  }
  /**
   * @param GoogleCloudDialogflowV2beta1IntentSuggestion
   */
  public function setIntentSuggestion(GoogleCloudDialogflowV2beta1IntentSuggestion $intentSuggestion)
  {
    $this->intentSuggestion = $intentSuggestion;
  }
  /**
   * @return GoogleCloudDialogflowV2beta1IntentSuggestion
   */
  public function getIntentSuggestion()
  {
    return $this->intentSuggestion;
  }
  /**
   * @param GoogleCloudDialogflowV2beta1QueryResult
   */
  public function setQueryResult(GoogleCloudDialogflowV2beta1QueryResult $queryResult)
  {
    $this->queryResult = $queryResult;
  }
  /**
   * @return GoogleCloudDialogflowV2beta1QueryResult
   */
  public function getQueryResult()
  {
    return $this->queryResult;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDialogflowV2beta1DialogflowAssistAnswer::class, 'Google_Service_Dialogflow_GoogleCloudDialogflowV2beta1DialogflowAssistAnswer');
