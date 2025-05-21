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

class GoogleCloudContactcenterinsightsV1ConversationSummarizationSuggestionData extends \Google\Model
{
  /**
   * @var string
   */
  public $answerRecord;
  /**
   * @var float
   */
  public $confidence;
  /**
   * @var string
   */
  public $conversationModel;
  /**
   * @var string[]
   */
  public $metadata;
  /**
   * @var string
   */
  public $text;
  /**
   * @var string[]
   */
  public $textSections;

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
   * @param float
   */
  public function setConfidence($confidence)
  {
    $this->confidence = $confidence;
  }
  /**
   * @return float
   */
  public function getConfidence()
  {
    return $this->confidence;
  }
  /**
   * @param string
   */
  public function setConversationModel($conversationModel)
  {
    $this->conversationModel = $conversationModel;
  }
  /**
   * @return string
   */
  public function getConversationModel()
  {
    return $this->conversationModel;
  }
  /**
   * @param string[]
   */
  public function setMetadata($metadata)
  {
    $this->metadata = $metadata;
  }
  /**
   * @return string[]
   */
  public function getMetadata()
  {
    return $this->metadata;
  }
  /**
   * @param string
   */
  public function setText($text)
  {
    $this->text = $text;
  }
  /**
   * @return string
   */
  public function getText()
  {
    return $this->text;
  }
  /**
   * @param string[]
   */
  public function setTextSections($textSections)
  {
    $this->textSections = $textSections;
  }
  /**
   * @return string[]
   */
  public function getTextSections()
  {
    return $this->textSections;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1ConversationSummarizationSuggestionData::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1ConversationSummarizationSuggestionData');
