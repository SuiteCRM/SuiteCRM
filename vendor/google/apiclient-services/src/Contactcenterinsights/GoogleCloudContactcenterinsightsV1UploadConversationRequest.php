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

class GoogleCloudContactcenterinsightsV1UploadConversationRequest extends \Google\Model
{
  protected $conversationType = GoogleCloudContactcenterinsightsV1Conversation::class;
  protected $conversationDataType = '';
  /**
   * @var string
   */
  public $conversationId;
  /**
   * @var string
   */
  public $parent;
  protected $redactionConfigType = GoogleCloudContactcenterinsightsV1RedactionConfig::class;
  protected $redactionConfigDataType = '';
  protected $speechConfigType = GoogleCloudContactcenterinsightsV1SpeechConfig::class;
  protected $speechConfigDataType = '';

  /**
   * @param GoogleCloudContactcenterinsightsV1Conversation
   */
  public function setConversation(GoogleCloudContactcenterinsightsV1Conversation $conversation)
  {
    $this->conversation = $conversation;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1Conversation
   */
  public function getConversation()
  {
    return $this->conversation;
  }
  /**
   * @param string
   */
  public function setConversationId($conversationId)
  {
    $this->conversationId = $conversationId;
  }
  /**
   * @return string
   */
  public function getConversationId()
  {
    return $this->conversationId;
  }
  /**
   * @param string
   */
  public function setParent($parent)
  {
    $this->parent = $parent;
  }
  /**
   * @return string
   */
  public function getParent()
  {
    return $this->parent;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1RedactionConfig
   */
  public function setRedactionConfig(GoogleCloudContactcenterinsightsV1RedactionConfig $redactionConfig)
  {
    $this->redactionConfig = $redactionConfig;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1RedactionConfig
   */
  public function getRedactionConfig()
  {
    return $this->redactionConfig;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1SpeechConfig
   */
  public function setSpeechConfig(GoogleCloudContactcenterinsightsV1SpeechConfig $speechConfig)
  {
    $this->speechConfig = $speechConfig;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1SpeechConfig
   */
  public function getSpeechConfig()
  {
    return $this->speechConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1UploadConversationRequest::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1UploadConversationRequest');
