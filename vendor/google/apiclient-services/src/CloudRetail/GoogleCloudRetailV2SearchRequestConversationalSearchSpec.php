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

namespace Google\Service\CloudRetail;

class GoogleCloudRetailV2SearchRequestConversationalSearchSpec extends \Google\Model
{
  /**
   * @var string
   */
  public $conversationId;
  /**
   * @var bool
   */
  public $followupConversationRequested;
  protected $userAnswerType = GoogleCloudRetailV2SearchRequestConversationalSearchSpecUserAnswer::class;
  protected $userAnswerDataType = '';

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
   * @param bool
   */
  public function setFollowupConversationRequested($followupConversationRequested)
  {
    $this->followupConversationRequested = $followupConversationRequested;
  }
  /**
   * @return bool
   */
  public function getFollowupConversationRequested()
  {
    return $this->followupConversationRequested;
  }
  /**
   * @param GoogleCloudRetailV2SearchRequestConversationalSearchSpecUserAnswer
   */
  public function setUserAnswer(GoogleCloudRetailV2SearchRequestConversationalSearchSpecUserAnswer $userAnswer)
  {
    $this->userAnswer = $userAnswer;
  }
  /**
   * @return GoogleCloudRetailV2SearchRequestConversationalSearchSpecUserAnswer
   */
  public function getUserAnswer()
  {
    return $this->userAnswer;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRetailV2SearchRequestConversationalSearchSpec::class, 'Google_Service_CloudRetail_GoogleCloudRetailV2SearchRequestConversationalSearchSpec');
