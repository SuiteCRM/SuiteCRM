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

class GoogleCloudDiscoveryengineV1AnswerQueryResponse extends \Google\Model
{
  protected $answerType = GoogleCloudDiscoveryengineV1Answer::class;
  protected $answerDataType = '';
  /**
   * @var string
   */
  public $answerQueryToken;
  protected $sessionType = GoogleCloudDiscoveryengineV1Session::class;
  protected $sessionDataType = '';

  /**
   * @param GoogleCloudDiscoveryengineV1Answer
   */
  public function setAnswer(GoogleCloudDiscoveryengineV1Answer $answer)
  {
    $this->answer = $answer;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1Answer
   */
  public function getAnswer()
  {
    return $this->answer;
  }
  /**
   * @param string
   */
  public function setAnswerQueryToken($answerQueryToken)
  {
    $this->answerQueryToken = $answerQueryToken;
  }
  /**
   * @return string
   */
  public function getAnswerQueryToken()
  {
    return $this->answerQueryToken;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1Session
   */
  public function setSession(GoogleCloudDiscoveryengineV1Session $session)
  {
    $this->session = $session;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1Session
   */
  public function getSession()
  {
    return $this->session;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1AnswerQueryResponse::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1AnswerQueryResponse');
