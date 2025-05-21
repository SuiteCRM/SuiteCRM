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

class GoogleCloudDialogflowV3alpha1TurnSignals extends \Google\Collection
{
  protected $collection_key = 'webhookStatuses';
  /**
   * @var bool
   */
  public $agentEscalated;
  /**
   * @var bool
   */
  public $dtmfUsed;
  /**
   * @var string[]
   */
  public $failureReasons;
  /**
   * @var bool
   */
  public $noMatch;
  /**
   * @var bool
   */
  public $noUserInput;
  /**
   * @var bool
   */
  public $reachedEndPage;
  /**
   * @var float
   */
  public $sentimentMagnitude;
  /**
   * @var float
   */
  public $sentimentScore;
  /**
   * @var bool
   */
  public $triggeredAbandonmentEvent;
  /**
   * @var bool
   */
  public $userEscalated;
  /**
   * @var string[]
   */
  public $webhookStatuses;

  /**
   * @param bool
   */
  public function setAgentEscalated($agentEscalated)
  {
    $this->agentEscalated = $agentEscalated;
  }
  /**
   * @return bool
   */
  public function getAgentEscalated()
  {
    return $this->agentEscalated;
  }
  /**
   * @param bool
   */
  public function setDtmfUsed($dtmfUsed)
  {
    $this->dtmfUsed = $dtmfUsed;
  }
  /**
   * @return bool
   */
  public function getDtmfUsed()
  {
    return $this->dtmfUsed;
  }
  /**
   * @param string[]
   */
  public function setFailureReasons($failureReasons)
  {
    $this->failureReasons = $failureReasons;
  }
  /**
   * @return string[]
   */
  public function getFailureReasons()
  {
    return $this->failureReasons;
  }
  /**
   * @param bool
   */
  public function setNoMatch($noMatch)
  {
    $this->noMatch = $noMatch;
  }
  /**
   * @return bool
   */
  public function getNoMatch()
  {
    return $this->noMatch;
  }
  /**
   * @param bool
   */
  public function setNoUserInput($noUserInput)
  {
    $this->noUserInput = $noUserInput;
  }
  /**
   * @return bool
   */
  public function getNoUserInput()
  {
    return $this->noUserInput;
  }
  /**
   * @param bool
   */
  public function setReachedEndPage($reachedEndPage)
  {
    $this->reachedEndPage = $reachedEndPage;
  }
  /**
   * @return bool
   */
  public function getReachedEndPage()
  {
    return $this->reachedEndPage;
  }
  /**
   * @param float
   */
  public function setSentimentMagnitude($sentimentMagnitude)
  {
    $this->sentimentMagnitude = $sentimentMagnitude;
  }
  /**
   * @return float
   */
  public function getSentimentMagnitude()
  {
    return $this->sentimentMagnitude;
  }
  /**
   * @param float
   */
  public function setSentimentScore($sentimentScore)
  {
    $this->sentimentScore = $sentimentScore;
  }
  /**
   * @return float
   */
  public function getSentimentScore()
  {
    return $this->sentimentScore;
  }
  /**
   * @param bool
   */
  public function setTriggeredAbandonmentEvent($triggeredAbandonmentEvent)
  {
    $this->triggeredAbandonmentEvent = $triggeredAbandonmentEvent;
  }
  /**
   * @return bool
   */
  public function getTriggeredAbandonmentEvent()
  {
    return $this->triggeredAbandonmentEvent;
  }
  /**
   * @param bool
   */
  public function setUserEscalated($userEscalated)
  {
    $this->userEscalated = $userEscalated;
  }
  /**
   * @return bool
   */
  public function getUserEscalated()
  {
    return $this->userEscalated;
  }
  /**
   * @param string[]
   */
  public function setWebhookStatuses($webhookStatuses)
  {
    $this->webhookStatuses = $webhookStatuses;
  }
  /**
   * @return string[]
   */
  public function getWebhookStatuses()
  {
    return $this->webhookStatuses;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDialogflowV3alpha1TurnSignals::class, 'Google_Service_Dialogflow_GoogleCloudDialogflowV3alpha1TurnSignals');
