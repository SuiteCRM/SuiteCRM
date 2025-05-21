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

class GoogleCloudDialogflowCxV3DataStoreConnectionSignalsSafetySignals extends \Google\Model
{
  /**
   * @var string
   */
  public $bannedPhraseMatch;
  /**
   * @var string
   */
  public $decision;
  /**
   * @var string
   */
  public $matchedBannedPhrase;

  /**
   * @param string
   */
  public function setBannedPhraseMatch($bannedPhraseMatch)
  {
    $this->bannedPhraseMatch = $bannedPhraseMatch;
  }
  /**
   * @return string
   */
  public function getBannedPhraseMatch()
  {
    return $this->bannedPhraseMatch;
  }
  /**
   * @param string
   */
  public function setDecision($decision)
  {
    $this->decision = $decision;
  }
  /**
   * @return string
   */
  public function getDecision()
  {
    return $this->decision;
  }
  /**
   * @param string
   */
  public function setMatchedBannedPhrase($matchedBannedPhrase)
  {
    $this->matchedBannedPhrase = $matchedBannedPhrase;
  }
  /**
   * @return string
   */
  public function getMatchedBannedPhrase()
  {
    return $this->matchedBannedPhrase;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDialogflowCxV3DataStoreConnectionSignalsSafetySignals::class, 'Google_Service_Dialogflow_GoogleCloudDialogflowCxV3DataStoreConnectionSignalsSafetySignals');
