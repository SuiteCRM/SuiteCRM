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

class GoogleCloudDiscoveryengineV1SessionTurn extends \Google\Model
{
  /**
   * @var string
   */
  public $answer;
  protected $detailedAnswerType = GoogleCloudDiscoveryengineV1Answer::class;
  protected $detailedAnswerDataType = '';
  protected $queryType = GoogleCloudDiscoveryengineV1Query::class;
  protected $queryDataType = '';

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
   * @param GoogleCloudDiscoveryengineV1Answer
   */
  public function setDetailedAnswer(GoogleCloudDiscoveryengineV1Answer $detailedAnswer)
  {
    $this->detailedAnswer = $detailedAnswer;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1Answer
   */
  public function getDetailedAnswer()
  {
    return $this->detailedAnswer;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1Query
   */
  public function setQuery(GoogleCloudDiscoveryengineV1Query $query)
  {
    $this->query = $query;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1Query
   */
  public function getQuery()
  {
    return $this->query;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1SessionTurn::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1SessionTurn');
