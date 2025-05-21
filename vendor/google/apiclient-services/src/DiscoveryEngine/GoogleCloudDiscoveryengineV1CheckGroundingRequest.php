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

class GoogleCloudDiscoveryengineV1CheckGroundingRequest extends \Google\Collection
{
  protected $collection_key = 'facts';
  /**
   * @var string
   */
  public $answerCandidate;
  protected $factsType = GoogleCloudDiscoveryengineV1GroundingFact::class;
  protected $factsDataType = 'array';
  protected $groundingSpecType = GoogleCloudDiscoveryengineV1CheckGroundingSpec::class;
  protected $groundingSpecDataType = '';
  /**
   * @var string[]
   */
  public $userLabels;

  /**
   * @param string
   */
  public function setAnswerCandidate($answerCandidate)
  {
    $this->answerCandidate = $answerCandidate;
  }
  /**
   * @return string
   */
  public function getAnswerCandidate()
  {
    return $this->answerCandidate;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1GroundingFact[]
   */
  public function setFacts($facts)
  {
    $this->facts = $facts;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1GroundingFact[]
   */
  public function getFacts()
  {
    return $this->facts;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1CheckGroundingSpec
   */
  public function setGroundingSpec(GoogleCloudDiscoveryengineV1CheckGroundingSpec $groundingSpec)
  {
    $this->groundingSpec = $groundingSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1CheckGroundingSpec
   */
  public function getGroundingSpec()
  {
    return $this->groundingSpec;
  }
  /**
   * @param string[]
   */
  public function setUserLabels($userLabels)
  {
    $this->userLabels = $userLabels;
  }
  /**
   * @return string[]
   */
  public function getUserLabels()
  {
    return $this->userLabels;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1CheckGroundingRequest::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1CheckGroundingRequest');
