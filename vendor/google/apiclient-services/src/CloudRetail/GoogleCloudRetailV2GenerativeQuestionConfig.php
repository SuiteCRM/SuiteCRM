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

class GoogleCloudRetailV2GenerativeQuestionConfig extends \Google\Collection
{
  protected $collection_key = 'exampleValues';
  /**
   * @var bool
   */
  public $allowedInConversation;
  /**
   * @var string
   */
  public $catalog;
  /**
   * @var string[]
   */
  public $exampleValues;
  /**
   * @var string
   */
  public $facet;
  /**
   * @var string
   */
  public $finalQuestion;
  /**
   * @var float
   */
  public $frequency;
  /**
   * @var string
   */
  public $generatedQuestion;

  /**
   * @param bool
   */
  public function setAllowedInConversation($allowedInConversation)
  {
    $this->allowedInConversation = $allowedInConversation;
  }
  /**
   * @return bool
   */
  public function getAllowedInConversation()
  {
    return $this->allowedInConversation;
  }
  /**
   * @param string
   */
  public function setCatalog($catalog)
  {
    $this->catalog = $catalog;
  }
  /**
   * @return string
   */
  public function getCatalog()
  {
    return $this->catalog;
  }
  /**
   * @param string[]
   */
  public function setExampleValues($exampleValues)
  {
    $this->exampleValues = $exampleValues;
  }
  /**
   * @return string[]
   */
  public function getExampleValues()
  {
    return $this->exampleValues;
  }
  /**
   * @param string
   */
  public function setFacet($facet)
  {
    $this->facet = $facet;
  }
  /**
   * @return string
   */
  public function getFacet()
  {
    return $this->facet;
  }
  /**
   * @param string
   */
  public function setFinalQuestion($finalQuestion)
  {
    $this->finalQuestion = $finalQuestion;
  }
  /**
   * @return string
   */
  public function getFinalQuestion()
  {
    return $this->finalQuestion;
  }
  /**
   * @param float
   */
  public function setFrequency($frequency)
  {
    $this->frequency = $frequency;
  }
  /**
   * @return float
   */
  public function getFrequency()
  {
    return $this->frequency;
  }
  /**
   * @param string
   */
  public function setGeneratedQuestion($generatedQuestion)
  {
    $this->generatedQuestion = $generatedQuestion;
  }
  /**
   * @return string
   */
  public function getGeneratedQuestion()
  {
    return $this->generatedQuestion;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRetailV2GenerativeQuestionConfig::class, 'Google_Service_CloudRetail_GoogleCloudRetailV2GenerativeQuestionConfig');
