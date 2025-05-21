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

class GoogleCloudContactcenterinsightsV1alpha1QaAnswerAnswerValue extends \Google\Model
{
  /**
   * @var bool
   */
  public $boolValue;
  /**
   * @var string
   */
  public $key;
  /**
   * @var bool
   */
  public $naValue;
  public $normalizedScore;
  public $numValue;
  public $potentialScore;
  public $score;
  /**
   * @var string
   */
  public $strValue;

  /**
   * @param bool
   */
  public function setBoolValue($boolValue)
  {
    $this->boolValue = $boolValue;
  }
  /**
   * @return bool
   */
  public function getBoolValue()
  {
    return $this->boolValue;
  }
  /**
   * @param string
   */
  public function setKey($key)
  {
    $this->key = $key;
  }
  /**
   * @return string
   */
  public function getKey()
  {
    return $this->key;
  }
  /**
   * @param bool
   */
  public function setNaValue($naValue)
  {
    $this->naValue = $naValue;
  }
  /**
   * @return bool
   */
  public function getNaValue()
  {
    return $this->naValue;
  }
  public function setNormalizedScore($normalizedScore)
  {
    $this->normalizedScore = $normalizedScore;
  }
  public function getNormalizedScore()
  {
    return $this->normalizedScore;
  }
  public function setNumValue($numValue)
  {
    $this->numValue = $numValue;
  }
  public function getNumValue()
  {
    return $this->numValue;
  }
  public function setPotentialScore($potentialScore)
  {
    $this->potentialScore = $potentialScore;
  }
  public function getPotentialScore()
  {
    return $this->potentialScore;
  }
  public function setScore($score)
  {
    $this->score = $score;
  }
  public function getScore()
  {
    return $this->score;
  }
  /**
   * @param string
   */
  public function setStrValue($strValue)
  {
    $this->strValue = $strValue;
  }
  /**
   * @return string
   */
  public function getStrValue()
  {
    return $this->strValue;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1alpha1QaAnswerAnswerValue::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1alpha1QaAnswerAnswerValue');
