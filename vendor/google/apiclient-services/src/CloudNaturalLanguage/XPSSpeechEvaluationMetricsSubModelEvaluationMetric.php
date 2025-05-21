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

namespace Google\Service\CloudNaturalLanguage;

class XPSSpeechEvaluationMetricsSubModelEvaluationMetric extends \Google\Model
{
  /**
   * @var string
   */
  public $biasingModelType;
  /**
   * @var bool
   */
  public $isEnhancedModel;
  /**
   * @var int
   */
  public $numDeletions;
  /**
   * @var int
   */
  public $numInsertions;
  /**
   * @var int
   */
  public $numSubstitutions;
  /**
   * @var int
   */
  public $numUtterances;
  /**
   * @var int
   */
  public $numWords;
  public $sentenceAccuracy;
  public $wer;

  /**
   * @param string
   */
  public function setBiasingModelType($biasingModelType)
  {
    $this->biasingModelType = $biasingModelType;
  }
  /**
   * @return string
   */
  public function getBiasingModelType()
  {
    return $this->biasingModelType;
  }
  /**
   * @param bool
   */
  public function setIsEnhancedModel($isEnhancedModel)
  {
    $this->isEnhancedModel = $isEnhancedModel;
  }
  /**
   * @return bool
   */
  public function getIsEnhancedModel()
  {
    return $this->isEnhancedModel;
  }
  /**
   * @param int
   */
  public function setNumDeletions($numDeletions)
  {
    $this->numDeletions = $numDeletions;
  }
  /**
   * @return int
   */
  public function getNumDeletions()
  {
    return $this->numDeletions;
  }
  /**
   * @param int
   */
  public function setNumInsertions($numInsertions)
  {
    $this->numInsertions = $numInsertions;
  }
  /**
   * @return int
   */
  public function getNumInsertions()
  {
    return $this->numInsertions;
  }
  /**
   * @param int
   */
  public function setNumSubstitutions($numSubstitutions)
  {
    $this->numSubstitutions = $numSubstitutions;
  }
  /**
   * @return int
   */
  public function getNumSubstitutions()
  {
    return $this->numSubstitutions;
  }
  /**
   * @param int
   */
  public function setNumUtterances($numUtterances)
  {
    $this->numUtterances = $numUtterances;
  }
  /**
   * @return int
   */
  public function getNumUtterances()
  {
    return $this->numUtterances;
  }
  /**
   * @param int
   */
  public function setNumWords($numWords)
  {
    $this->numWords = $numWords;
  }
  /**
   * @return int
   */
  public function getNumWords()
  {
    return $this->numWords;
  }
  public function setSentenceAccuracy($sentenceAccuracy)
  {
    $this->sentenceAccuracy = $sentenceAccuracy;
  }
  public function getSentenceAccuracy()
  {
    return $this->sentenceAccuracy;
  }
  public function setWer($wer)
  {
    $this->wer = $wer;
  }
  public function getWer()
  {
    return $this->wer;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSSpeechEvaluationMetricsSubModelEvaluationMetric::class, 'Google_Service_CloudNaturalLanguage_XPSSpeechEvaluationMetricsSubModelEvaluationMetric');
