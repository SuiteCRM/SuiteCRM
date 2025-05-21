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

class XPSImageSegmentationEvaluationMetricsConfidenceMetricsEntry extends \Google\Model
{
  /**
   * @var float
   */
  public $confidenceThreshold;
  protected $confusionMatrixType = XPSConfusionMatrix::class;
  protected $confusionMatrixDataType = '';
  /**
   * @var float
   */
  public $diceScoreCoefficient;
  /**
   * @var float
   */
  public $iouScore;
  /**
   * @var float
   */
  public $precision;
  /**
   * @var float
   */
  public $recall;

  /**
   * @param float
   */
  public function setConfidenceThreshold($confidenceThreshold)
  {
    $this->confidenceThreshold = $confidenceThreshold;
  }
  /**
   * @return float
   */
  public function getConfidenceThreshold()
  {
    return $this->confidenceThreshold;
  }
  /**
   * @param XPSConfusionMatrix
   */
  public function setConfusionMatrix(XPSConfusionMatrix $confusionMatrix)
  {
    $this->confusionMatrix = $confusionMatrix;
  }
  /**
   * @return XPSConfusionMatrix
   */
  public function getConfusionMatrix()
  {
    return $this->confusionMatrix;
  }
  /**
   * @param float
   */
  public function setDiceScoreCoefficient($diceScoreCoefficient)
  {
    $this->diceScoreCoefficient = $diceScoreCoefficient;
  }
  /**
   * @return float
   */
  public function getDiceScoreCoefficient()
  {
    return $this->diceScoreCoefficient;
  }
  /**
   * @param float
   */
  public function setIouScore($iouScore)
  {
    $this->iouScore = $iouScore;
  }
  /**
   * @return float
   */
  public function getIouScore()
  {
    return $this->iouScore;
  }
  /**
   * @param float
   */
  public function setPrecision($precision)
  {
    $this->precision = $precision;
  }
  /**
   * @return float
   */
  public function getPrecision()
  {
    return $this->precision;
  }
  /**
   * @param float
   */
  public function setRecall($recall)
  {
    $this->recall = $recall;
  }
  /**
   * @return float
   */
  public function getRecall()
  {
    return $this->recall;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSImageSegmentationEvaluationMetricsConfidenceMetricsEntry::class, 'Google_Service_CloudNaturalLanguage_XPSImageSegmentationEvaluationMetricsConfidenceMetricsEntry');
