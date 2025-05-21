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

namespace Google\Service\Document;

class GoogleCloudDocumentaiV1EvaluationMetrics extends \Google\Model
{
  /**
   * @var float
   */
  public $f1Score;
  /**
   * @var int
   */
  public $falseNegativesCount;
  /**
   * @var int
   */
  public $falsePositivesCount;
  /**
   * @var int
   */
  public $groundTruthDocumentCount;
  /**
   * @var int
   */
  public $groundTruthOccurrencesCount;
  /**
   * @var float
   */
  public $precision;
  /**
   * @var int
   */
  public $predictedDocumentCount;
  /**
   * @var int
   */
  public $predictedOccurrencesCount;
  /**
   * @var float
   */
  public $recall;
  /**
   * @var int
   */
  public $totalDocumentsCount;
  /**
   * @var int
   */
  public $truePositivesCount;

  /**
   * @param float
   */
  public function setF1Score($f1Score)
  {
    $this->f1Score = $f1Score;
  }
  /**
   * @return float
   */
  public function getF1Score()
  {
    return $this->f1Score;
  }
  /**
   * @param int
   */
  public function setFalseNegativesCount($falseNegativesCount)
  {
    $this->falseNegativesCount = $falseNegativesCount;
  }
  /**
   * @return int
   */
  public function getFalseNegativesCount()
  {
    return $this->falseNegativesCount;
  }
  /**
   * @param int
   */
  public function setFalsePositivesCount($falsePositivesCount)
  {
    $this->falsePositivesCount = $falsePositivesCount;
  }
  /**
   * @return int
   */
  public function getFalsePositivesCount()
  {
    return $this->falsePositivesCount;
  }
  /**
   * @param int
   */
  public function setGroundTruthDocumentCount($groundTruthDocumentCount)
  {
    $this->groundTruthDocumentCount = $groundTruthDocumentCount;
  }
  /**
   * @return int
   */
  public function getGroundTruthDocumentCount()
  {
    return $this->groundTruthDocumentCount;
  }
  /**
   * @param int
   */
  public function setGroundTruthOccurrencesCount($groundTruthOccurrencesCount)
  {
    $this->groundTruthOccurrencesCount = $groundTruthOccurrencesCount;
  }
  /**
   * @return int
   */
  public function getGroundTruthOccurrencesCount()
  {
    return $this->groundTruthOccurrencesCount;
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
   * @param int
   */
  public function setPredictedDocumentCount($predictedDocumentCount)
  {
    $this->predictedDocumentCount = $predictedDocumentCount;
  }
  /**
   * @return int
   */
  public function getPredictedDocumentCount()
  {
    return $this->predictedDocumentCount;
  }
  /**
   * @param int
   */
  public function setPredictedOccurrencesCount($predictedOccurrencesCount)
  {
    $this->predictedOccurrencesCount = $predictedOccurrencesCount;
  }
  /**
   * @return int
   */
  public function getPredictedOccurrencesCount()
  {
    return $this->predictedOccurrencesCount;
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
  /**
   * @param int
   */
  public function setTotalDocumentsCount($totalDocumentsCount)
  {
    $this->totalDocumentsCount = $totalDocumentsCount;
  }
  /**
   * @return int
   */
  public function getTotalDocumentsCount()
  {
    return $this->totalDocumentsCount;
  }
  /**
   * @param int
   */
  public function setTruePositivesCount($truePositivesCount)
  {
    $this->truePositivesCount = $truePositivesCount;
  }
  /**
   * @return int
   */
  public function getTruePositivesCount()
  {
    return $this->truePositivesCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDocumentaiV1EvaluationMetrics::class, 'Google_Service_Document_GoogleCloudDocumentaiV1EvaluationMetrics');
