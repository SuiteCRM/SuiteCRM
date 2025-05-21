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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1SchemaModelevaluationMetricsPairwiseTextGenerationEvaluationMetrics extends \Google\Model
{
  /**
   * @var float
   */
  public $accuracy;
  /**
   * @var float
   */
  public $baselineModelWinRate;
  /**
   * @var float
   */
  public $cohensKappa;
  /**
   * @var float
   */
  public $f1Score;
  /**
   * @var string
   */
  public $falseNegativeCount;
  /**
   * @var string
   */
  public $falsePositiveCount;
  /**
   * @var float
   */
  public $humanPreferenceBaselineModelWinRate;
  /**
   * @var float
   */
  public $humanPreferenceModelWinRate;
  /**
   * @var float
   */
  public $modelWinRate;
  /**
   * @var float
   */
  public $precision;
  /**
   * @var float
   */
  public $recall;
  /**
   * @var string
   */
  public $trueNegativeCount;
  /**
   * @var string
   */
  public $truePositiveCount;

  /**
   * @param float
   */
  public function setAccuracy($accuracy)
  {
    $this->accuracy = $accuracy;
  }
  /**
   * @return float
   */
  public function getAccuracy()
  {
    return $this->accuracy;
  }
  /**
   * @param float
   */
  public function setBaselineModelWinRate($baselineModelWinRate)
  {
    $this->baselineModelWinRate = $baselineModelWinRate;
  }
  /**
   * @return float
   */
  public function getBaselineModelWinRate()
  {
    return $this->baselineModelWinRate;
  }
  /**
   * @param float
   */
  public function setCohensKappa($cohensKappa)
  {
    $this->cohensKappa = $cohensKappa;
  }
  /**
   * @return float
   */
  public function getCohensKappa()
  {
    return $this->cohensKappa;
  }
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
   * @param string
   */
  public function setFalseNegativeCount($falseNegativeCount)
  {
    $this->falseNegativeCount = $falseNegativeCount;
  }
  /**
   * @return string
   */
  public function getFalseNegativeCount()
  {
    return $this->falseNegativeCount;
  }
  /**
   * @param string
   */
  public function setFalsePositiveCount($falsePositiveCount)
  {
    $this->falsePositiveCount = $falsePositiveCount;
  }
  /**
   * @return string
   */
  public function getFalsePositiveCount()
  {
    return $this->falsePositiveCount;
  }
  /**
   * @param float
   */
  public function setHumanPreferenceBaselineModelWinRate($humanPreferenceBaselineModelWinRate)
  {
    $this->humanPreferenceBaselineModelWinRate = $humanPreferenceBaselineModelWinRate;
  }
  /**
   * @return float
   */
  public function getHumanPreferenceBaselineModelWinRate()
  {
    return $this->humanPreferenceBaselineModelWinRate;
  }
  /**
   * @param float
   */
  public function setHumanPreferenceModelWinRate($humanPreferenceModelWinRate)
  {
    $this->humanPreferenceModelWinRate = $humanPreferenceModelWinRate;
  }
  /**
   * @return float
   */
  public function getHumanPreferenceModelWinRate()
  {
    return $this->humanPreferenceModelWinRate;
  }
  /**
   * @param float
   */
  public function setModelWinRate($modelWinRate)
  {
    $this->modelWinRate = $modelWinRate;
  }
  /**
   * @return float
   */
  public function getModelWinRate()
  {
    return $this->modelWinRate;
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
  /**
   * @param string
   */
  public function setTrueNegativeCount($trueNegativeCount)
  {
    $this->trueNegativeCount = $trueNegativeCount;
  }
  /**
   * @return string
   */
  public function getTrueNegativeCount()
  {
    return $this->trueNegativeCount;
  }
  /**
   * @param string
   */
  public function setTruePositiveCount($truePositiveCount)
  {
    $this->truePositiveCount = $truePositiveCount;
  }
  /**
   * @return string
   */
  public function getTruePositiveCount()
  {
    return $this->truePositiveCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SchemaModelevaluationMetricsPairwiseTextGenerationEvaluationMetrics::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SchemaModelevaluationMetricsPairwiseTextGenerationEvaluationMetrics');
