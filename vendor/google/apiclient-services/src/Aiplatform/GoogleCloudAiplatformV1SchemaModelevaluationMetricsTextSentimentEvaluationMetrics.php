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

class GoogleCloudAiplatformV1SchemaModelevaluationMetricsTextSentimentEvaluationMetrics extends \Google\Model
{
  protected $confusionMatrixType = GoogleCloudAiplatformV1SchemaModelevaluationMetricsConfusionMatrix::class;
  protected $confusionMatrixDataType = '';
  /**
   * @var float
   */
  public $f1Score;
  /**
   * @var float
   */
  public $linearKappa;
  /**
   * @var float
   */
  public $meanAbsoluteError;
  /**
   * @var float
   */
  public $meanSquaredError;
  /**
   * @var float
   */
  public $precision;
  /**
   * @var float
   */
  public $quadraticKappa;
  /**
   * @var float
   */
  public $recall;

  /**
   * @param GoogleCloudAiplatformV1SchemaModelevaluationMetricsConfusionMatrix
   */
  public function setConfusionMatrix(GoogleCloudAiplatformV1SchemaModelevaluationMetricsConfusionMatrix $confusionMatrix)
  {
    $this->confusionMatrix = $confusionMatrix;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaModelevaluationMetricsConfusionMatrix
   */
  public function getConfusionMatrix()
  {
    return $this->confusionMatrix;
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
   * @param float
   */
  public function setLinearKappa($linearKappa)
  {
    $this->linearKappa = $linearKappa;
  }
  /**
   * @return float
   */
  public function getLinearKappa()
  {
    return $this->linearKappa;
  }
  /**
   * @param float
   */
  public function setMeanAbsoluteError($meanAbsoluteError)
  {
    $this->meanAbsoluteError = $meanAbsoluteError;
  }
  /**
   * @return float
   */
  public function getMeanAbsoluteError()
  {
    return $this->meanAbsoluteError;
  }
  /**
   * @param float
   */
  public function setMeanSquaredError($meanSquaredError)
  {
    $this->meanSquaredError = $meanSquaredError;
  }
  /**
   * @return float
   */
  public function getMeanSquaredError()
  {
    return $this->meanSquaredError;
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
  public function setQuadraticKappa($quadraticKappa)
  {
    $this->quadraticKappa = $quadraticKappa;
  }
  /**
   * @return float
   */
  public function getQuadraticKappa()
  {
    return $this->quadraticKappa;
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
class_alias(GoogleCloudAiplatformV1SchemaModelevaluationMetricsTextSentimentEvaluationMetrics::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SchemaModelevaluationMetricsTextSentimentEvaluationMetrics');
