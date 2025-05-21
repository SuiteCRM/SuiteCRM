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

class GoogleCloudAiplatformV1SchemaModelevaluationMetricsForecastingEvaluationMetrics extends \Google\Collection
{
  protected $collection_key = 'quantileMetrics';
  /**
   * @var float
   */
  public $meanAbsoluteError;
  /**
   * @var float
   */
  public $meanAbsolutePercentageError;
  protected $quantileMetricsType = GoogleCloudAiplatformV1SchemaModelevaluationMetricsForecastingEvaluationMetricsQuantileMetricsEntry::class;
  protected $quantileMetricsDataType = 'array';
  /**
   * @var float
   */
  public $rSquared;
  /**
   * @var float
   */
  public $rootMeanSquaredError;
  /**
   * @var float
   */
  public $rootMeanSquaredLogError;
  /**
   * @var float
   */
  public $rootMeanSquaredPercentageError;
  /**
   * @var float
   */
  public $weightedAbsolutePercentageError;

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
  public function setMeanAbsolutePercentageError($meanAbsolutePercentageError)
  {
    $this->meanAbsolutePercentageError = $meanAbsolutePercentageError;
  }
  /**
   * @return float
   */
  public function getMeanAbsolutePercentageError()
  {
    return $this->meanAbsolutePercentageError;
  }
  /**
   * @param GoogleCloudAiplatformV1SchemaModelevaluationMetricsForecastingEvaluationMetricsQuantileMetricsEntry[]
   */
  public function setQuantileMetrics($quantileMetrics)
  {
    $this->quantileMetrics = $quantileMetrics;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaModelevaluationMetricsForecastingEvaluationMetricsQuantileMetricsEntry[]
   */
  public function getQuantileMetrics()
  {
    return $this->quantileMetrics;
  }
  /**
   * @param float
   */
  public function setRSquared($rSquared)
  {
    $this->rSquared = $rSquared;
  }
  /**
   * @return float
   */
  public function getRSquared()
  {
    return $this->rSquared;
  }
  /**
   * @param float
   */
  public function setRootMeanSquaredError($rootMeanSquaredError)
  {
    $this->rootMeanSquaredError = $rootMeanSquaredError;
  }
  /**
   * @return float
   */
  public function getRootMeanSquaredError()
  {
    return $this->rootMeanSquaredError;
  }
  /**
   * @param float
   */
  public function setRootMeanSquaredLogError($rootMeanSquaredLogError)
  {
    $this->rootMeanSquaredLogError = $rootMeanSquaredLogError;
  }
  /**
   * @return float
   */
  public function getRootMeanSquaredLogError()
  {
    return $this->rootMeanSquaredLogError;
  }
  /**
   * @param float
   */
  public function setRootMeanSquaredPercentageError($rootMeanSquaredPercentageError)
  {
    $this->rootMeanSquaredPercentageError = $rootMeanSquaredPercentageError;
  }
  /**
   * @return float
   */
  public function getRootMeanSquaredPercentageError()
  {
    return $this->rootMeanSquaredPercentageError;
  }
  /**
   * @param float
   */
  public function setWeightedAbsolutePercentageError($weightedAbsolutePercentageError)
  {
    $this->weightedAbsolutePercentageError = $weightedAbsolutePercentageError;
  }
  /**
   * @return float
   */
  public function getWeightedAbsolutePercentageError()
  {
    return $this->weightedAbsolutePercentageError;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SchemaModelevaluationMetricsForecastingEvaluationMetrics::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SchemaModelevaluationMetricsForecastingEvaluationMetrics');
