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

class GoogleCloudAiplatformV1SchemaPredictPredictionTimeSeriesForecastingPredictionResult extends \Google\Collection
{
  protected $collection_key = 'quantileValues';
  /**
   * @var float[]
   */
  public $quantilePredictions;
  /**
   * @var float[]
   */
  public $quantileValues;
  protected $tftFeatureImportanceType = GoogleCloudAiplatformV1SchemaPredictPredictionTftFeatureImportance::class;
  protected $tftFeatureImportanceDataType = '';
  /**
   * @var float
   */
  public $value;

  /**
   * @param float[]
   */
  public function setQuantilePredictions($quantilePredictions)
  {
    $this->quantilePredictions = $quantilePredictions;
  }
  /**
   * @return float[]
   */
  public function getQuantilePredictions()
  {
    return $this->quantilePredictions;
  }
  /**
   * @param float[]
   */
  public function setQuantileValues($quantileValues)
  {
    $this->quantileValues = $quantileValues;
  }
  /**
   * @return float[]
   */
  public function getQuantileValues()
  {
    return $this->quantileValues;
  }
  /**
   * @param GoogleCloudAiplatformV1SchemaPredictPredictionTftFeatureImportance
   */
  public function setTftFeatureImportance(GoogleCloudAiplatformV1SchemaPredictPredictionTftFeatureImportance $tftFeatureImportance)
  {
    $this->tftFeatureImportance = $tftFeatureImportance;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaPredictPredictionTftFeatureImportance
   */
  public function getTftFeatureImportance()
  {
    return $this->tftFeatureImportance;
  }
  /**
   * @param float
   */
  public function setValue($value)
  {
    $this->value = $value;
  }
  /**
   * @return float
   */
  public function getValue()
  {
    return $this->value;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SchemaPredictPredictionTimeSeriesForecastingPredictionResult::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SchemaPredictPredictionTimeSeriesForecastingPredictionResult');
