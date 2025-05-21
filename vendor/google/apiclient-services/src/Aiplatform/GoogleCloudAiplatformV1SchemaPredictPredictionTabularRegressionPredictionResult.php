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

class GoogleCloudAiplatformV1SchemaPredictPredictionTabularRegressionPredictionResult extends \Google\Collection
{
  protected $collection_key = 'quantileValues';
  /**
   * @var float
   */
  public $lowerBound;
  /**
   * @var float[]
   */
  public $quantilePredictions;
  /**
   * @var float[]
   */
  public $quantileValues;
  /**
   * @var float
   */
  public $upperBound;
  /**
   * @var float
   */
  public $value;

  /**
   * @param float
   */
  public function setLowerBound($lowerBound)
  {
    $this->lowerBound = $lowerBound;
  }
  /**
   * @return float
   */
  public function getLowerBound()
  {
    return $this->lowerBound;
  }
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
   * @param float
   */
  public function setUpperBound($upperBound)
  {
    $this->upperBound = $upperBound;
  }
  /**
   * @return float
   */
  public function getUpperBound()
  {
    return $this->upperBound;
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
class_alias(GoogleCloudAiplatformV1SchemaPredictPredictionTabularRegressionPredictionResult::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SchemaPredictPredictionTabularRegressionPredictionResult');
