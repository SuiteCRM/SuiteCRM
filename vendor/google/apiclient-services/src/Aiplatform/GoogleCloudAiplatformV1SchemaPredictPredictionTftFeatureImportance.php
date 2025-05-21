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

class GoogleCloudAiplatformV1SchemaPredictPredictionTftFeatureImportance extends \Google\Collection
{
  protected $collection_key = 'horizonWeights';
  /**
   * @var string[]
   */
  public $attributeColumns;
  /**
   * @var float[]
   */
  public $attributeWeights;
  /**
   * @var string[]
   */
  public $contextColumns;
  /**
   * @var float[]
   */
  public $contextWeights;
  /**
   * @var string[]
   */
  public $horizonColumns;
  /**
   * @var float[]
   */
  public $horizonWeights;

  /**
   * @param string[]
   */
  public function setAttributeColumns($attributeColumns)
  {
    $this->attributeColumns = $attributeColumns;
  }
  /**
   * @return string[]
   */
  public function getAttributeColumns()
  {
    return $this->attributeColumns;
  }
  /**
   * @param float[]
   */
  public function setAttributeWeights($attributeWeights)
  {
    $this->attributeWeights = $attributeWeights;
  }
  /**
   * @return float[]
   */
  public function getAttributeWeights()
  {
    return $this->attributeWeights;
  }
  /**
   * @param string[]
   */
  public function setContextColumns($contextColumns)
  {
    $this->contextColumns = $contextColumns;
  }
  /**
   * @return string[]
   */
  public function getContextColumns()
  {
    return $this->contextColumns;
  }
  /**
   * @param float[]
   */
  public function setContextWeights($contextWeights)
  {
    $this->contextWeights = $contextWeights;
  }
  /**
   * @return float[]
   */
  public function getContextWeights()
  {
    return $this->contextWeights;
  }
  /**
   * @param string[]
   */
  public function setHorizonColumns($horizonColumns)
  {
    $this->horizonColumns = $horizonColumns;
  }
  /**
   * @return string[]
   */
  public function getHorizonColumns()
  {
    return $this->horizonColumns;
  }
  /**
   * @param float[]
   */
  public function setHorizonWeights($horizonWeights)
  {
    $this->horizonWeights = $horizonWeights;
  }
  /**
   * @return float[]
   */
  public function getHorizonWeights()
  {
    return $this->horizonWeights;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SchemaPredictPredictionTftFeatureImportance::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SchemaPredictPredictionTftFeatureImportance');
