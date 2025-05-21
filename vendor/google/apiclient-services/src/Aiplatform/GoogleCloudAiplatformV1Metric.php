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

class GoogleCloudAiplatformV1Metric extends \Google\Collection
{
  protected $collection_key = 'aggregationMetrics';
  /**
   * @var string[]
   */
  public $aggregationMetrics;
  protected $bleuSpecType = GoogleCloudAiplatformV1BleuSpec::class;
  protected $bleuSpecDataType = '';
  protected $exactMatchSpecType = GoogleCloudAiplatformV1ExactMatchSpec::class;
  protected $exactMatchSpecDataType = '';
  protected $pairwiseMetricSpecType = GoogleCloudAiplatformV1PairwiseMetricSpec::class;
  protected $pairwiseMetricSpecDataType = '';
  protected $pointwiseMetricSpecType = GoogleCloudAiplatformV1PointwiseMetricSpec::class;
  protected $pointwiseMetricSpecDataType = '';
  protected $rougeSpecType = GoogleCloudAiplatformV1RougeSpec::class;
  protected $rougeSpecDataType = '';

  /**
   * @param string[]
   */
  public function setAggregationMetrics($aggregationMetrics)
  {
    $this->aggregationMetrics = $aggregationMetrics;
  }
  /**
   * @return string[]
   */
  public function getAggregationMetrics()
  {
    return $this->aggregationMetrics;
  }
  /**
   * @param GoogleCloudAiplatformV1BleuSpec
   */
  public function setBleuSpec(GoogleCloudAiplatformV1BleuSpec $bleuSpec)
  {
    $this->bleuSpec = $bleuSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1BleuSpec
   */
  public function getBleuSpec()
  {
    return $this->bleuSpec;
  }
  /**
   * @param GoogleCloudAiplatformV1ExactMatchSpec
   */
  public function setExactMatchSpec(GoogleCloudAiplatformV1ExactMatchSpec $exactMatchSpec)
  {
    $this->exactMatchSpec = $exactMatchSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1ExactMatchSpec
   */
  public function getExactMatchSpec()
  {
    return $this->exactMatchSpec;
  }
  /**
   * @param GoogleCloudAiplatformV1PairwiseMetricSpec
   */
  public function setPairwiseMetricSpec(GoogleCloudAiplatformV1PairwiseMetricSpec $pairwiseMetricSpec)
  {
    $this->pairwiseMetricSpec = $pairwiseMetricSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1PairwiseMetricSpec
   */
  public function getPairwiseMetricSpec()
  {
    return $this->pairwiseMetricSpec;
  }
  /**
   * @param GoogleCloudAiplatformV1PointwiseMetricSpec
   */
  public function setPointwiseMetricSpec(GoogleCloudAiplatformV1PointwiseMetricSpec $pointwiseMetricSpec)
  {
    $this->pointwiseMetricSpec = $pointwiseMetricSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1PointwiseMetricSpec
   */
  public function getPointwiseMetricSpec()
  {
    return $this->pointwiseMetricSpec;
  }
  /**
   * @param GoogleCloudAiplatformV1RougeSpec
   */
  public function setRougeSpec(GoogleCloudAiplatformV1RougeSpec $rougeSpec)
  {
    $this->rougeSpec = $rougeSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1RougeSpec
   */
  public function getRougeSpec()
  {
    return $this->rougeSpec;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1Metric::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1Metric');
