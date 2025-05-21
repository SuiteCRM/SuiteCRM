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

class GoogleCloudAiplatformV1EvaluateDatasetRequest extends \Google\Collection
{
  protected $collection_key = 'metrics';
  protected $autoraterConfigType = GoogleCloudAiplatformV1AutoraterConfig::class;
  protected $autoraterConfigDataType = '';
  protected $datasetType = GoogleCloudAiplatformV1EvaluationDataset::class;
  protected $datasetDataType = '';
  protected $metricsType = GoogleCloudAiplatformV1Metric::class;
  protected $metricsDataType = 'array';
  protected $outputConfigType = GoogleCloudAiplatformV1OutputConfig::class;
  protected $outputConfigDataType = '';

  /**
   * @param GoogleCloudAiplatformV1AutoraterConfig
   */
  public function setAutoraterConfig(GoogleCloudAiplatformV1AutoraterConfig $autoraterConfig)
  {
    $this->autoraterConfig = $autoraterConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1AutoraterConfig
   */
  public function getAutoraterConfig()
  {
    return $this->autoraterConfig;
  }
  /**
   * @param GoogleCloudAiplatformV1EvaluationDataset
   */
  public function setDataset(GoogleCloudAiplatformV1EvaluationDataset $dataset)
  {
    $this->dataset = $dataset;
  }
  /**
   * @return GoogleCloudAiplatformV1EvaluationDataset
   */
  public function getDataset()
  {
    return $this->dataset;
  }
  /**
   * @param GoogleCloudAiplatformV1Metric[]
   */
  public function setMetrics($metrics)
  {
    $this->metrics = $metrics;
  }
  /**
   * @return GoogleCloudAiplatformV1Metric[]
   */
  public function getMetrics()
  {
    return $this->metrics;
  }
  /**
   * @param GoogleCloudAiplatformV1OutputConfig
   */
  public function setOutputConfig(GoogleCloudAiplatformV1OutputConfig $outputConfig)
  {
    $this->outputConfig = $outputConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1OutputConfig
   */
  public function getOutputConfig()
  {
    return $this->outputConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1EvaluateDatasetRequest::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1EvaluateDatasetRequest');
