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

class GoogleCloudAiplatformV1ModelMonitoringObjectiveConfigTrainingDataset extends \Google\Model
{
  protected $bigquerySourceType = GoogleCloudAiplatformV1BigQuerySource::class;
  protected $bigquerySourceDataType = '';
  /**
   * @var string
   */
  public $dataFormat;
  /**
   * @var string
   */
  public $dataset;
  protected $gcsSourceType = GoogleCloudAiplatformV1GcsSource::class;
  protected $gcsSourceDataType = '';
  protected $loggingSamplingStrategyType = GoogleCloudAiplatformV1SamplingStrategy::class;
  protected $loggingSamplingStrategyDataType = '';
  /**
   * @var string
   */
  public $targetField;

  /**
   * @param GoogleCloudAiplatformV1BigQuerySource
   */
  public function setBigquerySource(GoogleCloudAiplatformV1BigQuerySource $bigquerySource)
  {
    $this->bigquerySource = $bigquerySource;
  }
  /**
   * @return GoogleCloudAiplatformV1BigQuerySource
   */
  public function getBigquerySource()
  {
    return $this->bigquerySource;
  }
  /**
   * @param string
   */
  public function setDataFormat($dataFormat)
  {
    $this->dataFormat = $dataFormat;
  }
  /**
   * @return string
   */
  public function getDataFormat()
  {
    return $this->dataFormat;
  }
  /**
   * @param string
   */
  public function setDataset($dataset)
  {
    $this->dataset = $dataset;
  }
  /**
   * @return string
   */
  public function getDataset()
  {
    return $this->dataset;
  }
  /**
   * @param GoogleCloudAiplatformV1GcsSource
   */
  public function setGcsSource(GoogleCloudAiplatformV1GcsSource $gcsSource)
  {
    $this->gcsSource = $gcsSource;
  }
  /**
   * @return GoogleCloudAiplatformV1GcsSource
   */
  public function getGcsSource()
  {
    return $this->gcsSource;
  }
  /**
   * @param GoogleCloudAiplatformV1SamplingStrategy
   */
  public function setLoggingSamplingStrategy(GoogleCloudAiplatformV1SamplingStrategy $loggingSamplingStrategy)
  {
    $this->loggingSamplingStrategy = $loggingSamplingStrategy;
  }
  /**
   * @return GoogleCloudAiplatformV1SamplingStrategy
   */
  public function getLoggingSamplingStrategy()
  {
    return $this->loggingSamplingStrategy;
  }
  /**
   * @param string
   */
  public function setTargetField($targetField)
  {
    $this->targetField = $targetField;
  }
  /**
   * @return string
   */
  public function getTargetField()
  {
    return $this->targetField;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ModelMonitoringObjectiveConfigTrainingDataset::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ModelMonitoringObjectiveConfigTrainingDataset');
