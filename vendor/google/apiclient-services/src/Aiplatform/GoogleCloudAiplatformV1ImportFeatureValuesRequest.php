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

class GoogleCloudAiplatformV1ImportFeatureValuesRequest extends \Google\Collection
{
  protected $collection_key = 'featureSpecs';
  protected $avroSourceType = GoogleCloudAiplatformV1AvroSource::class;
  protected $avroSourceDataType = '';
  protected $bigquerySourceType = GoogleCloudAiplatformV1BigQuerySource::class;
  protected $bigquerySourceDataType = '';
  protected $csvSourceType = GoogleCloudAiplatformV1CsvSource::class;
  protected $csvSourceDataType = '';
  /**
   * @var bool
   */
  public $disableIngestionAnalysis;
  /**
   * @var bool
   */
  public $disableOnlineServing;
  /**
   * @var string
   */
  public $entityIdField;
  protected $featureSpecsType = GoogleCloudAiplatformV1ImportFeatureValuesRequestFeatureSpec::class;
  protected $featureSpecsDataType = 'array';
  /**
   * @var string
   */
  public $featureTime;
  /**
   * @var string
   */
  public $featureTimeField;
  /**
   * @var int
   */
  public $workerCount;

  /**
   * @param GoogleCloudAiplatformV1AvroSource
   */
  public function setAvroSource(GoogleCloudAiplatformV1AvroSource $avroSource)
  {
    $this->avroSource = $avroSource;
  }
  /**
   * @return GoogleCloudAiplatformV1AvroSource
   */
  public function getAvroSource()
  {
    return $this->avroSource;
  }
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
   * @param GoogleCloudAiplatformV1CsvSource
   */
  public function setCsvSource(GoogleCloudAiplatformV1CsvSource $csvSource)
  {
    $this->csvSource = $csvSource;
  }
  /**
   * @return GoogleCloudAiplatformV1CsvSource
   */
  public function getCsvSource()
  {
    return $this->csvSource;
  }
  /**
   * @param bool
   */
  public function setDisableIngestionAnalysis($disableIngestionAnalysis)
  {
    $this->disableIngestionAnalysis = $disableIngestionAnalysis;
  }
  /**
   * @return bool
   */
  public function getDisableIngestionAnalysis()
  {
    return $this->disableIngestionAnalysis;
  }
  /**
   * @param bool
   */
  public function setDisableOnlineServing($disableOnlineServing)
  {
    $this->disableOnlineServing = $disableOnlineServing;
  }
  /**
   * @return bool
   */
  public function getDisableOnlineServing()
  {
    return $this->disableOnlineServing;
  }
  /**
   * @param string
   */
  public function setEntityIdField($entityIdField)
  {
    $this->entityIdField = $entityIdField;
  }
  /**
   * @return string
   */
  public function getEntityIdField()
  {
    return $this->entityIdField;
  }
  /**
   * @param GoogleCloudAiplatformV1ImportFeatureValuesRequestFeatureSpec[]
   */
  public function setFeatureSpecs($featureSpecs)
  {
    $this->featureSpecs = $featureSpecs;
  }
  /**
   * @return GoogleCloudAiplatformV1ImportFeatureValuesRequestFeatureSpec[]
   */
  public function getFeatureSpecs()
  {
    return $this->featureSpecs;
  }
  /**
   * @param string
   */
  public function setFeatureTime($featureTime)
  {
    $this->featureTime = $featureTime;
  }
  /**
   * @return string
   */
  public function getFeatureTime()
  {
    return $this->featureTime;
  }
  /**
   * @param string
   */
  public function setFeatureTimeField($featureTimeField)
  {
    $this->featureTimeField = $featureTimeField;
  }
  /**
   * @return string
   */
  public function getFeatureTimeField()
  {
    return $this->featureTimeField;
  }
  /**
   * @param int
   */
  public function setWorkerCount($workerCount)
  {
    $this->workerCount = $workerCount;
  }
  /**
   * @return int
   */
  public function getWorkerCount()
  {
    return $this->workerCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ImportFeatureValuesRequest::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ImportFeatureValuesRequest');
