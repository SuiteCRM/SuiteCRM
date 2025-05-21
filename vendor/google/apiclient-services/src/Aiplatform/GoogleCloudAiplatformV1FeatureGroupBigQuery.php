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

class GoogleCloudAiplatformV1FeatureGroupBigQuery extends \Google\Collection
{
  protected $collection_key = 'entityIdColumns';
  protected $bigQuerySourceType = GoogleCloudAiplatformV1BigQuerySource::class;
  protected $bigQuerySourceDataType = '';
  /**
   * @var bool
   */
  public $dense;
  /**
   * @var string[]
   */
  public $entityIdColumns;
  /**
   * @var bool
   */
  public $staticDataSource;
  protected $timeSeriesType = GoogleCloudAiplatformV1FeatureGroupBigQueryTimeSeries::class;
  protected $timeSeriesDataType = '';

  /**
   * @param GoogleCloudAiplatformV1BigQuerySource
   */
  public function setBigQuerySource(GoogleCloudAiplatformV1BigQuerySource $bigQuerySource)
  {
    $this->bigQuerySource = $bigQuerySource;
  }
  /**
   * @return GoogleCloudAiplatformV1BigQuerySource
   */
  public function getBigQuerySource()
  {
    return $this->bigQuerySource;
  }
  /**
   * @param bool
   */
  public function setDense($dense)
  {
    $this->dense = $dense;
  }
  /**
   * @return bool
   */
  public function getDense()
  {
    return $this->dense;
  }
  /**
   * @param string[]
   */
  public function setEntityIdColumns($entityIdColumns)
  {
    $this->entityIdColumns = $entityIdColumns;
  }
  /**
   * @return string[]
   */
  public function getEntityIdColumns()
  {
    return $this->entityIdColumns;
  }
  /**
   * @param bool
   */
  public function setStaticDataSource($staticDataSource)
  {
    $this->staticDataSource = $staticDataSource;
  }
  /**
   * @return bool
   */
  public function getStaticDataSource()
  {
    return $this->staticDataSource;
  }
  /**
   * @param GoogleCloudAiplatformV1FeatureGroupBigQueryTimeSeries
   */
  public function setTimeSeries(GoogleCloudAiplatformV1FeatureGroupBigQueryTimeSeries $timeSeries)
  {
    $this->timeSeries = $timeSeries;
  }
  /**
   * @return GoogleCloudAiplatformV1FeatureGroupBigQueryTimeSeries
   */
  public function getTimeSeries()
  {
    return $this->timeSeries;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1FeatureGroupBigQuery::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1FeatureGroupBigQuery');
