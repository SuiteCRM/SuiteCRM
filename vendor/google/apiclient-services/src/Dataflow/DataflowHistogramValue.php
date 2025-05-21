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

namespace Google\Service\Dataflow;

class DataflowHistogramValue extends \Google\Collection
{
  protected $collection_key = 'bucketCounts';
  /**
   * @var string[]
   */
  public $bucketCounts;
  protected $bucketOptionsType = BucketOptions::class;
  protected $bucketOptionsDataType = '';
  /**
   * @var string
   */
  public $count;
  protected $outlierStatsType = OutlierStats::class;
  protected $outlierStatsDataType = '';

  /**
   * @param string[]
   */
  public function setBucketCounts($bucketCounts)
  {
    $this->bucketCounts = $bucketCounts;
  }
  /**
   * @return string[]
   */
  public function getBucketCounts()
  {
    return $this->bucketCounts;
  }
  /**
   * @param BucketOptions
   */
  public function setBucketOptions(BucketOptions $bucketOptions)
  {
    $this->bucketOptions = $bucketOptions;
  }
  /**
   * @return BucketOptions
   */
  public function getBucketOptions()
  {
    return $this->bucketOptions;
  }
  /**
   * @param string
   */
  public function setCount($count)
  {
    $this->count = $count;
  }
  /**
   * @return string
   */
  public function getCount()
  {
    return $this->count;
  }
  /**
   * @param OutlierStats
   */
  public function setOutlierStats(OutlierStats $outlierStats)
  {
    $this->outlierStats = $outlierStats;
  }
  /**
   * @return OutlierStats
   */
  public function getOutlierStats()
  {
    return $this->outlierStats;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DataflowHistogramValue::class, 'Google_Service_Dataflow_DataflowHistogramValue');
