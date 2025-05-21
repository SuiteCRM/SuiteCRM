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

namespace Google\Service\DiscoveryEngine;

class GoogleApiDistribution extends \Google\Collection
{
  protected $collection_key = 'exemplars';
  /**
   * @var string[]
   */
  public $bucketCounts;
  protected $bucketOptionsType = GoogleApiDistributionBucketOptions::class;
  protected $bucketOptionsDataType = '';
  /**
   * @var string
   */
  public $count;
  protected $exemplarsType = GoogleApiDistributionExemplar::class;
  protected $exemplarsDataType = 'array';
  public $mean;
  protected $rangeType = GoogleApiDistributionRange::class;
  protected $rangeDataType = '';
  public $sumOfSquaredDeviation;

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
   * @param GoogleApiDistributionBucketOptions
   */
  public function setBucketOptions(GoogleApiDistributionBucketOptions $bucketOptions)
  {
    $this->bucketOptions = $bucketOptions;
  }
  /**
   * @return GoogleApiDistributionBucketOptions
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
   * @param GoogleApiDistributionExemplar[]
   */
  public function setExemplars($exemplars)
  {
    $this->exemplars = $exemplars;
  }
  /**
   * @return GoogleApiDistributionExemplar[]
   */
  public function getExemplars()
  {
    return $this->exemplars;
  }
  public function setMean($mean)
  {
    $this->mean = $mean;
  }
  public function getMean()
  {
    return $this->mean;
  }
  /**
   * @param GoogleApiDistributionRange
   */
  public function setRange(GoogleApiDistributionRange $range)
  {
    $this->range = $range;
  }
  /**
   * @return GoogleApiDistributionRange
   */
  public function getRange()
  {
    return $this->range;
  }
  public function setSumOfSquaredDeviation($sumOfSquaredDeviation)
  {
    $this->sumOfSquaredDeviation = $sumOfSquaredDeviation;
  }
  public function getSumOfSquaredDeviation()
  {
    return $this->sumOfSquaredDeviation;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleApiDistribution::class, 'Google_Service_DiscoveryEngine_GoogleApiDistribution');
