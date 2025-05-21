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

namespace Google\Service\Compute;

class QuotaExceededInfo extends \Google\Model
{
  /**
   * @var string[]
   */
  public $dimensions;
  public $futureLimit;
  public $limit;
  /**
   * @var string
   */
  public $limitName;
  /**
   * @var string
   */
  public $metricName;
  /**
   * @var string
   */
  public $rolloutStatus;

  /**
   * @param string[]
   */
  public function setDimensions($dimensions)
  {
    $this->dimensions = $dimensions;
  }
  /**
   * @return string[]
   */
  public function getDimensions()
  {
    return $this->dimensions;
  }
  public function setFutureLimit($futureLimit)
  {
    $this->futureLimit = $futureLimit;
  }
  public function getFutureLimit()
  {
    return $this->futureLimit;
  }
  public function setLimit($limit)
  {
    $this->limit = $limit;
  }
  public function getLimit()
  {
    return $this->limit;
  }
  /**
   * @param string
   */
  public function setLimitName($limitName)
  {
    $this->limitName = $limitName;
  }
  /**
   * @return string
   */
  public function getLimitName()
  {
    return $this->limitName;
  }
  /**
   * @param string
   */
  public function setMetricName($metricName)
  {
    $this->metricName = $metricName;
  }
  /**
   * @return string
   */
  public function getMetricName()
  {
    return $this->metricName;
  }
  /**
   * @param string
   */
  public function setRolloutStatus($rolloutStatus)
  {
    $this->rolloutStatus = $rolloutStatus;
  }
  /**
   * @return string
   */
  public function getRolloutStatus()
  {
    return $this->rolloutStatus;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(QuotaExceededInfo::class, 'Google_Service_Compute_QuotaExceededInfo');
