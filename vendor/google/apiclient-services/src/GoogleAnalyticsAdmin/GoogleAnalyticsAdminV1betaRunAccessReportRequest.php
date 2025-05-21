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

namespace Google\Service\GoogleAnalyticsAdmin;

class GoogleAnalyticsAdminV1betaRunAccessReportRequest extends \Google\Collection
{
  protected $collection_key = 'orderBys';
  protected $dateRangesType = GoogleAnalyticsAdminV1betaAccessDateRange::class;
  protected $dateRangesDataType = 'array';
  protected $dimensionFilterType = GoogleAnalyticsAdminV1betaAccessFilterExpression::class;
  protected $dimensionFilterDataType = '';
  protected $dimensionsType = GoogleAnalyticsAdminV1betaAccessDimension::class;
  protected $dimensionsDataType = 'array';
  /**
   * @var bool
   */
  public $expandGroups;
  /**
   * @var bool
   */
  public $includeAllUsers;
  /**
   * @var string
   */
  public $limit;
  protected $metricFilterType = GoogleAnalyticsAdminV1betaAccessFilterExpression::class;
  protected $metricFilterDataType = '';
  protected $metricsType = GoogleAnalyticsAdminV1betaAccessMetric::class;
  protected $metricsDataType = 'array';
  /**
   * @var string
   */
  public $offset;
  protected $orderBysType = GoogleAnalyticsAdminV1betaAccessOrderBy::class;
  protected $orderBysDataType = 'array';
  /**
   * @var bool
   */
  public $returnEntityQuota;
  /**
   * @var string
   */
  public $timeZone;

  /**
   * @param GoogleAnalyticsAdminV1betaAccessDateRange[]
   */
  public function setDateRanges($dateRanges)
  {
    $this->dateRanges = $dateRanges;
  }
  /**
   * @return GoogleAnalyticsAdminV1betaAccessDateRange[]
   */
  public function getDateRanges()
  {
    return $this->dateRanges;
  }
  /**
   * @param GoogleAnalyticsAdminV1betaAccessFilterExpression
   */
  public function setDimensionFilter(GoogleAnalyticsAdminV1betaAccessFilterExpression $dimensionFilter)
  {
    $this->dimensionFilter = $dimensionFilter;
  }
  /**
   * @return GoogleAnalyticsAdminV1betaAccessFilterExpression
   */
  public function getDimensionFilter()
  {
    return $this->dimensionFilter;
  }
  /**
   * @param GoogleAnalyticsAdminV1betaAccessDimension[]
   */
  public function setDimensions($dimensions)
  {
    $this->dimensions = $dimensions;
  }
  /**
   * @return GoogleAnalyticsAdminV1betaAccessDimension[]
   */
  public function getDimensions()
  {
    return $this->dimensions;
  }
  /**
   * @param bool
   */
  public function setExpandGroups($expandGroups)
  {
    $this->expandGroups = $expandGroups;
  }
  /**
   * @return bool
   */
  public function getExpandGroups()
  {
    return $this->expandGroups;
  }
  /**
   * @param bool
   */
  public function setIncludeAllUsers($includeAllUsers)
  {
    $this->includeAllUsers = $includeAllUsers;
  }
  /**
   * @return bool
   */
  public function getIncludeAllUsers()
  {
    return $this->includeAllUsers;
  }
  /**
   * @param string
   */
  public function setLimit($limit)
  {
    $this->limit = $limit;
  }
  /**
   * @return string
   */
  public function getLimit()
  {
    return $this->limit;
  }
  /**
   * @param GoogleAnalyticsAdminV1betaAccessFilterExpression
   */
  public function setMetricFilter(GoogleAnalyticsAdminV1betaAccessFilterExpression $metricFilter)
  {
    $this->metricFilter = $metricFilter;
  }
  /**
   * @return GoogleAnalyticsAdminV1betaAccessFilterExpression
   */
  public function getMetricFilter()
  {
    return $this->metricFilter;
  }
  /**
   * @param GoogleAnalyticsAdminV1betaAccessMetric[]
   */
  public function setMetrics($metrics)
  {
    $this->metrics = $metrics;
  }
  /**
   * @return GoogleAnalyticsAdminV1betaAccessMetric[]
   */
  public function getMetrics()
  {
    return $this->metrics;
  }
  /**
   * @param string
   */
  public function setOffset($offset)
  {
    $this->offset = $offset;
  }
  /**
   * @return string
   */
  public function getOffset()
  {
    return $this->offset;
  }
  /**
   * @param GoogleAnalyticsAdminV1betaAccessOrderBy[]
   */
  public function setOrderBys($orderBys)
  {
    $this->orderBys = $orderBys;
  }
  /**
   * @return GoogleAnalyticsAdminV1betaAccessOrderBy[]
   */
  public function getOrderBys()
  {
    return $this->orderBys;
  }
  /**
   * @param bool
   */
  public function setReturnEntityQuota($returnEntityQuota)
  {
    $this->returnEntityQuota = $returnEntityQuota;
  }
  /**
   * @return bool
   */
  public function getReturnEntityQuota()
  {
    return $this->returnEntityQuota;
  }
  /**
   * @param string
   */
  public function setTimeZone($timeZone)
  {
    $this->timeZone = $timeZone;
  }
  /**
   * @return string
   */
  public function getTimeZone()
  {
    return $this->timeZone;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAnalyticsAdminV1betaRunAccessReportRequest::class, 'Google_Service_GoogleAnalyticsAdmin_GoogleAnalyticsAdminV1betaRunAccessReportRequest');
