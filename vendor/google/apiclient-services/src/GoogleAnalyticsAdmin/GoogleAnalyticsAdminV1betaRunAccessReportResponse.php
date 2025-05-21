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

class GoogleAnalyticsAdminV1betaRunAccessReportResponse extends \Google\Collection
{
  protected $collection_key = 'rows';
  protected $dimensionHeadersType = GoogleAnalyticsAdminV1betaAccessDimensionHeader::class;
  protected $dimensionHeadersDataType = 'array';
  protected $metricHeadersType = GoogleAnalyticsAdminV1betaAccessMetricHeader::class;
  protected $metricHeadersDataType = 'array';
  protected $quotaType = GoogleAnalyticsAdminV1betaAccessQuota::class;
  protected $quotaDataType = '';
  /**
   * @var int
   */
  public $rowCount;
  protected $rowsType = GoogleAnalyticsAdminV1betaAccessRow::class;
  protected $rowsDataType = 'array';

  /**
   * @param GoogleAnalyticsAdminV1betaAccessDimensionHeader[]
   */
  public function setDimensionHeaders($dimensionHeaders)
  {
    $this->dimensionHeaders = $dimensionHeaders;
  }
  /**
   * @return GoogleAnalyticsAdminV1betaAccessDimensionHeader[]
   */
  public function getDimensionHeaders()
  {
    return $this->dimensionHeaders;
  }
  /**
   * @param GoogleAnalyticsAdminV1betaAccessMetricHeader[]
   */
  public function setMetricHeaders($metricHeaders)
  {
    $this->metricHeaders = $metricHeaders;
  }
  /**
   * @return GoogleAnalyticsAdminV1betaAccessMetricHeader[]
   */
  public function getMetricHeaders()
  {
    return $this->metricHeaders;
  }
  /**
   * @param GoogleAnalyticsAdminV1betaAccessQuota
   */
  public function setQuota(GoogleAnalyticsAdminV1betaAccessQuota $quota)
  {
    $this->quota = $quota;
  }
  /**
   * @return GoogleAnalyticsAdminV1betaAccessQuota
   */
  public function getQuota()
  {
    return $this->quota;
  }
  /**
   * @param int
   */
  public function setRowCount($rowCount)
  {
    $this->rowCount = $rowCount;
  }
  /**
   * @return int
   */
  public function getRowCount()
  {
    return $this->rowCount;
  }
  /**
   * @param GoogleAnalyticsAdminV1betaAccessRow[]
   */
  public function setRows($rows)
  {
    $this->rows = $rows;
  }
  /**
   * @return GoogleAnalyticsAdminV1betaAccessRow[]
   */
  public function getRows()
  {
    return $this->rows;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAnalyticsAdminV1betaRunAccessReportResponse::class, 'Google_Service_GoogleAnalyticsAdmin_GoogleAnalyticsAdminV1betaRunAccessReportResponse');
