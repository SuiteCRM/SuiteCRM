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

namespace Google\Service\MigrationCenterAPI;

class ReportSummaryAssetAggregateStats extends \Google\Model
{
  protected $coreCountHistogramType = ReportSummaryHistogramChartData::class;
  protected $coreCountHistogramDataType = '';
  protected $memoryBytesHistogramType = ReportSummaryHistogramChartData::class;
  protected $memoryBytesHistogramDataType = '';
  protected $memoryUtilizationChartType = ReportSummaryUtilizationChartData::class;
  protected $memoryUtilizationChartDataType = '';
  protected $operatingSystemType = ReportSummaryChartData::class;
  protected $operatingSystemDataType = '';
  protected $storageBytesHistogramType = ReportSummaryHistogramChartData::class;
  protected $storageBytesHistogramDataType = '';
  protected $storageUtilizationChartType = ReportSummaryUtilizationChartData::class;
  protected $storageUtilizationChartDataType = '';
  /**
   * @var string
   */
  public $totalAssets;
  /**
   * @var string
   */
  public $totalCores;
  /**
   * @var string
   */
  public $totalMemoryBytes;
  /**
   * @var string
   */
  public $totalStorageBytes;

  /**
   * @param ReportSummaryHistogramChartData
   */
  public function setCoreCountHistogram(ReportSummaryHistogramChartData $coreCountHistogram)
  {
    $this->coreCountHistogram = $coreCountHistogram;
  }
  /**
   * @return ReportSummaryHistogramChartData
   */
  public function getCoreCountHistogram()
  {
    return $this->coreCountHistogram;
  }
  /**
   * @param ReportSummaryHistogramChartData
   */
  public function setMemoryBytesHistogram(ReportSummaryHistogramChartData $memoryBytesHistogram)
  {
    $this->memoryBytesHistogram = $memoryBytesHistogram;
  }
  /**
   * @return ReportSummaryHistogramChartData
   */
  public function getMemoryBytesHistogram()
  {
    return $this->memoryBytesHistogram;
  }
  /**
   * @param ReportSummaryUtilizationChartData
   */
  public function setMemoryUtilizationChart(ReportSummaryUtilizationChartData $memoryUtilizationChart)
  {
    $this->memoryUtilizationChart = $memoryUtilizationChart;
  }
  /**
   * @return ReportSummaryUtilizationChartData
   */
  public function getMemoryUtilizationChart()
  {
    return $this->memoryUtilizationChart;
  }
  /**
   * @param ReportSummaryChartData
   */
  public function setOperatingSystem(ReportSummaryChartData $operatingSystem)
  {
    $this->operatingSystem = $operatingSystem;
  }
  /**
   * @return ReportSummaryChartData
   */
  public function getOperatingSystem()
  {
    return $this->operatingSystem;
  }
  /**
   * @param ReportSummaryHistogramChartData
   */
  public function setStorageBytesHistogram(ReportSummaryHistogramChartData $storageBytesHistogram)
  {
    $this->storageBytesHistogram = $storageBytesHistogram;
  }
  /**
   * @return ReportSummaryHistogramChartData
   */
  public function getStorageBytesHistogram()
  {
    return $this->storageBytesHistogram;
  }
  /**
   * @param ReportSummaryUtilizationChartData
   */
  public function setStorageUtilizationChart(ReportSummaryUtilizationChartData $storageUtilizationChart)
  {
    $this->storageUtilizationChart = $storageUtilizationChart;
  }
  /**
   * @return ReportSummaryUtilizationChartData
   */
  public function getStorageUtilizationChart()
  {
    return $this->storageUtilizationChart;
  }
  /**
   * @param string
   */
  public function setTotalAssets($totalAssets)
  {
    $this->totalAssets = $totalAssets;
  }
  /**
   * @return string
   */
  public function getTotalAssets()
  {
    return $this->totalAssets;
  }
  /**
   * @param string
   */
  public function setTotalCores($totalCores)
  {
    $this->totalCores = $totalCores;
  }
  /**
   * @return string
   */
  public function getTotalCores()
  {
    return $this->totalCores;
  }
  /**
   * @param string
   */
  public function setTotalMemoryBytes($totalMemoryBytes)
  {
    $this->totalMemoryBytes = $totalMemoryBytes;
  }
  /**
   * @return string
   */
  public function getTotalMemoryBytes()
  {
    return $this->totalMemoryBytes;
  }
  /**
   * @param string
   */
  public function setTotalStorageBytes($totalStorageBytes)
  {
    $this->totalStorageBytes = $totalStorageBytes;
  }
  /**
   * @return string
   */
  public function getTotalStorageBytes()
  {
    return $this->totalStorageBytes;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ReportSummaryAssetAggregateStats::class, 'Google_Service_MigrationCenterAPI_ReportSummaryAssetAggregateStats');
