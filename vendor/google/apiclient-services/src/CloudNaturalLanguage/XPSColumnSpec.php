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

namespace Google\Service\CloudNaturalLanguage;

class XPSColumnSpec extends \Google\Collection
{
  protected $collection_key = 'topCorrelatedColumns';
  /**
   * @var int
   */
  public $columnId;
  protected $dataStatsType = XPSDataStats::class;
  protected $dataStatsDataType = '';
  protected $dataTypeType = XPSDataType::class;
  protected $dataTypeDataType = '';
  /**
   * @var string
   */
  public $displayName;
  protected $forecastingMetadataType = XPSColumnSpecForecastingMetadata::class;
  protected $forecastingMetadataDataType = '';
  protected $topCorrelatedColumnsType = XPSColumnSpecCorrelatedColumn::class;
  protected $topCorrelatedColumnsDataType = 'array';

  /**
   * @param int
   */
  public function setColumnId($columnId)
  {
    $this->columnId = $columnId;
  }
  /**
   * @return int
   */
  public function getColumnId()
  {
    return $this->columnId;
  }
  /**
   * @param XPSDataStats
   */
  public function setDataStats(XPSDataStats $dataStats)
  {
    $this->dataStats = $dataStats;
  }
  /**
   * @return XPSDataStats
   */
  public function getDataStats()
  {
    return $this->dataStats;
  }
  /**
   * @param XPSDataType
   */
  public function setDataType(XPSDataType $dataType)
  {
    $this->dataType = $dataType;
  }
  /**
   * @return XPSDataType
   */
  public function getDataType()
  {
    return $this->dataType;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param XPSColumnSpecForecastingMetadata
   */
  public function setForecastingMetadata(XPSColumnSpecForecastingMetadata $forecastingMetadata)
  {
    $this->forecastingMetadata = $forecastingMetadata;
  }
  /**
   * @return XPSColumnSpecForecastingMetadata
   */
  public function getForecastingMetadata()
  {
    return $this->forecastingMetadata;
  }
  /**
   * @param XPSColumnSpecCorrelatedColumn[]
   */
  public function setTopCorrelatedColumns($topCorrelatedColumns)
  {
    $this->topCorrelatedColumns = $topCorrelatedColumns;
  }
  /**
   * @return XPSColumnSpecCorrelatedColumn[]
   */
  public function getTopCorrelatedColumns()
  {
    return $this->topCorrelatedColumns;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSColumnSpec::class, 'Google_Service_CloudNaturalLanguage_XPSColumnSpec');
