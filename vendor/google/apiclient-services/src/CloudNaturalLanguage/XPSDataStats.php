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

class XPSDataStats extends \Google\Model
{
  protected $arrayStatsType = XPSArrayStats::class;
  protected $arrayStatsDataType = '';
  protected $categoryStatsType = XPSCategoryStats::class;
  protected $categoryStatsDataType = '';
  /**
   * @var string
   */
  public $distinctValueCount;
  protected $float64StatsType = XPSFloat64Stats::class;
  protected $float64StatsDataType = '';
  /**
   * @var string
   */
  public $nullValueCount;
  protected $stringStatsType = XPSStringStats::class;
  protected $stringStatsDataType = '';
  protected $structStatsType = XPSStructStats::class;
  protected $structStatsDataType = '';
  protected $timestampStatsType = XPSTimestampStats::class;
  protected $timestampStatsDataType = '';
  /**
   * @var string
   */
  public $validValueCount;

  /**
   * @param XPSArrayStats
   */
  public function setArrayStats(XPSArrayStats $arrayStats)
  {
    $this->arrayStats = $arrayStats;
  }
  /**
   * @return XPSArrayStats
   */
  public function getArrayStats()
  {
    return $this->arrayStats;
  }
  /**
   * @param XPSCategoryStats
   */
  public function setCategoryStats(XPSCategoryStats $categoryStats)
  {
    $this->categoryStats = $categoryStats;
  }
  /**
   * @return XPSCategoryStats
   */
  public function getCategoryStats()
  {
    return $this->categoryStats;
  }
  /**
   * @param string
   */
  public function setDistinctValueCount($distinctValueCount)
  {
    $this->distinctValueCount = $distinctValueCount;
  }
  /**
   * @return string
   */
  public function getDistinctValueCount()
  {
    return $this->distinctValueCount;
  }
  /**
   * @param XPSFloat64Stats
   */
  public function setFloat64Stats(XPSFloat64Stats $float64Stats)
  {
    $this->float64Stats = $float64Stats;
  }
  /**
   * @return XPSFloat64Stats
   */
  public function getFloat64Stats()
  {
    return $this->float64Stats;
  }
  /**
   * @param string
   */
  public function setNullValueCount($nullValueCount)
  {
    $this->nullValueCount = $nullValueCount;
  }
  /**
   * @return string
   */
  public function getNullValueCount()
  {
    return $this->nullValueCount;
  }
  /**
   * @param XPSStringStats
   */
  public function setStringStats(XPSStringStats $stringStats)
  {
    $this->stringStats = $stringStats;
  }
  /**
   * @return XPSStringStats
   */
  public function getStringStats()
  {
    return $this->stringStats;
  }
  /**
   * @param XPSStructStats
   */
  public function setStructStats(XPSStructStats $structStats)
  {
    $this->structStats = $structStats;
  }
  /**
   * @return XPSStructStats
   */
  public function getStructStats()
  {
    return $this->structStats;
  }
  /**
   * @param XPSTimestampStats
   */
  public function setTimestampStats(XPSTimestampStats $timestampStats)
  {
    $this->timestampStats = $timestampStats;
  }
  /**
   * @return XPSTimestampStats
   */
  public function getTimestampStats()
  {
    return $this->timestampStats;
  }
  /**
   * @param string
   */
  public function setValidValueCount($validValueCount)
  {
    $this->validValueCount = $validValueCount;
  }
  /**
   * @return string
   */
  public function getValidValueCount()
  {
    return $this->validValueCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSDataStats::class, 'Google_Service_CloudNaturalLanguage_XPSDataStats');
