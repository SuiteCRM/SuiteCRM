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

namespace Google\Service\Solar;

class SolarPotential extends \Google\Collection
{
  protected $collection_key = 'solarPanels';
  protected $buildingStatsType = SizeAndSunshineStats::class;
  protected $buildingStatsDataType = '';
  /**
   * @var float
   */
  public $carbonOffsetFactorKgPerMwh;
  protected $financialAnalysesType = FinancialAnalysis::class;
  protected $financialAnalysesDataType = 'array';
  /**
   * @var float
   */
  public $maxArrayAreaMeters2;
  /**
   * @var int
   */
  public $maxArrayPanelsCount;
  /**
   * @var float
   */
  public $maxSunshineHoursPerYear;
  /**
   * @var float
   */
  public $panelCapacityWatts;
  /**
   * @var float
   */
  public $panelHeightMeters;
  /**
   * @var int
   */
  public $panelLifetimeYears;
  /**
   * @var float
   */
  public $panelWidthMeters;
  protected $roofSegmentStatsType = RoofSegmentSizeAndSunshineStats::class;
  protected $roofSegmentStatsDataType = 'array';
  protected $solarPanelConfigsType = SolarPanelConfig::class;
  protected $solarPanelConfigsDataType = 'array';
  protected $solarPanelsType = SolarPanel::class;
  protected $solarPanelsDataType = 'array';
  protected $wholeRoofStatsType = SizeAndSunshineStats::class;
  protected $wholeRoofStatsDataType = '';

  /**
   * @param SizeAndSunshineStats
   */
  public function setBuildingStats(SizeAndSunshineStats $buildingStats)
  {
    $this->buildingStats = $buildingStats;
  }
  /**
   * @return SizeAndSunshineStats
   */
  public function getBuildingStats()
  {
    return $this->buildingStats;
  }
  /**
   * @param float
   */
  public function setCarbonOffsetFactorKgPerMwh($carbonOffsetFactorKgPerMwh)
  {
    $this->carbonOffsetFactorKgPerMwh = $carbonOffsetFactorKgPerMwh;
  }
  /**
   * @return float
   */
  public function getCarbonOffsetFactorKgPerMwh()
  {
    return $this->carbonOffsetFactorKgPerMwh;
  }
  /**
   * @param FinancialAnalysis[]
   */
  public function setFinancialAnalyses($financialAnalyses)
  {
    $this->financialAnalyses = $financialAnalyses;
  }
  /**
   * @return FinancialAnalysis[]
   */
  public function getFinancialAnalyses()
  {
    return $this->financialAnalyses;
  }
  /**
   * @param float
   */
  public function setMaxArrayAreaMeters2($maxArrayAreaMeters2)
  {
    $this->maxArrayAreaMeters2 = $maxArrayAreaMeters2;
  }
  /**
   * @return float
   */
  public function getMaxArrayAreaMeters2()
  {
    return $this->maxArrayAreaMeters2;
  }
  /**
   * @param int
   */
  public function setMaxArrayPanelsCount($maxArrayPanelsCount)
  {
    $this->maxArrayPanelsCount = $maxArrayPanelsCount;
  }
  /**
   * @return int
   */
  public function getMaxArrayPanelsCount()
  {
    return $this->maxArrayPanelsCount;
  }
  /**
   * @param float
   */
  public function setMaxSunshineHoursPerYear($maxSunshineHoursPerYear)
  {
    $this->maxSunshineHoursPerYear = $maxSunshineHoursPerYear;
  }
  /**
   * @return float
   */
  public function getMaxSunshineHoursPerYear()
  {
    return $this->maxSunshineHoursPerYear;
  }
  /**
   * @param float
   */
  public function setPanelCapacityWatts($panelCapacityWatts)
  {
    $this->panelCapacityWatts = $panelCapacityWatts;
  }
  /**
   * @return float
   */
  public function getPanelCapacityWatts()
  {
    return $this->panelCapacityWatts;
  }
  /**
   * @param float
   */
  public function setPanelHeightMeters($panelHeightMeters)
  {
    $this->panelHeightMeters = $panelHeightMeters;
  }
  /**
   * @return float
   */
  public function getPanelHeightMeters()
  {
    return $this->panelHeightMeters;
  }
  /**
   * @param int
   */
  public function setPanelLifetimeYears($panelLifetimeYears)
  {
    $this->panelLifetimeYears = $panelLifetimeYears;
  }
  /**
   * @return int
   */
  public function getPanelLifetimeYears()
  {
    return $this->panelLifetimeYears;
  }
  /**
   * @param float
   */
  public function setPanelWidthMeters($panelWidthMeters)
  {
    $this->panelWidthMeters = $panelWidthMeters;
  }
  /**
   * @return float
   */
  public function getPanelWidthMeters()
  {
    return $this->panelWidthMeters;
  }
  /**
   * @param RoofSegmentSizeAndSunshineStats[]
   */
  public function setRoofSegmentStats($roofSegmentStats)
  {
    $this->roofSegmentStats = $roofSegmentStats;
  }
  /**
   * @return RoofSegmentSizeAndSunshineStats[]
   */
  public function getRoofSegmentStats()
  {
    return $this->roofSegmentStats;
  }
  /**
   * @param SolarPanelConfig[]
   */
  public function setSolarPanelConfigs($solarPanelConfigs)
  {
    $this->solarPanelConfigs = $solarPanelConfigs;
  }
  /**
   * @return SolarPanelConfig[]
   */
  public function getSolarPanelConfigs()
  {
    return $this->solarPanelConfigs;
  }
  /**
   * @param SolarPanel[]
   */
  public function setSolarPanels($solarPanels)
  {
    $this->solarPanels = $solarPanels;
  }
  /**
   * @return SolarPanel[]
   */
  public function getSolarPanels()
  {
    return $this->solarPanels;
  }
  /**
   * @param SizeAndSunshineStats
   */
  public function setWholeRoofStats(SizeAndSunshineStats $wholeRoofStats)
  {
    $this->wholeRoofStats = $wholeRoofStats;
  }
  /**
   * @return SizeAndSunshineStats
   */
  public function getWholeRoofStats()
  {
    return $this->wholeRoofStats;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SolarPotential::class, 'Google_Service_Solar_SolarPotential');
