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

class RoofSegmentSummary extends \Google\Model
{
  /**
   * @var float
   */
  public $azimuthDegrees;
  /**
   * @var int
   */
  public $panelsCount;
  /**
   * @var float
   */
  public $pitchDegrees;
  /**
   * @var int
   */
  public $segmentIndex;
  /**
   * @var float
   */
  public $yearlyEnergyDcKwh;

  /**
   * @param float
   */
  public function setAzimuthDegrees($azimuthDegrees)
  {
    $this->azimuthDegrees = $azimuthDegrees;
  }
  /**
   * @return float
   */
  public function getAzimuthDegrees()
  {
    return $this->azimuthDegrees;
  }
  /**
   * @param int
   */
  public function setPanelsCount($panelsCount)
  {
    $this->panelsCount = $panelsCount;
  }
  /**
   * @return int
   */
  public function getPanelsCount()
  {
    return $this->panelsCount;
  }
  /**
   * @param float
   */
  public function setPitchDegrees($pitchDegrees)
  {
    $this->pitchDegrees = $pitchDegrees;
  }
  /**
   * @return float
   */
  public function getPitchDegrees()
  {
    return $this->pitchDegrees;
  }
  /**
   * @param int
   */
  public function setSegmentIndex($segmentIndex)
  {
    $this->segmentIndex = $segmentIndex;
  }
  /**
   * @return int
   */
  public function getSegmentIndex()
  {
    return $this->segmentIndex;
  }
  /**
   * @param float
   */
  public function setYearlyEnergyDcKwh($yearlyEnergyDcKwh)
  {
    $this->yearlyEnergyDcKwh = $yearlyEnergyDcKwh;
  }
  /**
   * @return float
   */
  public function getYearlyEnergyDcKwh()
  {
    return $this->yearlyEnergyDcKwh;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RoofSegmentSummary::class, 'Google_Service_Solar_RoofSegmentSummary');
