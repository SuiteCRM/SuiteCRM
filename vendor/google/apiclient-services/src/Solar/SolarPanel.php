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

class SolarPanel extends \Google\Model
{
  protected $centerType = LatLng::class;
  protected $centerDataType = '';
  /**
   * @var string
   */
  public $orientation;
  /**
   * @var int
   */
  public $segmentIndex;
  /**
   * @var float
   */
  public $yearlyEnergyDcKwh;

  /**
   * @param LatLng
   */
  public function setCenter(LatLng $center)
  {
    $this->center = $center;
  }
  /**
   * @return LatLng
   */
  public function getCenter()
  {
    return $this->center;
  }
  /**
   * @param string
   */
  public function setOrientation($orientation)
  {
    $this->orientation = $orientation;
  }
  /**
   * @return string
   */
  public function getOrientation()
  {
    return $this->orientation;
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
class_alias(SolarPanel::class, 'Google_Service_Solar_SolarPanel');
