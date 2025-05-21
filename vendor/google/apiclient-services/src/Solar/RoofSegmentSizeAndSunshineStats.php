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

class RoofSegmentSizeAndSunshineStats extends \Google\Model
{
  /**
   * @var float
   */
  public $azimuthDegrees;
  protected $boundingBoxType = LatLngBox::class;
  protected $boundingBoxDataType = '';
  protected $centerType = LatLng::class;
  protected $centerDataType = '';
  /**
   * @var float
   */
  public $pitchDegrees;
  /**
   * @var float
   */
  public $planeHeightAtCenterMeters;
  protected $statsType = SizeAndSunshineStats::class;
  protected $statsDataType = '';

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
   * @param LatLngBox
   */
  public function setBoundingBox(LatLngBox $boundingBox)
  {
    $this->boundingBox = $boundingBox;
  }
  /**
   * @return LatLngBox
   */
  public function getBoundingBox()
  {
    return $this->boundingBox;
  }
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
   * @param float
   */
  public function setPlaneHeightAtCenterMeters($planeHeightAtCenterMeters)
  {
    $this->planeHeightAtCenterMeters = $planeHeightAtCenterMeters;
  }
  /**
   * @return float
   */
  public function getPlaneHeightAtCenterMeters()
  {
    return $this->planeHeightAtCenterMeters;
  }
  /**
   * @param SizeAndSunshineStats
   */
  public function setStats(SizeAndSunshineStats $stats)
  {
    $this->stats = $stats;
  }
  /**
   * @return SizeAndSunshineStats
   */
  public function getStats()
  {
    return $this->stats;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RoofSegmentSizeAndSunshineStats::class, 'Google_Service_Solar_RoofSegmentSizeAndSunshineStats');
