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

class BuildingInsights extends \Google\Model
{
  /**
   * @var string
   */
  public $administrativeArea;
  protected $boundingBoxType = LatLngBox::class;
  protected $boundingBoxDataType = '';
  protected $centerType = LatLng::class;
  protected $centerDataType = '';
  protected $imageryDateType = Date::class;
  protected $imageryDateDataType = '';
  protected $imageryProcessedDateType = Date::class;
  protected $imageryProcessedDateDataType = '';
  /**
   * @var string
   */
  public $imageryQuality;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $postalCode;
  /**
   * @var string
   */
  public $regionCode;
  protected $solarPotentialType = SolarPotential::class;
  protected $solarPotentialDataType = '';
  /**
   * @var string
   */
  public $statisticalArea;

  /**
   * @param string
   */
  public function setAdministrativeArea($administrativeArea)
  {
    $this->administrativeArea = $administrativeArea;
  }
  /**
   * @return string
   */
  public function getAdministrativeArea()
  {
    return $this->administrativeArea;
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
   * @param Date
   */
  public function setImageryDate(Date $imageryDate)
  {
    $this->imageryDate = $imageryDate;
  }
  /**
   * @return Date
   */
  public function getImageryDate()
  {
    return $this->imageryDate;
  }
  /**
   * @param Date
   */
  public function setImageryProcessedDate(Date $imageryProcessedDate)
  {
    $this->imageryProcessedDate = $imageryProcessedDate;
  }
  /**
   * @return Date
   */
  public function getImageryProcessedDate()
  {
    return $this->imageryProcessedDate;
  }
  /**
   * @param string
   */
  public function setImageryQuality($imageryQuality)
  {
    $this->imageryQuality = $imageryQuality;
  }
  /**
   * @return string
   */
  public function getImageryQuality()
  {
    return $this->imageryQuality;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setPostalCode($postalCode)
  {
    $this->postalCode = $postalCode;
  }
  /**
   * @return string
   */
  public function getPostalCode()
  {
    return $this->postalCode;
  }
  /**
   * @param string
   */
  public function setRegionCode($regionCode)
  {
    $this->regionCode = $regionCode;
  }
  /**
   * @return string
   */
  public function getRegionCode()
  {
    return $this->regionCode;
  }
  /**
   * @param SolarPotential
   */
  public function setSolarPotential(SolarPotential $solarPotential)
  {
    $this->solarPotential = $solarPotential;
  }
  /**
   * @return SolarPotential
   */
  public function getSolarPotential()
  {
    return $this->solarPotential;
  }
  /**
   * @param string
   */
  public function setStatisticalArea($statisticalArea)
  {
    $this->statisticalArea = $statisticalArea;
  }
  /**
   * @return string
   */
  public function getStatisticalArea()
  {
    return $this->statisticalArea;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BuildingInsights::class, 'Google_Service_Solar_BuildingInsights');
