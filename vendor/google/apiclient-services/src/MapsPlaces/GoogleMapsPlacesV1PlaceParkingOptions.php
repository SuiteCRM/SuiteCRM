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

namespace Google\Service\MapsPlaces;

class GoogleMapsPlacesV1PlaceParkingOptions extends \Google\Model
{
  /**
   * @var bool
   */
  public $freeGarageParking;
  /**
   * @var bool
   */
  public $freeParkingLot;
  /**
   * @var bool
   */
  public $freeStreetParking;
  /**
   * @var bool
   */
  public $paidGarageParking;
  /**
   * @var bool
   */
  public $paidParkingLot;
  /**
   * @var bool
   */
  public $paidStreetParking;
  /**
   * @var bool
   */
  public $valetParking;

  /**
   * @param bool
   */
  public function setFreeGarageParking($freeGarageParking)
  {
    $this->freeGarageParking = $freeGarageParking;
  }
  /**
   * @return bool
   */
  public function getFreeGarageParking()
  {
    return $this->freeGarageParking;
  }
  /**
   * @param bool
   */
  public function setFreeParkingLot($freeParkingLot)
  {
    $this->freeParkingLot = $freeParkingLot;
  }
  /**
   * @return bool
   */
  public function getFreeParkingLot()
  {
    return $this->freeParkingLot;
  }
  /**
   * @param bool
   */
  public function setFreeStreetParking($freeStreetParking)
  {
    $this->freeStreetParking = $freeStreetParking;
  }
  /**
   * @return bool
   */
  public function getFreeStreetParking()
  {
    return $this->freeStreetParking;
  }
  /**
   * @param bool
   */
  public function setPaidGarageParking($paidGarageParking)
  {
    $this->paidGarageParking = $paidGarageParking;
  }
  /**
   * @return bool
   */
  public function getPaidGarageParking()
  {
    return $this->paidGarageParking;
  }
  /**
   * @param bool
   */
  public function setPaidParkingLot($paidParkingLot)
  {
    $this->paidParkingLot = $paidParkingLot;
  }
  /**
   * @return bool
   */
  public function getPaidParkingLot()
  {
    return $this->paidParkingLot;
  }
  /**
   * @param bool
   */
  public function setPaidStreetParking($paidStreetParking)
  {
    $this->paidStreetParking = $paidStreetParking;
  }
  /**
   * @return bool
   */
  public function getPaidStreetParking()
  {
    return $this->paidStreetParking;
  }
  /**
   * @param bool
   */
  public function setValetParking($valetParking)
  {
    $this->valetParking = $valetParking;
  }
  /**
   * @return bool
   */
  public function getValetParking()
  {
    return $this->valetParking;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsPlacesV1PlaceParkingOptions::class, 'Google_Service_MapsPlaces_GoogleMapsPlacesV1PlaceParkingOptions');
