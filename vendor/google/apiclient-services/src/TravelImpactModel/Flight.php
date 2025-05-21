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

namespace Google\Service\TravelImpactModel;

class Flight extends \Google\Model
{
  protected $departureDateType = Date::class;
  protected $departureDateDataType = '';
  /**
   * @var string
   */
  public $destination;
  /**
   * @var int
   */
  public $flightNumber;
  /**
   * @var string
   */
  public $operatingCarrierCode;
  /**
   * @var string
   */
  public $origin;

  /**
   * @param Date
   */
  public function setDepartureDate(Date $departureDate)
  {
    $this->departureDate = $departureDate;
  }
  /**
   * @return Date
   */
  public function getDepartureDate()
  {
    return $this->departureDate;
  }
  /**
   * @param string
   */
  public function setDestination($destination)
  {
    $this->destination = $destination;
  }
  /**
   * @return string
   */
  public function getDestination()
  {
    return $this->destination;
  }
  /**
   * @param int
   */
  public function setFlightNumber($flightNumber)
  {
    $this->flightNumber = $flightNumber;
  }
  /**
   * @return int
   */
  public function getFlightNumber()
  {
    return $this->flightNumber;
  }
  /**
   * @param string
   */
  public function setOperatingCarrierCode($operatingCarrierCode)
  {
    $this->operatingCarrierCode = $operatingCarrierCode;
  }
  /**
   * @return string
   */
  public function getOperatingCarrierCode()
  {
    return $this->operatingCarrierCode;
  }
  /**
   * @param string
   */
  public function setOrigin($origin)
  {
    $this->origin = $origin;
  }
  /**
   * @return string
   */
  public function getOrigin()
  {
    return $this->origin;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Flight::class, 'Google_Service_TravelImpactModel_Flight');
