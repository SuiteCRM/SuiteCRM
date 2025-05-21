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

namespace Google\Service\Walletobjects;

class FlightHeader extends \Google\Model
{
  protected $carrierType = FlightCarrier::class;
  protected $carrierDataType = '';
  /**
   * @var string
   */
  public $flightNumber;
  /**
   * @var string
   */
  public $flightNumberDisplayOverride;
  /**
   * @var string
   */
  public $kind;
  protected $operatingCarrierType = FlightCarrier::class;
  protected $operatingCarrierDataType = '';
  /**
   * @var string
   */
  public $operatingFlightNumber;

  /**
   * @param FlightCarrier
   */
  public function setCarrier(FlightCarrier $carrier)
  {
    $this->carrier = $carrier;
  }
  /**
   * @return FlightCarrier
   */
  public function getCarrier()
  {
    return $this->carrier;
  }
  /**
   * @param string
   */
  public function setFlightNumber($flightNumber)
  {
    $this->flightNumber = $flightNumber;
  }
  /**
   * @return string
   */
  public function getFlightNumber()
  {
    return $this->flightNumber;
  }
  /**
   * @param string
   */
  public function setFlightNumberDisplayOverride($flightNumberDisplayOverride)
  {
    $this->flightNumberDisplayOverride = $flightNumberDisplayOverride;
  }
  /**
   * @return string
   */
  public function getFlightNumberDisplayOverride()
  {
    return $this->flightNumberDisplayOverride;
  }
  /**
   * @param string
   */
  public function setKind($kind)
  {
    $this->kind = $kind;
  }
  /**
   * @return string
   */
  public function getKind()
  {
    return $this->kind;
  }
  /**
   * @param FlightCarrier
   */
  public function setOperatingCarrier(FlightCarrier $operatingCarrier)
  {
    $this->operatingCarrier = $operatingCarrier;
  }
  /**
   * @return FlightCarrier
   */
  public function getOperatingCarrier()
  {
    return $this->operatingCarrier;
  }
  /**
   * @param string
   */
  public function setOperatingFlightNumber($operatingFlightNumber)
  {
    $this->operatingFlightNumber = $operatingFlightNumber;
  }
  /**
   * @return string
   */
  public function getOperatingFlightNumber()
  {
    return $this->operatingFlightNumber;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(FlightHeader::class, 'Google_Service_Walletobjects_FlightHeader');
