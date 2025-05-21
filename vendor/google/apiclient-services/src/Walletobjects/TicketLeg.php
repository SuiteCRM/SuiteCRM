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

class TicketLeg extends \Google\Collection
{
  protected $collection_key = 'ticketSeats';
  /**
   * @var string
   */
  public $arrivalDateTime;
  /**
   * @var string
   */
  public $carriage;
  /**
   * @var string
   */
  public $departureDateTime;
  protected $destinationNameType = LocalizedString::class;
  protected $destinationNameDataType = '';
  /**
   * @var string
   */
  public $destinationStationCode;
  protected $fareNameType = LocalizedString::class;
  protected $fareNameDataType = '';
  protected $originNameType = LocalizedString::class;
  protected $originNameDataType = '';
  /**
   * @var string
   */
  public $originStationCode;
  /**
   * @var string
   */
  public $platform;
  protected $ticketSeatType = TicketSeat::class;
  protected $ticketSeatDataType = '';
  protected $ticketSeatsType = TicketSeat::class;
  protected $ticketSeatsDataType = 'array';
  protected $transitOperatorNameType = LocalizedString::class;
  protected $transitOperatorNameDataType = '';
  protected $transitTerminusNameType = LocalizedString::class;
  protected $transitTerminusNameDataType = '';
  /**
   * @var string
   */
  public $zone;

  /**
   * @param string
   */
  public function setArrivalDateTime($arrivalDateTime)
  {
    $this->arrivalDateTime = $arrivalDateTime;
  }
  /**
   * @return string
   */
  public function getArrivalDateTime()
  {
    return $this->arrivalDateTime;
  }
  /**
   * @param string
   */
  public function setCarriage($carriage)
  {
    $this->carriage = $carriage;
  }
  /**
   * @return string
   */
  public function getCarriage()
  {
    return $this->carriage;
  }
  /**
   * @param string
   */
  public function setDepartureDateTime($departureDateTime)
  {
    $this->departureDateTime = $departureDateTime;
  }
  /**
   * @return string
   */
  public function getDepartureDateTime()
  {
    return $this->departureDateTime;
  }
  /**
   * @param LocalizedString
   */
  public function setDestinationName(LocalizedString $destinationName)
  {
    $this->destinationName = $destinationName;
  }
  /**
   * @return LocalizedString
   */
  public function getDestinationName()
  {
    return $this->destinationName;
  }
  /**
   * @param string
   */
  public function setDestinationStationCode($destinationStationCode)
  {
    $this->destinationStationCode = $destinationStationCode;
  }
  /**
   * @return string
   */
  public function getDestinationStationCode()
  {
    return $this->destinationStationCode;
  }
  /**
   * @param LocalizedString
   */
  public function setFareName(LocalizedString $fareName)
  {
    $this->fareName = $fareName;
  }
  /**
   * @return LocalizedString
   */
  public function getFareName()
  {
    return $this->fareName;
  }
  /**
   * @param LocalizedString
   */
  public function setOriginName(LocalizedString $originName)
  {
    $this->originName = $originName;
  }
  /**
   * @return LocalizedString
   */
  public function getOriginName()
  {
    return $this->originName;
  }
  /**
   * @param string
   */
  public function setOriginStationCode($originStationCode)
  {
    $this->originStationCode = $originStationCode;
  }
  /**
   * @return string
   */
  public function getOriginStationCode()
  {
    return $this->originStationCode;
  }
  /**
   * @param string
   */
  public function setPlatform($platform)
  {
    $this->platform = $platform;
  }
  /**
   * @return string
   */
  public function getPlatform()
  {
    return $this->platform;
  }
  /**
   * @param TicketSeat
   */
  public function setTicketSeat(TicketSeat $ticketSeat)
  {
    $this->ticketSeat = $ticketSeat;
  }
  /**
   * @return TicketSeat
   */
  public function getTicketSeat()
  {
    return $this->ticketSeat;
  }
  /**
   * @param TicketSeat[]
   */
  public function setTicketSeats($ticketSeats)
  {
    $this->ticketSeats = $ticketSeats;
  }
  /**
   * @return TicketSeat[]
   */
  public function getTicketSeats()
  {
    return $this->ticketSeats;
  }
  /**
   * @param LocalizedString
   */
  public function setTransitOperatorName(LocalizedString $transitOperatorName)
  {
    $this->transitOperatorName = $transitOperatorName;
  }
  /**
   * @return LocalizedString
   */
  public function getTransitOperatorName()
  {
    return $this->transitOperatorName;
  }
  /**
   * @param LocalizedString
   */
  public function setTransitTerminusName(LocalizedString $transitTerminusName)
  {
    $this->transitTerminusName = $transitTerminusName;
  }
  /**
   * @return LocalizedString
   */
  public function getTransitTerminusName()
  {
    return $this->transitTerminusName;
  }
  /**
   * @param string
   */
  public function setZone($zone)
  {
    $this->zone = $zone;
  }
  /**
   * @return string
   */
  public function getZone()
  {
    return $this->zone;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TicketLeg::class, 'Google_Service_Walletobjects_TicketLeg');
