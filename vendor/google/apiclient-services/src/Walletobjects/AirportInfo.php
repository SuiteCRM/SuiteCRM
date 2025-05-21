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

class AirportInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $airportIataCode;
  protected $airportNameOverrideType = LocalizedString::class;
  protected $airportNameOverrideDataType = '';
  /**
   * @var string
   */
  public $gate;
  /**
   * @var string
   */
  public $kind;
  /**
   * @var string
   */
  public $terminal;

  /**
   * @param string
   */
  public function setAirportIataCode($airportIataCode)
  {
    $this->airportIataCode = $airportIataCode;
  }
  /**
   * @return string
   */
  public function getAirportIataCode()
  {
    return $this->airportIataCode;
  }
  /**
   * @param LocalizedString
   */
  public function setAirportNameOverride(LocalizedString $airportNameOverride)
  {
    $this->airportNameOverride = $airportNameOverride;
  }
  /**
   * @return LocalizedString
   */
  public function getAirportNameOverride()
  {
    return $this->airportNameOverride;
  }
  /**
   * @param string
   */
  public function setGate($gate)
  {
    $this->gate = $gate;
  }
  /**
   * @return string
   */
  public function getGate()
  {
    return $this->gate;
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
   * @param string
   */
  public function setTerminal($terminal)
  {
    $this->terminal = $terminal;
  }
  /**
   * @return string
   */
  public function getTerminal()
  {
    return $this->terminal;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AirportInfo::class, 'Google_Service_Walletobjects_AirportInfo');
