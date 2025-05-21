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

class FlightCarrier extends \Google\Model
{
  protected $airlineAllianceLogoType = Image::class;
  protected $airlineAllianceLogoDataType = '';
  protected $airlineLogoType = Image::class;
  protected $airlineLogoDataType = '';
  protected $airlineNameType = LocalizedString::class;
  protected $airlineNameDataType = '';
  /**
   * @var string
   */
  public $carrierIataCode;
  /**
   * @var string
   */
  public $carrierIcaoCode;
  /**
   * @var string
   */
  public $kind;
  protected $wideAirlineLogoType = Image::class;
  protected $wideAirlineLogoDataType = '';

  /**
   * @param Image
   */
  public function setAirlineAllianceLogo(Image $airlineAllianceLogo)
  {
    $this->airlineAllianceLogo = $airlineAllianceLogo;
  }
  /**
   * @return Image
   */
  public function getAirlineAllianceLogo()
  {
    return $this->airlineAllianceLogo;
  }
  /**
   * @param Image
   */
  public function setAirlineLogo(Image $airlineLogo)
  {
    $this->airlineLogo = $airlineLogo;
  }
  /**
   * @return Image
   */
  public function getAirlineLogo()
  {
    return $this->airlineLogo;
  }
  /**
   * @param LocalizedString
   */
  public function setAirlineName(LocalizedString $airlineName)
  {
    $this->airlineName = $airlineName;
  }
  /**
   * @return LocalizedString
   */
  public function getAirlineName()
  {
    return $this->airlineName;
  }
  /**
   * @param string
   */
  public function setCarrierIataCode($carrierIataCode)
  {
    $this->carrierIataCode = $carrierIataCode;
  }
  /**
   * @return string
   */
  public function getCarrierIataCode()
  {
    return $this->carrierIataCode;
  }
  /**
   * @param string
   */
  public function setCarrierIcaoCode($carrierIcaoCode)
  {
    $this->carrierIcaoCode = $carrierIcaoCode;
  }
  /**
   * @return string
   */
  public function getCarrierIcaoCode()
  {
    return $this->carrierIcaoCode;
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
   * @param Image
   */
  public function setWideAirlineLogo(Image $wideAirlineLogo)
  {
    $this->wideAirlineLogo = $wideAirlineLogo;
  }
  /**
   * @return Image
   */
  public function getWideAirlineLogo()
  {
    return $this->wideAirlineLogo;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(FlightCarrier::class, 'Google_Service_Walletobjects_FlightCarrier');
