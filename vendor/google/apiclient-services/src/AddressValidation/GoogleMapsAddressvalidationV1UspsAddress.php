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

namespace Google\Service\AddressValidation;

class GoogleMapsAddressvalidationV1UspsAddress extends \Google\Model
{
  /**
   * @var string
   */
  public $city;
  /**
   * @var string
   */
  public $cityStateZipAddressLine;
  /**
   * @var string
   */
  public $firm;
  /**
   * @var string
   */
  public $firstAddressLine;
  /**
   * @var string
   */
  public $secondAddressLine;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $urbanization;
  /**
   * @var string
   */
  public $zipCode;
  /**
   * @var string
   */
  public $zipCodeExtension;

  /**
   * @param string
   */
  public function setCity($city)
  {
    $this->city = $city;
  }
  /**
   * @return string
   */
  public function getCity()
  {
    return $this->city;
  }
  /**
   * @param string
   */
  public function setCityStateZipAddressLine($cityStateZipAddressLine)
  {
    $this->cityStateZipAddressLine = $cityStateZipAddressLine;
  }
  /**
   * @return string
   */
  public function getCityStateZipAddressLine()
  {
    return $this->cityStateZipAddressLine;
  }
  /**
   * @param string
   */
  public function setFirm($firm)
  {
    $this->firm = $firm;
  }
  /**
   * @return string
   */
  public function getFirm()
  {
    return $this->firm;
  }
  /**
   * @param string
   */
  public function setFirstAddressLine($firstAddressLine)
  {
    $this->firstAddressLine = $firstAddressLine;
  }
  /**
   * @return string
   */
  public function getFirstAddressLine()
  {
    return $this->firstAddressLine;
  }
  /**
   * @param string
   */
  public function setSecondAddressLine($secondAddressLine)
  {
    $this->secondAddressLine = $secondAddressLine;
  }
  /**
   * @return string
   */
  public function getSecondAddressLine()
  {
    return $this->secondAddressLine;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param string
   */
  public function setUrbanization($urbanization)
  {
    $this->urbanization = $urbanization;
  }
  /**
   * @return string
   */
  public function getUrbanization()
  {
    return $this->urbanization;
  }
  /**
   * @param string
   */
  public function setZipCode($zipCode)
  {
    $this->zipCode = $zipCode;
  }
  /**
   * @return string
   */
  public function getZipCode()
  {
    return $this->zipCode;
  }
  /**
   * @param string
   */
  public function setZipCodeExtension($zipCodeExtension)
  {
    $this->zipCodeExtension = $zipCodeExtension;
  }
  /**
   * @return string
   */
  public function getZipCodeExtension()
  {
    return $this->zipCodeExtension;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsAddressvalidationV1UspsAddress::class, 'Google_Service_AddressValidation_GoogleMapsAddressvalidationV1UspsAddress');
