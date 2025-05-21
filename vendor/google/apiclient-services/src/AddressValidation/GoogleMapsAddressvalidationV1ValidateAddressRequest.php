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

class GoogleMapsAddressvalidationV1ValidateAddressRequest extends \Google\Model
{
  protected $addressType = GoogleTypePostalAddress::class;
  protected $addressDataType = '';
  /**
   * @var bool
   */
  public $enableUspsCass;
  protected $languageOptionsType = GoogleMapsAddressvalidationV1LanguageOptions::class;
  protected $languageOptionsDataType = '';
  /**
   * @var string
   */
  public $previousResponseId;
  /**
   * @var string
   */
  public $sessionToken;

  /**
   * @param GoogleTypePostalAddress
   */
  public function setAddress(GoogleTypePostalAddress $address)
  {
    $this->address = $address;
  }
  /**
   * @return GoogleTypePostalAddress
   */
  public function getAddress()
  {
    return $this->address;
  }
  /**
   * @param bool
   */
  public function setEnableUspsCass($enableUspsCass)
  {
    $this->enableUspsCass = $enableUspsCass;
  }
  /**
   * @return bool
   */
  public function getEnableUspsCass()
  {
    return $this->enableUspsCass;
  }
  /**
   * @param GoogleMapsAddressvalidationV1LanguageOptions
   */
  public function setLanguageOptions(GoogleMapsAddressvalidationV1LanguageOptions $languageOptions)
  {
    $this->languageOptions = $languageOptions;
  }
  /**
   * @return GoogleMapsAddressvalidationV1LanguageOptions
   */
  public function getLanguageOptions()
  {
    return $this->languageOptions;
  }
  /**
   * @param string
   */
  public function setPreviousResponseId($previousResponseId)
  {
    $this->previousResponseId = $previousResponseId;
  }
  /**
   * @return string
   */
  public function getPreviousResponseId()
  {
    return $this->previousResponseId;
  }
  /**
   * @param string
   */
  public function setSessionToken($sessionToken)
  {
    $this->sessionToken = $sessionToken;
  }
  /**
   * @return string
   */
  public function getSessionToken()
  {
    return $this->sessionToken;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsAddressvalidationV1ValidateAddressRequest::class, 'Google_Service_AddressValidation_GoogleMapsAddressvalidationV1ValidateAddressRequest');
