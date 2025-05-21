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

class GoogleMapsAddressvalidationV1Address extends \Google\Collection
{
  protected $collection_key = 'unresolvedTokens';
  protected $addressComponentsType = GoogleMapsAddressvalidationV1AddressComponent::class;
  protected $addressComponentsDataType = 'array';
  /**
   * @var string
   */
  public $formattedAddress;
  /**
   * @var string[]
   */
  public $missingComponentTypes;
  protected $postalAddressType = GoogleTypePostalAddress::class;
  protected $postalAddressDataType = '';
  /**
   * @var string[]
   */
  public $unconfirmedComponentTypes;
  /**
   * @var string[]
   */
  public $unresolvedTokens;

  /**
   * @param GoogleMapsAddressvalidationV1AddressComponent[]
   */
  public function setAddressComponents($addressComponents)
  {
    $this->addressComponents = $addressComponents;
  }
  /**
   * @return GoogleMapsAddressvalidationV1AddressComponent[]
   */
  public function getAddressComponents()
  {
    return $this->addressComponents;
  }
  /**
   * @param string
   */
  public function setFormattedAddress($formattedAddress)
  {
    $this->formattedAddress = $formattedAddress;
  }
  /**
   * @return string
   */
  public function getFormattedAddress()
  {
    return $this->formattedAddress;
  }
  /**
   * @param string[]
   */
  public function setMissingComponentTypes($missingComponentTypes)
  {
    $this->missingComponentTypes = $missingComponentTypes;
  }
  /**
   * @return string[]
   */
  public function getMissingComponentTypes()
  {
    return $this->missingComponentTypes;
  }
  /**
   * @param GoogleTypePostalAddress
   */
  public function setPostalAddress(GoogleTypePostalAddress $postalAddress)
  {
    $this->postalAddress = $postalAddress;
  }
  /**
   * @return GoogleTypePostalAddress
   */
  public function getPostalAddress()
  {
    return $this->postalAddress;
  }
  /**
   * @param string[]
   */
  public function setUnconfirmedComponentTypes($unconfirmedComponentTypes)
  {
    $this->unconfirmedComponentTypes = $unconfirmedComponentTypes;
  }
  /**
   * @return string[]
   */
  public function getUnconfirmedComponentTypes()
  {
    return $this->unconfirmedComponentTypes;
  }
  /**
   * @param string[]
   */
  public function setUnresolvedTokens($unresolvedTokens)
  {
    $this->unresolvedTokens = $unresolvedTokens;
  }
  /**
   * @return string[]
   */
  public function getUnresolvedTokens()
  {
    return $this->unresolvedTokens;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsAddressvalidationV1Address::class, 'Google_Service_AddressValidation_GoogleMapsAddressvalidationV1Address');
