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

class GoogleMapsAddressvalidationV1ValidationResult extends \Google\Model
{
  protected $addressType = GoogleMapsAddressvalidationV1Address::class;
  protected $addressDataType = '';
  protected $englishLatinAddressType = GoogleMapsAddressvalidationV1Address::class;
  protected $englishLatinAddressDataType = '';
  protected $geocodeType = GoogleMapsAddressvalidationV1Geocode::class;
  protected $geocodeDataType = '';
  protected $metadataType = GoogleMapsAddressvalidationV1AddressMetadata::class;
  protected $metadataDataType = '';
  protected $uspsDataType = GoogleMapsAddressvalidationV1UspsData::class;
  protected $uspsDataDataType = '';
  protected $verdictType = GoogleMapsAddressvalidationV1Verdict::class;
  protected $verdictDataType = '';

  /**
   * @param GoogleMapsAddressvalidationV1Address
   */
  public function setAddress(GoogleMapsAddressvalidationV1Address $address)
  {
    $this->address = $address;
  }
  /**
   * @return GoogleMapsAddressvalidationV1Address
   */
  public function getAddress()
  {
    return $this->address;
  }
  /**
   * @param GoogleMapsAddressvalidationV1Address
   */
  public function setEnglishLatinAddress(GoogleMapsAddressvalidationV1Address $englishLatinAddress)
  {
    $this->englishLatinAddress = $englishLatinAddress;
  }
  /**
   * @return GoogleMapsAddressvalidationV1Address
   */
  public function getEnglishLatinAddress()
  {
    return $this->englishLatinAddress;
  }
  /**
   * @param GoogleMapsAddressvalidationV1Geocode
   */
  public function setGeocode(GoogleMapsAddressvalidationV1Geocode $geocode)
  {
    $this->geocode = $geocode;
  }
  /**
   * @return GoogleMapsAddressvalidationV1Geocode
   */
  public function getGeocode()
  {
    return $this->geocode;
  }
  /**
   * @param GoogleMapsAddressvalidationV1AddressMetadata
   */
  public function setMetadata(GoogleMapsAddressvalidationV1AddressMetadata $metadata)
  {
    $this->metadata = $metadata;
  }
  /**
   * @return GoogleMapsAddressvalidationV1AddressMetadata
   */
  public function getMetadata()
  {
    return $this->metadata;
  }
  /**
   * @param GoogleMapsAddressvalidationV1UspsData
   */
  public function setUspsData(GoogleMapsAddressvalidationV1UspsData $uspsData)
  {
    $this->uspsData = $uspsData;
  }
  /**
   * @return GoogleMapsAddressvalidationV1UspsData
   */
  public function getUspsData()
  {
    return $this->uspsData;
  }
  /**
   * @param GoogleMapsAddressvalidationV1Verdict
   */
  public function setVerdict(GoogleMapsAddressvalidationV1Verdict $verdict)
  {
    $this->verdict = $verdict;
  }
  /**
   * @return GoogleMapsAddressvalidationV1Verdict
   */
  public function getVerdict()
  {
    return $this->verdict;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsAddressvalidationV1ValidationResult::class, 'Google_Service_AddressValidation_GoogleMapsAddressvalidationV1ValidationResult');
