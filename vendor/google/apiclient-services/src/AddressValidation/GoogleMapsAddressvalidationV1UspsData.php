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

class GoogleMapsAddressvalidationV1UspsData extends \Google\Model
{
  /**
   * @var string
   */
  public $abbreviatedCity;
  /**
   * @var string
   */
  public $addressRecordType;
  /**
   * @var string
   */
  public $carrierRoute;
  /**
   * @var string
   */
  public $carrierRouteIndicator;
  /**
   * @var bool
   */
  public $cassProcessed;
  /**
   * @var string
   */
  public $county;
  /**
   * @var bool
   */
  public $defaultAddress;
  /**
   * @var string
   */
  public $deliveryPointCheckDigit;
  /**
   * @var string
   */
  public $deliveryPointCode;
  /**
   * @var string
   */
  public $dpvCmra;
  /**
   * @var string
   */
  public $dpvConfirmation;
  /**
   * @var string
   */
  public $dpvDoorNotAccessible;
  /**
   * @var string
   */
  public $dpvDrop;
  /**
   * @var string
   */
  public $dpvEnhancedDeliveryCode;
  /**
   * @var string
   */
  public $dpvFootnote;
  /**
   * @var string
   */
  public $dpvNoSecureLocation;
  /**
   * @var string
   */
  public $dpvNoStat;
  /**
   * @var int
   */
  public $dpvNoStatReasonCode;
  /**
   * @var string
   */
  public $dpvNonDeliveryDays;
  /**
   * @var int
   */
  public $dpvNonDeliveryDaysValues;
  /**
   * @var string
   */
  public $dpvPbsa;
  /**
   * @var string
   */
  public $dpvThrowback;
  /**
   * @var string
   */
  public $dpvVacant;
  /**
   * @var string
   */
  public $elotFlag;
  /**
   * @var string
   */
  public $elotNumber;
  /**
   * @var string
   */
  public $errorMessage;
  /**
   * @var bool
   */
  public $ewsNoMatch;
  /**
   * @var string
   */
  public $fipsCountyCode;
  /**
   * @var string
   */
  public $lacsLinkIndicator;
  /**
   * @var string
   */
  public $lacsLinkReturnCode;
  /**
   * @var string
   */
  public $pmbDesignator;
  /**
   * @var string
   */
  public $pmbNumber;
  /**
   * @var bool
   */
  public $poBoxOnlyPostalCode;
  /**
   * @var string
   */
  public $postOfficeCity;
  /**
   * @var string
   */
  public $postOfficeState;
  protected $standardizedAddressType = GoogleMapsAddressvalidationV1UspsAddress::class;
  protected $standardizedAddressDataType = '';
  /**
   * @var string
   */
  public $suitelinkFootnote;

  /**
   * @param string
   */
  public function setAbbreviatedCity($abbreviatedCity)
  {
    $this->abbreviatedCity = $abbreviatedCity;
  }
  /**
   * @return string
   */
  public function getAbbreviatedCity()
  {
    return $this->abbreviatedCity;
  }
  /**
   * @param string
   */
  public function setAddressRecordType($addressRecordType)
  {
    $this->addressRecordType = $addressRecordType;
  }
  /**
   * @return string
   */
  public function getAddressRecordType()
  {
    return $this->addressRecordType;
  }
  /**
   * @param string
   */
  public function setCarrierRoute($carrierRoute)
  {
    $this->carrierRoute = $carrierRoute;
  }
  /**
   * @return string
   */
  public function getCarrierRoute()
  {
    return $this->carrierRoute;
  }
  /**
   * @param string
   */
  public function setCarrierRouteIndicator($carrierRouteIndicator)
  {
    $this->carrierRouteIndicator = $carrierRouteIndicator;
  }
  /**
   * @return string
   */
  public function getCarrierRouteIndicator()
  {
    return $this->carrierRouteIndicator;
  }
  /**
   * @param bool
   */
  public function setCassProcessed($cassProcessed)
  {
    $this->cassProcessed = $cassProcessed;
  }
  /**
   * @return bool
   */
  public function getCassProcessed()
  {
    return $this->cassProcessed;
  }
  /**
   * @param string
   */
  public function setCounty($county)
  {
    $this->county = $county;
  }
  /**
   * @return string
   */
  public function getCounty()
  {
    return $this->county;
  }
  /**
   * @param bool
   */
  public function setDefaultAddress($defaultAddress)
  {
    $this->defaultAddress = $defaultAddress;
  }
  /**
   * @return bool
   */
  public function getDefaultAddress()
  {
    return $this->defaultAddress;
  }
  /**
   * @param string
   */
  public function setDeliveryPointCheckDigit($deliveryPointCheckDigit)
  {
    $this->deliveryPointCheckDigit = $deliveryPointCheckDigit;
  }
  /**
   * @return string
   */
  public function getDeliveryPointCheckDigit()
  {
    return $this->deliveryPointCheckDigit;
  }
  /**
   * @param string
   */
  public function setDeliveryPointCode($deliveryPointCode)
  {
    $this->deliveryPointCode = $deliveryPointCode;
  }
  /**
   * @return string
   */
  public function getDeliveryPointCode()
  {
    return $this->deliveryPointCode;
  }
  /**
   * @param string
   */
  public function setDpvCmra($dpvCmra)
  {
    $this->dpvCmra = $dpvCmra;
  }
  /**
   * @return string
   */
  public function getDpvCmra()
  {
    return $this->dpvCmra;
  }
  /**
   * @param string
   */
  public function setDpvConfirmation($dpvConfirmation)
  {
    $this->dpvConfirmation = $dpvConfirmation;
  }
  /**
   * @return string
   */
  public function getDpvConfirmation()
  {
    return $this->dpvConfirmation;
  }
  /**
   * @param string
   */
  public function setDpvDoorNotAccessible($dpvDoorNotAccessible)
  {
    $this->dpvDoorNotAccessible = $dpvDoorNotAccessible;
  }
  /**
   * @return string
   */
  public function getDpvDoorNotAccessible()
  {
    return $this->dpvDoorNotAccessible;
  }
  /**
   * @param string
   */
  public function setDpvDrop($dpvDrop)
  {
    $this->dpvDrop = $dpvDrop;
  }
  /**
   * @return string
   */
  public function getDpvDrop()
  {
    return $this->dpvDrop;
  }
  /**
   * @param string
   */
  public function setDpvEnhancedDeliveryCode($dpvEnhancedDeliveryCode)
  {
    $this->dpvEnhancedDeliveryCode = $dpvEnhancedDeliveryCode;
  }
  /**
   * @return string
   */
  public function getDpvEnhancedDeliveryCode()
  {
    return $this->dpvEnhancedDeliveryCode;
  }
  /**
   * @param string
   */
  public function setDpvFootnote($dpvFootnote)
  {
    $this->dpvFootnote = $dpvFootnote;
  }
  /**
   * @return string
   */
  public function getDpvFootnote()
  {
    return $this->dpvFootnote;
  }
  /**
   * @param string
   */
  public function setDpvNoSecureLocation($dpvNoSecureLocation)
  {
    $this->dpvNoSecureLocation = $dpvNoSecureLocation;
  }
  /**
   * @return string
   */
  public function getDpvNoSecureLocation()
  {
    return $this->dpvNoSecureLocation;
  }
  /**
   * @param string
   */
  public function setDpvNoStat($dpvNoStat)
  {
    $this->dpvNoStat = $dpvNoStat;
  }
  /**
   * @return string
   */
  public function getDpvNoStat()
  {
    return $this->dpvNoStat;
  }
  /**
   * @param int
   */
  public function setDpvNoStatReasonCode($dpvNoStatReasonCode)
  {
    $this->dpvNoStatReasonCode = $dpvNoStatReasonCode;
  }
  /**
   * @return int
   */
  public function getDpvNoStatReasonCode()
  {
    return $this->dpvNoStatReasonCode;
  }
  /**
   * @param string
   */
  public function setDpvNonDeliveryDays($dpvNonDeliveryDays)
  {
    $this->dpvNonDeliveryDays = $dpvNonDeliveryDays;
  }
  /**
   * @return string
   */
  public function getDpvNonDeliveryDays()
  {
    return $this->dpvNonDeliveryDays;
  }
  /**
   * @param int
   */
  public function setDpvNonDeliveryDaysValues($dpvNonDeliveryDaysValues)
  {
    $this->dpvNonDeliveryDaysValues = $dpvNonDeliveryDaysValues;
  }
  /**
   * @return int
   */
  public function getDpvNonDeliveryDaysValues()
  {
    return $this->dpvNonDeliveryDaysValues;
  }
  /**
   * @param string
   */
  public function setDpvPbsa($dpvPbsa)
  {
    $this->dpvPbsa = $dpvPbsa;
  }
  /**
   * @return string
   */
  public function getDpvPbsa()
  {
    return $this->dpvPbsa;
  }
  /**
   * @param string
   */
  public function setDpvThrowback($dpvThrowback)
  {
    $this->dpvThrowback = $dpvThrowback;
  }
  /**
   * @return string
   */
  public function getDpvThrowback()
  {
    return $this->dpvThrowback;
  }
  /**
   * @param string
   */
  public function setDpvVacant($dpvVacant)
  {
    $this->dpvVacant = $dpvVacant;
  }
  /**
   * @return string
   */
  public function getDpvVacant()
  {
    return $this->dpvVacant;
  }
  /**
   * @param string
   */
  public function setElotFlag($elotFlag)
  {
    $this->elotFlag = $elotFlag;
  }
  /**
   * @return string
   */
  public function getElotFlag()
  {
    return $this->elotFlag;
  }
  /**
   * @param string
   */
  public function setElotNumber($elotNumber)
  {
    $this->elotNumber = $elotNumber;
  }
  /**
   * @return string
   */
  public function getElotNumber()
  {
    return $this->elotNumber;
  }
  /**
   * @param string
   */
  public function setErrorMessage($errorMessage)
  {
    $this->errorMessage = $errorMessage;
  }
  /**
   * @return string
   */
  public function getErrorMessage()
  {
    return $this->errorMessage;
  }
  /**
   * @param bool
   */
  public function setEwsNoMatch($ewsNoMatch)
  {
    $this->ewsNoMatch = $ewsNoMatch;
  }
  /**
   * @return bool
   */
  public function getEwsNoMatch()
  {
    return $this->ewsNoMatch;
  }
  /**
   * @param string
   */
  public function setFipsCountyCode($fipsCountyCode)
  {
    $this->fipsCountyCode = $fipsCountyCode;
  }
  /**
   * @return string
   */
  public function getFipsCountyCode()
  {
    return $this->fipsCountyCode;
  }
  /**
   * @param string
   */
  public function setLacsLinkIndicator($lacsLinkIndicator)
  {
    $this->lacsLinkIndicator = $lacsLinkIndicator;
  }
  /**
   * @return string
   */
  public function getLacsLinkIndicator()
  {
    return $this->lacsLinkIndicator;
  }
  /**
   * @param string
   */
  public function setLacsLinkReturnCode($lacsLinkReturnCode)
  {
    $this->lacsLinkReturnCode = $lacsLinkReturnCode;
  }
  /**
   * @return string
   */
  public function getLacsLinkReturnCode()
  {
    return $this->lacsLinkReturnCode;
  }
  /**
   * @param string
   */
  public function setPmbDesignator($pmbDesignator)
  {
    $this->pmbDesignator = $pmbDesignator;
  }
  /**
   * @return string
   */
  public function getPmbDesignator()
  {
    return $this->pmbDesignator;
  }
  /**
   * @param string
   */
  public function setPmbNumber($pmbNumber)
  {
    $this->pmbNumber = $pmbNumber;
  }
  /**
   * @return string
   */
  public function getPmbNumber()
  {
    return $this->pmbNumber;
  }
  /**
   * @param bool
   */
  public function setPoBoxOnlyPostalCode($poBoxOnlyPostalCode)
  {
    $this->poBoxOnlyPostalCode = $poBoxOnlyPostalCode;
  }
  /**
   * @return bool
   */
  public function getPoBoxOnlyPostalCode()
  {
    return $this->poBoxOnlyPostalCode;
  }
  /**
   * @param string
   */
  public function setPostOfficeCity($postOfficeCity)
  {
    $this->postOfficeCity = $postOfficeCity;
  }
  /**
   * @return string
   */
  public function getPostOfficeCity()
  {
    return $this->postOfficeCity;
  }
  /**
   * @param string
   */
  public function setPostOfficeState($postOfficeState)
  {
    $this->postOfficeState = $postOfficeState;
  }
  /**
   * @return string
   */
  public function getPostOfficeState()
  {
    return $this->postOfficeState;
  }
  /**
   * @param GoogleMapsAddressvalidationV1UspsAddress
   */
  public function setStandardizedAddress(GoogleMapsAddressvalidationV1UspsAddress $standardizedAddress)
  {
    $this->standardizedAddress = $standardizedAddress;
  }
  /**
   * @return GoogleMapsAddressvalidationV1UspsAddress
   */
  public function getStandardizedAddress()
  {
    return $this->standardizedAddress;
  }
  /**
   * @param string
   */
  public function setSuitelinkFootnote($suitelinkFootnote)
  {
    $this->suitelinkFootnote = $suitelinkFootnote;
  }
  /**
   * @return string
   */
  public function getSuitelinkFootnote()
  {
    return $this->suitelinkFootnote;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsAddressvalidationV1UspsData::class, 'Google_Service_AddressValidation_GoogleMapsAddressvalidationV1UspsData');
