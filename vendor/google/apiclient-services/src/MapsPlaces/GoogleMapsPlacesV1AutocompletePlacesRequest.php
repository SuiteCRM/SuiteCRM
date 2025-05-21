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

namespace Google\Service\MapsPlaces;

class GoogleMapsPlacesV1AutocompletePlacesRequest extends \Google\Collection
{
  protected $collection_key = 'includedRegionCodes';
  /**
   * @var bool
   */
  public $includePureServiceAreaBusinesses;
  /**
   * @var bool
   */
  public $includeQueryPredictions;
  /**
   * @var string[]
   */
  public $includedPrimaryTypes;
  /**
   * @var string[]
   */
  public $includedRegionCodes;
  /**
   * @var string
   */
  public $input;
  /**
   * @var int
   */
  public $inputOffset;
  /**
   * @var string
   */
  public $languageCode;
  protected $locationBiasType = GoogleMapsPlacesV1AutocompletePlacesRequestLocationBias::class;
  protected $locationBiasDataType = '';
  protected $locationRestrictionType = GoogleMapsPlacesV1AutocompletePlacesRequestLocationRestriction::class;
  protected $locationRestrictionDataType = '';
  protected $originType = GoogleTypeLatLng::class;
  protected $originDataType = '';
  /**
   * @var string
   */
  public $regionCode;
  /**
   * @var string
   */
  public $sessionToken;

  /**
   * @param bool
   */
  public function setIncludePureServiceAreaBusinesses($includePureServiceAreaBusinesses)
  {
    $this->includePureServiceAreaBusinesses = $includePureServiceAreaBusinesses;
  }
  /**
   * @return bool
   */
  public function getIncludePureServiceAreaBusinesses()
  {
    return $this->includePureServiceAreaBusinesses;
  }
  /**
   * @param bool
   */
  public function setIncludeQueryPredictions($includeQueryPredictions)
  {
    $this->includeQueryPredictions = $includeQueryPredictions;
  }
  /**
   * @return bool
   */
  public function getIncludeQueryPredictions()
  {
    return $this->includeQueryPredictions;
  }
  /**
   * @param string[]
   */
  public function setIncludedPrimaryTypes($includedPrimaryTypes)
  {
    $this->includedPrimaryTypes = $includedPrimaryTypes;
  }
  /**
   * @return string[]
   */
  public function getIncludedPrimaryTypes()
  {
    return $this->includedPrimaryTypes;
  }
  /**
   * @param string[]
   */
  public function setIncludedRegionCodes($includedRegionCodes)
  {
    $this->includedRegionCodes = $includedRegionCodes;
  }
  /**
   * @return string[]
   */
  public function getIncludedRegionCodes()
  {
    return $this->includedRegionCodes;
  }
  /**
   * @param string
   */
  public function setInput($input)
  {
    $this->input = $input;
  }
  /**
   * @return string
   */
  public function getInput()
  {
    return $this->input;
  }
  /**
   * @param int
   */
  public function setInputOffset($inputOffset)
  {
    $this->inputOffset = $inputOffset;
  }
  /**
   * @return int
   */
  public function getInputOffset()
  {
    return $this->inputOffset;
  }
  /**
   * @param string
   */
  public function setLanguageCode($languageCode)
  {
    $this->languageCode = $languageCode;
  }
  /**
   * @return string
   */
  public function getLanguageCode()
  {
    return $this->languageCode;
  }
  /**
   * @param GoogleMapsPlacesV1AutocompletePlacesRequestLocationBias
   */
  public function setLocationBias(GoogleMapsPlacesV1AutocompletePlacesRequestLocationBias $locationBias)
  {
    $this->locationBias = $locationBias;
  }
  /**
   * @return GoogleMapsPlacesV1AutocompletePlacesRequestLocationBias
   */
  public function getLocationBias()
  {
    return $this->locationBias;
  }
  /**
   * @param GoogleMapsPlacesV1AutocompletePlacesRequestLocationRestriction
   */
  public function setLocationRestriction(GoogleMapsPlacesV1AutocompletePlacesRequestLocationRestriction $locationRestriction)
  {
    $this->locationRestriction = $locationRestriction;
  }
  /**
   * @return GoogleMapsPlacesV1AutocompletePlacesRequestLocationRestriction
   */
  public function getLocationRestriction()
  {
    return $this->locationRestriction;
  }
  /**
   * @param GoogleTypeLatLng
   */
  public function setOrigin(GoogleTypeLatLng $origin)
  {
    $this->origin = $origin;
  }
  /**
   * @return GoogleTypeLatLng
   */
  public function getOrigin()
  {
    return $this->origin;
  }
  /**
   * @param string
   */
  public function setRegionCode($regionCode)
  {
    $this->regionCode = $regionCode;
  }
  /**
   * @return string
   */
  public function getRegionCode()
  {
    return $this->regionCode;
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
class_alias(GoogleMapsPlacesV1AutocompletePlacesRequest::class, 'Google_Service_MapsPlaces_GoogleMapsPlacesV1AutocompletePlacesRequest');
