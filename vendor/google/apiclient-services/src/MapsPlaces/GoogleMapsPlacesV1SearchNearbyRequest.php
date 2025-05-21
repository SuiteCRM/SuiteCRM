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

class GoogleMapsPlacesV1SearchNearbyRequest extends \Google\Collection
{
  protected $collection_key = 'includedTypes';
  /**
   * @var string[]
   */
  public $excludedPrimaryTypes;
  /**
   * @var string[]
   */
  public $excludedTypes;
  /**
   * @var string[]
   */
  public $includedPrimaryTypes;
  /**
   * @var string[]
   */
  public $includedTypes;
  /**
   * @var string
   */
  public $languageCode;
  protected $locationRestrictionType = GoogleMapsPlacesV1SearchNearbyRequestLocationRestriction::class;
  protected $locationRestrictionDataType = '';
  /**
   * @var int
   */
  public $maxResultCount;
  /**
   * @var string
   */
  public $rankPreference;
  /**
   * @var string
   */
  public $regionCode;
  protected $routingParametersType = GoogleMapsPlacesV1RoutingParameters::class;
  protected $routingParametersDataType = '';

  /**
   * @param string[]
   */
  public function setExcludedPrimaryTypes($excludedPrimaryTypes)
  {
    $this->excludedPrimaryTypes = $excludedPrimaryTypes;
  }
  /**
   * @return string[]
   */
  public function getExcludedPrimaryTypes()
  {
    return $this->excludedPrimaryTypes;
  }
  /**
   * @param string[]
   */
  public function setExcludedTypes($excludedTypes)
  {
    $this->excludedTypes = $excludedTypes;
  }
  /**
   * @return string[]
   */
  public function getExcludedTypes()
  {
    return $this->excludedTypes;
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
  public function setIncludedTypes($includedTypes)
  {
    $this->includedTypes = $includedTypes;
  }
  /**
   * @return string[]
   */
  public function getIncludedTypes()
  {
    return $this->includedTypes;
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
   * @param GoogleMapsPlacesV1SearchNearbyRequestLocationRestriction
   */
  public function setLocationRestriction(GoogleMapsPlacesV1SearchNearbyRequestLocationRestriction $locationRestriction)
  {
    $this->locationRestriction = $locationRestriction;
  }
  /**
   * @return GoogleMapsPlacesV1SearchNearbyRequestLocationRestriction
   */
  public function getLocationRestriction()
  {
    return $this->locationRestriction;
  }
  /**
   * @param int
   */
  public function setMaxResultCount($maxResultCount)
  {
    $this->maxResultCount = $maxResultCount;
  }
  /**
   * @return int
   */
  public function getMaxResultCount()
  {
    return $this->maxResultCount;
  }
  /**
   * @param string
   */
  public function setRankPreference($rankPreference)
  {
    $this->rankPreference = $rankPreference;
  }
  /**
   * @return string
   */
  public function getRankPreference()
  {
    return $this->rankPreference;
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
   * @param GoogleMapsPlacesV1RoutingParameters
   */
  public function setRoutingParameters(GoogleMapsPlacesV1RoutingParameters $routingParameters)
  {
    $this->routingParameters = $routingParameters;
  }
  /**
   * @return GoogleMapsPlacesV1RoutingParameters
   */
  public function getRoutingParameters()
  {
    return $this->routingParameters;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsPlacesV1SearchNearbyRequest::class, 'Google_Service_MapsPlaces_GoogleMapsPlacesV1SearchNearbyRequest');
