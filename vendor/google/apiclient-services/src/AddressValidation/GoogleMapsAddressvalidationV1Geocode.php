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

class GoogleMapsAddressvalidationV1Geocode extends \Google\Collection
{
  protected $collection_key = 'placeTypes';
  protected $boundsType = GoogleGeoTypeViewport::class;
  protected $boundsDataType = '';
  /**
   * @var float
   */
  public $featureSizeMeters;
  protected $locationType = GoogleTypeLatLng::class;
  protected $locationDataType = '';
  /**
   * @var string
   */
  public $placeId;
  /**
   * @var string[]
   */
  public $placeTypes;
  protected $plusCodeType = GoogleMapsAddressvalidationV1PlusCode::class;
  protected $plusCodeDataType = '';

  /**
   * @param GoogleGeoTypeViewport
   */
  public function setBounds(GoogleGeoTypeViewport $bounds)
  {
    $this->bounds = $bounds;
  }
  /**
   * @return GoogleGeoTypeViewport
   */
  public function getBounds()
  {
    return $this->bounds;
  }
  /**
   * @param float
   */
  public function setFeatureSizeMeters($featureSizeMeters)
  {
    $this->featureSizeMeters = $featureSizeMeters;
  }
  /**
   * @return float
   */
  public function getFeatureSizeMeters()
  {
    return $this->featureSizeMeters;
  }
  /**
   * @param GoogleTypeLatLng
   */
  public function setLocation(GoogleTypeLatLng $location)
  {
    $this->location = $location;
  }
  /**
   * @return GoogleTypeLatLng
   */
  public function getLocation()
  {
    return $this->location;
  }
  /**
   * @param string
   */
  public function setPlaceId($placeId)
  {
    $this->placeId = $placeId;
  }
  /**
   * @return string
   */
  public function getPlaceId()
  {
    return $this->placeId;
  }
  /**
   * @param string[]
   */
  public function setPlaceTypes($placeTypes)
  {
    $this->placeTypes = $placeTypes;
  }
  /**
   * @return string[]
   */
  public function getPlaceTypes()
  {
    return $this->placeTypes;
  }
  /**
   * @param GoogleMapsAddressvalidationV1PlusCode
   */
  public function setPlusCode(GoogleMapsAddressvalidationV1PlusCode $plusCode)
  {
    $this->plusCode = $plusCode;
  }
  /**
   * @return GoogleMapsAddressvalidationV1PlusCode
   */
  public function getPlusCode()
  {
    return $this->plusCode;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsAddressvalidationV1Geocode::class, 'Google_Service_AddressValidation_GoogleMapsAddressvalidationV1Geocode');
