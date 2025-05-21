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

class GoogleMapsPlacesV1SearchNearbyResponse extends \Google\Collection
{
  protected $collection_key = 'routingSummaries';
  protected $placesType = GoogleMapsPlacesV1Place::class;
  protected $placesDataType = 'array';
  protected $routingSummariesType = GoogleMapsPlacesV1RoutingSummary::class;
  protected $routingSummariesDataType = 'array';

  /**
   * @param GoogleMapsPlacesV1Place[]
   */
  public function setPlaces($places)
  {
    $this->places = $places;
  }
  /**
   * @return GoogleMapsPlacesV1Place[]
   */
  public function getPlaces()
  {
    return $this->places;
  }
  /**
   * @param GoogleMapsPlacesV1RoutingSummary[]
   */
  public function setRoutingSummaries($routingSummaries)
  {
    $this->routingSummaries = $routingSummaries;
  }
  /**
   * @return GoogleMapsPlacesV1RoutingSummary[]
   */
  public function getRoutingSummaries()
  {
    return $this->routingSummaries;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsPlacesV1SearchNearbyResponse::class, 'Google_Service_MapsPlaces_GoogleMapsPlacesV1SearchNearbyResponse');
