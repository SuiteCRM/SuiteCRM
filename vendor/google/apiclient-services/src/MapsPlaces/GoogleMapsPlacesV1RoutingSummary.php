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

class GoogleMapsPlacesV1RoutingSummary extends \Google\Collection
{
  protected $collection_key = 'legs';
  /**
   * @var string
   */
  public $directionsUri;
  protected $legsType = GoogleMapsPlacesV1RoutingSummaryLeg::class;
  protected $legsDataType = 'array';

  /**
   * @param string
   */
  public function setDirectionsUri($directionsUri)
  {
    $this->directionsUri = $directionsUri;
  }
  /**
   * @return string
   */
  public function getDirectionsUri()
  {
    return $this->directionsUri;
  }
  /**
   * @param GoogleMapsPlacesV1RoutingSummaryLeg[]
   */
  public function setLegs($legs)
  {
    $this->legs = $legs;
  }
  /**
   * @return GoogleMapsPlacesV1RoutingSummaryLeg[]
   */
  public function getLegs()
  {
    return $this->legs;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsPlacesV1RoutingSummary::class, 'Google_Service_MapsPlaces_GoogleMapsPlacesV1RoutingSummary');
