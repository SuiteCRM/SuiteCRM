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

namespace Google\Service\TravelImpactModel\Resource;

use Google\Service\TravelImpactModel\ComputeFlightEmissionsRequest;
use Google\Service\TravelImpactModel\ComputeFlightEmissionsResponse;

/**
 * The "flights" collection of methods.
 * Typical usage is:
 *  <code>
 *   $travelimpactmodelService = new Google\Service\TravelImpactModel(...);
 *   $flights = $travelimpactmodelService->flights;
 *  </code>
 */
class Flights extends \Google\Service\Resource
{
  /**
   * Stateless method to retrieve emission estimates. Details on how emission
   * estimates are computed: https://github.com/google/travel-impact-model The
   * response will contain all entries that match the input flight legs, in the
   * same order. If there are no estimates available for a certain flight leg, the
   * response will return the flight leg object with empty emission fields. The
   * request will still be considered successful. Reasons for missing emission
   * estimates include: - The flight is unknown to the server. - The input flight
   * leg is missing one or more identifiers. - The flight date is in the past. -
   * The aircraft type is not supported by the model. - Missing seat
   * configuration. The request can contain up to 1000 flight legs. If the request
   * has more than 1000 direct flights, if will fail with an INVALID_ARGUMENT
   * error. (flights.computeFlightEmissions)
   *
   * @param ComputeFlightEmissionsRequest $postBody
   * @param array $optParams Optional parameters.
   * @return ComputeFlightEmissionsResponse
   * @throws \Google\Service\Exception
   */
  public function computeFlightEmissions(ComputeFlightEmissionsRequest $postBody, $optParams = [])
  {
    $params = ['postBody' => $postBody];
    $params = array_merge($params, $optParams);
    return $this->call('computeFlightEmissions', [$params], ComputeFlightEmissionsResponse::class);
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Flights::class, 'Google_Service_TravelImpactModel_Resource_Flights');
