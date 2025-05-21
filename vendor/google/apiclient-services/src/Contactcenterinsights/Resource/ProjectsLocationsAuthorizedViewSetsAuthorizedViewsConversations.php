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

namespace Google\Service\Contactcenterinsights\Resource;

use Google\Service\Contactcenterinsights\GoogleCloudContactcenterinsightsV1CalculateStatsResponse;

/**
 * The "conversations" collection of methods.
 * Typical usage is:
 *  <code>
 *   $contactcenterinsightsService = new Google\Service\Contactcenterinsights(...);
 *   $conversations = $contactcenterinsightsService->projects_locations_authorizedViewSets_authorizedViews_conversations;
 *  </code>
 */
class ProjectsLocationsAuthorizedViewSetsAuthorizedViewsConversations extends \Google\Service\Resource
{
  /**
   * Gets conversation statistics. (conversations.calculateStats)
   *
   * @param string $location Required. The location of the conversations.
   * @param array $optParams Optional parameters.
   *
   * @opt_param string filter A filter to reduce results to a specific subset.
   * This field is useful for getting statistics about conversations with specific
   * properties.
   * @return GoogleCloudContactcenterinsightsV1CalculateStatsResponse
   * @throws \Google\Service\Exception
   */
  public function calculateStats($location, $optParams = [])
  {
    $params = ['location' => $location];
    $params = array_merge($params, $optParams);
    return $this->call('calculateStats', [$params], GoogleCloudContactcenterinsightsV1CalculateStatsResponse::class);
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProjectsLocationsAuthorizedViewSetsAuthorizedViewsConversations::class, 'Google_Service_Contactcenterinsights_Resource_ProjectsLocationsAuthorizedViewSetsAuthorizedViewsConversations');
