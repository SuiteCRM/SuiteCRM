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

namespace Google\Service\WorkloadManager\Resource;

use Google\Service\WorkloadManager\WriteInsightRequest;
use Google\Service\WorkloadManager\WriteInsightResponse;

/**
 * The "insights" collection of methods.
 * Typical usage is:
 *  <code>
 *   $workloadmanagerService = new Google\Service\WorkloadManager(...);
 *   $insights = $workloadmanagerService->projects_locations_insights;
 *  </code>
 */
class ProjectsLocationsInsights extends \Google\Service\Resource
{
  /**
   * Write the data insights to workload manager data warehouse.
   * (insights.writeInsight)
   *
   * @param string $location Required. The GCP location. The format is:
   * projects/{project}/locations/{location}.
   * @param WriteInsightRequest $postBody
   * @param array $optParams Optional parameters.
   * @return WriteInsightResponse
   * @throws \Google\Service\Exception
   */
  public function writeInsight($location, WriteInsightRequest $postBody, $optParams = [])
  {
    $params = ['location' => $location, 'postBody' => $postBody];
    $params = array_merge($params, $optParams);
    return $this->call('writeInsight', [$params], WriteInsightResponse::class);
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProjectsLocationsInsights::class, 'Google_Service_WorkloadManager_Resource_ProjectsLocationsInsights');
