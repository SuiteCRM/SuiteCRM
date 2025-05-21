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

namespace Google\Service\AppHub\Resource;

use Google\Service\AppHub\DetachServiceProjectAttachmentRequest;
use Google\Service\AppHub\DetachServiceProjectAttachmentResponse;
use Google\Service\AppHub\ListLocationsResponse;
use Google\Service\AppHub\Location;
use Google\Service\AppHub\LookupServiceProjectAttachmentResponse;

/**
 * The "locations" collection of methods.
 * Typical usage is:
 *  <code>
 *   $apphubService = new Google\Service\AppHub(...);
 *   $locations = $apphubService->projects_locations;
 *  </code>
 */
class ProjectsLocations extends \Google\Service\Resource
{
  /**
   * Detaches a service project from a host project. You can call this API from
   * any service project without needing access to the host project that it is
   * attached to. (locations.detachServiceProjectAttachment)
   *
   * @param string $name Required. Service project id and location to detach from
   * a host project. Only global location is supported. Expected format:
   * `projects/{project}/locations/{location}`.
   * @param DetachServiceProjectAttachmentRequest $postBody
   * @param array $optParams Optional parameters.
   * @return DetachServiceProjectAttachmentResponse
   * @throws \Google\Service\Exception
   */
  public function detachServiceProjectAttachment($name, DetachServiceProjectAttachmentRequest $postBody, $optParams = [])
  {
    $params = ['name' => $name, 'postBody' => $postBody];
    $params = array_merge($params, $optParams);
    return $this->call('detachServiceProjectAttachment', [$params], DetachServiceProjectAttachmentResponse::class);
  }
  /**
   * Gets information about a location. (locations.get)
   *
   * @param string $name Resource name for the location.
   * @param array $optParams Optional parameters.
   * @return Location
   * @throws \Google\Service\Exception
   */
  public function get($name, $optParams = [])
  {
    $params = ['name' => $name];
    $params = array_merge($params, $optParams);
    return $this->call('get', [$params], Location::class);
  }
  /**
   * Lists information about the supported locations for this service.
   * (locations.listProjectsLocations)
   *
   * @param string $name The resource that owns the locations collection, if
   * applicable.
   * @param array $optParams Optional parameters.
   *
   * @opt_param string filter A filter to narrow down results to a preferred
   * subset. The filtering language accepts strings like `"displayName=tokyo"`,
   * and is documented in more detail in [AIP-160](https://google.aip.dev/160).
   * @opt_param int pageSize The maximum number of results to return. If not set,
   * the service selects a default.
   * @opt_param string pageToken A page token received from the `next_page_token`
   * field in the response. Send that page token to receive the subsequent page.
   * @return ListLocationsResponse
   * @throws \Google\Service\Exception
   */
  public function listProjectsLocations($name, $optParams = [])
  {
    $params = ['name' => $name];
    $params = array_merge($params, $optParams);
    return $this->call('list', [$params], ListLocationsResponse::class);
  }
  /**
   * Lists a service project attachment for a given service project. You can call
   * this API from any project to find if it is attached to a host project.
   * (locations.lookupServiceProjectAttachment)
   *
   * @param string $name Required. Service project ID and location to lookup
   * service project attachment for. Only global location is supported. Expected
   * format: `projects/{project}/locations/{location}`.
   * @param array $optParams Optional parameters.
   * @return LookupServiceProjectAttachmentResponse
   * @throws \Google\Service\Exception
   */
  public function lookupServiceProjectAttachment($name, $optParams = [])
  {
    $params = ['name' => $name];
    $params = array_merge($params, $optParams);
    return $this->call('lookupServiceProjectAttachment', [$params], LookupServiceProjectAttachmentResponse::class);
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProjectsLocations::class, 'Google_Service_AppHub_Resource_ProjectsLocations');
