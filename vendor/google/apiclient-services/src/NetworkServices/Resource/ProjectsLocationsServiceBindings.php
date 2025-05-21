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

namespace Google\Service\NetworkServices\Resource;

use Google\Service\NetworkServices\ListServiceBindingsResponse;
use Google\Service\NetworkServices\Operation;
use Google\Service\NetworkServices\ServiceBinding;

/**
 * The "serviceBindings" collection of methods.
 * Typical usage is:
 *  <code>
 *   $networkservicesService = new Google\Service\NetworkServices(...);
 *   $serviceBindings = $networkservicesService->projects_locations_serviceBindings;
 *  </code>
 */
class ProjectsLocationsServiceBindings extends \Google\Service\Resource
{
  /**
   * Creates a new ServiceBinding in a given project and location.
   * (serviceBindings.create)
   *
   * @param string $parent Required. The parent resource of the ServiceBinding.
   * Must be in the format `projects/locations/global`.
   * @param ServiceBinding $postBody
   * @param array $optParams Optional parameters.
   *
   * @opt_param string serviceBindingId Required. Short name of the ServiceBinding
   * resource to be created.
   * @return Operation
   * @throws \Google\Service\Exception
   */
  public function create($parent, ServiceBinding $postBody, $optParams = [])
  {
    $params = ['parent' => $parent, 'postBody' => $postBody];
    $params = array_merge($params, $optParams);
    return $this->call('create', [$params], Operation::class);
  }
  /**
   * Deletes a single ServiceBinding. (serviceBindings.delete)
   *
   * @param string $name Required. A name of the ServiceBinding to delete. Must be
   * in the format `projects/locations/global/serviceBindings`.
   * @param array $optParams Optional parameters.
   * @return Operation
   * @throws \Google\Service\Exception
   */
  public function delete($name, $optParams = [])
  {
    $params = ['name' => $name];
    $params = array_merge($params, $optParams);
    return $this->call('delete', [$params], Operation::class);
  }
  /**
   * Gets details of a single ServiceBinding. (serviceBindings.get)
   *
   * @param string $name Required. A name of the ServiceBinding to get. Must be in
   * the format `projects/locations/global/serviceBindings`.
   * @param array $optParams Optional parameters.
   * @return ServiceBinding
   * @throws \Google\Service\Exception
   */
  public function get($name, $optParams = [])
  {
    $params = ['name' => $name];
    $params = array_merge($params, $optParams);
    return $this->call('get', [$params], ServiceBinding::class);
  }
  /**
   * Lists ServiceBinding in a given project and location.
   * (serviceBindings.listProjectsLocationsServiceBindings)
   *
   * @param string $parent Required. The project and location from which the
   * ServiceBindings should be listed, specified in the format
   * `projects/locations/global`.
   * @param array $optParams Optional parameters.
   *
   * @opt_param int pageSize Maximum number of ServiceBindings to return per call.
   * @opt_param string pageToken The value returned by the last
   * `ListServiceBindingsResponse` Indicates that this is a continuation of a
   * prior `ListRouters` call, and that the system should return the next page of
   * data.
   * @return ListServiceBindingsResponse
   * @throws \Google\Service\Exception
   */
  public function listProjectsLocationsServiceBindings($parent, $optParams = [])
  {
    $params = ['parent' => $parent];
    $params = array_merge($params, $optParams);
    return $this->call('list', [$params], ListServiceBindingsResponse::class);
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProjectsLocationsServiceBindings::class, 'Google_Service_NetworkServices_Resource_ProjectsLocationsServiceBindings');
