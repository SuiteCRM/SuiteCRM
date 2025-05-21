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

namespace Google\Service\NetworkSecurity\Resource;

use Google\Service\NetworkSecurity\ListMirroringEndpointGroupAssociationsResponse;
use Google\Service\NetworkSecurity\MirroringEndpointGroupAssociation;
use Google\Service\NetworkSecurity\Operation;

/**
 * The "mirroringEndpointGroupAssociations" collection of methods.
 * Typical usage is:
 *  <code>
 *   $networksecurityService = new Google\Service\NetworkSecurity(...);
 *   $mirroringEndpointGroupAssociations = $networksecurityService->projects_locations_mirroringEndpointGroupAssociations;
 *  </code>
 */
class ProjectsLocationsMirroringEndpointGroupAssociations extends \Google\Service\Resource
{
  /**
   * Creates an association in a given project and location. See
   * https://google.aip.dev/133. (mirroringEndpointGroupAssociations.create)
   *
   * @param string $parent Required. Container (project and location) where the
   * association will be created, e.g. `projects/123456789/locations/global`.
   * @param MirroringEndpointGroupAssociation $postBody
   * @param array $optParams Optional parameters.
   *
   * @opt_param string mirroringEndpointGroupAssociationId Optional. ID for the
   * new association. If not provided, the server will generate a unique ID. The
   * ID must be a valid RFC 1035 resource name. The ID must be 1-63 characters
   * long and match the regular expression `[a-z]([-a-z0-9]*[a-z0-9])?`. The first
   * character must be a lowercase letter, and all following characters (except
   * for the last character) must be a dash, lowercase letter, or digit. The last
   * character must be a
   * @opt_param string requestId Optional. An optional request ID to identify
   * requests. Specify a unique request ID so that if you must retry your request,
   * the server will know to ignore the request if it has already been completed.
   * The server will guarantee that for at least 60 minutes since the first
   * request. For example, consider a situation where you make an initial request
   * and the request times out. If you make the request again with the same
   * request ID, the server can check if original operation with the same request
   * ID was received, and if so, will ignore the second request. This prevents
   * clients from accidentally creating duplicate commitments. The request ID must
   * be a valid UUID with the exception that zero UUID is not supported
   * (00000000-0000-0000-0000-000000000000).
   * @return Operation
   * @throws \Google\Service\Exception
   */
  public function create($parent, MirroringEndpointGroupAssociation $postBody, $optParams = [])
  {
    $params = ['parent' => $parent, 'postBody' => $postBody];
    $params = array_merge($params, $optParams);
    return $this->call('create', [$params], Operation::class);
  }
  /**
   * Deletes a single association. See https://google.aip.dev/135.
   * (mirroringEndpointGroupAssociations.delete)
   *
   * @param string $name Required. Full resource name of the association to
   * delete, e.g.
   * projects/123456789/locations/global/mirroringEndpointGroupAssociations/my-eg-
   * association.
   * @param array $optParams Optional parameters.
   *
   * @opt_param string requestId Optional. An optional request ID to identify
   * requests. Specify a unique request ID so that if you must retry your request,
   * the server will know to ignore the request if it has already been completed.
   * The server will guarantee that for at least 60 minutes after the first
   * request. For example, consider a situation where you make an initial request
   * and the request times out. If you make the request again with the same
   * request ID, the server can check if original operation with the same request
   * ID was received, and if so, will ignore the second request. This prevents
   * clients from accidentally creating duplicate commitments. The request ID must
   * be a valid UUID with the exception that zero UUID is not supported
   * (00000000-0000-0000-0000-000000000000).
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
   * Gets a specific association. See https://google.aip.dev/131.
   * (mirroringEndpointGroupAssociations.get)
   *
   * @param string $name Required. Full resource name of the association to get,
   * e.g.
   * projects/123456789/locations/global/mirroringEndpointGroupAssociations/my-eg-
   * association.
   * @param array $optParams Optional parameters.
   * @return MirroringEndpointGroupAssociation
   * @throws \Google\Service\Exception
   */
  public function get($name, $optParams = [])
  {
    $params = ['name' => $name];
    $params = array_merge($params, $optParams);
    return $this->call('get', [$params], MirroringEndpointGroupAssociation::class);
  }
  /**
   * Lists associations in a given project and location. See
   * https://google.aip.dev/132. (mirroringEndpointGroupAssociations.listProjectsL
   * ocationsMirroringEndpointGroupAssociations)
   *
   * @param string $parent Required. Parent container (project and location) of
   * the associations to list, e.g. `projects/123456789/locations/global`.
   * @param array $optParams Optional parameters.
   *
   * @opt_param string filter Optional. A filter expression that filters the
   * results listed in the response. See https://google.aip.dev/160.
   * @opt_param string orderBy Optional. Hint for how to order the results
   * @opt_param int pageSize Optional. Requested page size. Server may return
   * fewer items than requested. If unspecified, server will pick an appropriate
   * default. See https://google.aip.dev/158.
   * @opt_param string pageToken Optional. A token identifying a page of results
   * the server should return. See https://google.aip.dev/158.
   * @return ListMirroringEndpointGroupAssociationsResponse
   * @throws \Google\Service\Exception
   */
  public function listProjectsLocationsMirroringEndpointGroupAssociations($parent, $optParams = [])
  {
    $params = ['parent' => $parent];
    $params = array_merge($params, $optParams);
    return $this->call('list', [$params], ListMirroringEndpointGroupAssociationsResponse::class);
  }
  /**
   * Updates an association. See https://google.aip.dev/134.
   * (mirroringEndpointGroupAssociations.patch)
   *
   * @param string $name Immutable. Identifier. The resource name of this endpoint
   * group association, for example:
   * `projects/123456789/locations/global/mirroringEndpointGroupAssociations/my-
   * eg-association`. See https://google.aip.dev/122 for more details.
   * @param MirroringEndpointGroupAssociation $postBody
   * @param array $optParams Optional parameters.
   *
   * @opt_param string requestId Optional. An optional request ID to identify
   * requests. Specify a unique request ID so that if you must retry your request,
   * the server will know to ignore the request if it has already been completed.
   * The server will guarantee that for at least 60 minutes since the first
   * request. For example, consider a situation where you make an initial request
   * and the request times out. If you make the request again with the same
   * request ID, the server can check if original operation with the same request
   * ID was received, and if so, will ignore the second request. This prevents
   * clients from accidentally creating duplicate commitments. The request ID must
   * be a valid UUID with the exception that zero UUID is not supported
   * (00000000-0000-0000-0000-000000000000).
   * @opt_param string updateMask Optional. Field mask is used to specify the
   * fields to be overwritten in the association by the update. See
   * https://google.aip.dev/161.
   * @return Operation
   * @throws \Google\Service\Exception
   */
  public function patch($name, MirroringEndpointGroupAssociation $postBody, $optParams = [])
  {
    $params = ['name' => $name, 'postBody' => $postBody];
    $params = array_merge($params, $optParams);
    return $this->call('patch', [$params], Operation::class);
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProjectsLocationsMirroringEndpointGroupAssociations::class, 'Google_Service_NetworkSecurity_Resource_ProjectsLocationsMirroringEndpointGroupAssociations');
