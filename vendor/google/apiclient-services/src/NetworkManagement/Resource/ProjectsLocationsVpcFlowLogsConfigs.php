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

namespace Google\Service\NetworkManagement\Resource;

use Google\Service\NetworkManagement\ListVpcFlowLogsConfigsResponse;
use Google\Service\NetworkManagement\Operation;
use Google\Service\NetworkManagement\VpcFlowLogsConfig;

/**
 * The "vpcFlowLogsConfigs" collection of methods.
 * Typical usage is:
 *  <code>
 *   $networkmanagementService = new Google\Service\NetworkManagement(...);
 *   $vpcFlowLogsConfigs = $networkmanagementService->projects_locations_vpcFlowLogsConfigs;
 *  </code>
 */
class ProjectsLocationsVpcFlowLogsConfigs extends \Google\Service\Resource
{
  /**
   * Creates a new `VpcFlowLogsConfig`. If a configuration with the exact same
   * settings already exists (even if the ID is different), the creation fails.
   * Notes: 1. Creating a configuration with state=DISABLED will fail 2. The
   * following fields are not considered as `settings` for the purpose of the
   * check mentioned above, therefore - creating another configuration with the
   * same fields but different values for the following fields will fail as well:
   * * name * create_time * update_time * labels * description
   * (vpcFlowLogsConfigs.create)
   *
   * @param string $parent Required. The parent resource of the VPC Flow Logs
   * configuration to create: `projects/{project_id}/locations/global`
   * @param VpcFlowLogsConfig $postBody
   * @param array $optParams Optional parameters.
   *
   * @opt_param string vpcFlowLogsConfigId Required. ID of the
   * `VpcFlowLogsConfig`.
   * @return Operation
   * @throws \Google\Service\Exception
   */
  public function create($parent, VpcFlowLogsConfig $postBody, $optParams = [])
  {
    $params = ['parent' => $parent, 'postBody' => $postBody];
    $params = array_merge($params, $optParams);
    return $this->call('create', [$params], Operation::class);
  }
  /**
   * Deletes a specific `VpcFlowLogsConfig`. (vpcFlowLogsConfigs.delete)
   *
   * @param string $name Required. `VpcFlowLogsConfig` resource name using the
   * form: `projects/{project_id}/locations/global/vpcFlowLogsConfigs/{vpc_flow_lo
   * gs_config}`
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
   * Gets the details of a specific `VpcFlowLogsConfig`. (vpcFlowLogsConfigs.get)
   *
   * @param string $name Required. `VpcFlowLogsConfig` resource name using the
   * form: `projects/{project_id}/locations/global/vpcFlowLogsConfigs/{vpc_flow_lo
   * gs_config}`
   * @param array $optParams Optional parameters.
   * @return VpcFlowLogsConfig
   * @throws \Google\Service\Exception
   */
  public function get($name, $optParams = [])
  {
    $params = ['name' => $name];
    $params = array_merge($params, $optParams);
    return $this->call('get', [$params], VpcFlowLogsConfig::class);
  }
  /**
   * Lists all `VpcFlowLogsConfigs` in a given project.
   * (vpcFlowLogsConfigs.listProjectsLocationsVpcFlowLogsConfigs)
   *
   * @param string $parent Required. The parent resource of the VpcFlowLogsConfig:
   * `projects/{project_id}/locations/global`
   * @param array $optParams Optional parameters.
   *
   * @opt_param string filter Optional. Lists the `VpcFlowLogsConfigs` that match
   * the filter expression. A filter expression must use the supported [CEL logic
   * operators] (https://cloud.google.com/vpc/docs/about-flow-logs-
   * records#supported_cel_logic_operators).
   * @opt_param string orderBy Optional. Field to use to sort the list.
   * @opt_param int pageSize Optional. Number of `VpcFlowLogsConfigs` to return.
   * @opt_param string pageToken Optional. Page token from an earlier query, as
   * returned in `next_page_token`.
   * @return ListVpcFlowLogsConfigsResponse
   * @throws \Google\Service\Exception
   */
  public function listProjectsLocationsVpcFlowLogsConfigs($parent, $optParams = [])
  {
    $params = ['parent' => $parent];
    $params = array_merge($params, $optParams);
    return $this->call('list', [$params], ListVpcFlowLogsConfigsResponse::class);
  }
  /**
   * Updates an existing `VpcFlowLogsConfig`. If a configuration with the exact
   * same settings already exists (even if the ID is different), the creation
   * fails. Notes: 1. Updating a configuration with state=DISABLED will fail. 2.
   * The following fields are not considered as `settings` for the purpose of the
   * check mentioned above, therefore - updating another configuration with the
   * same fields but different values for the following fields will fail as well:
   * * name * create_time * update_time * labels * description
   * (vpcFlowLogsConfigs.patch)
   *
   * @param string $name Identifier. Unique name of the configuration using the
   * form: `projects/{project_id}/locations/global/vpcFlowLogsConfigs/{vpc_flow_lo
   * gs_config_id}`
   * @param VpcFlowLogsConfig $postBody
   * @param array $optParams Optional parameters.
   *
   * @opt_param string updateMask Required. Mask of fields to update. At least one
   * path must be supplied in this field.
   * @return Operation
   * @throws \Google\Service\Exception
   */
  public function patch($name, VpcFlowLogsConfig $postBody, $optParams = [])
  {
    $params = ['name' => $name, 'postBody' => $postBody];
    $params = array_merge($params, $optParams);
    return $this->call('patch', [$params], Operation::class);
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProjectsLocationsVpcFlowLogsConfigs::class, 'Google_Service_NetworkManagement_Resource_ProjectsLocationsVpcFlowLogsConfigs');
