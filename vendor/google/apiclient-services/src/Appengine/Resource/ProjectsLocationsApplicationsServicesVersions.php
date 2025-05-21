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

namespace Google\Service\Appengine\Resource;

use Google\Service\Appengine\Operation;

/**
 * The "versions" collection of methods.
 * Typical usage is:
 *  <code>
 *   $appengineService = new Google\Service\Appengine(...);
 *   $versions = $appengineService->projects_locations_applications_services_versions;
 *  </code>
 */
class ProjectsLocationsApplicationsServicesVersions extends \Google\Service\Resource
{
  /**
   * Deletes an existing Version resource. (versions.delete)
   *
   * @param string $projectsId Part of `name`. Name of the resource requested.
   * Example: apps/myapp/services/default/versions/v1.
   * @param string $locationsId Part of `name`. See documentation of `projectsId`.
   * @param string $applicationsId Part of `name`. See documentation of
   * `projectsId`.
   * @param string $servicesId Part of `name`. See documentation of `projectsId`.
   * @param string $versionsId Part of `name`. See documentation of `projectsId`.
   * @param array $optParams Optional parameters.
   * @return Operation
   * @throws \Google\Service\Exception
   */
  public function delete($projectsId, $locationsId, $applicationsId, $servicesId, $versionsId, $optParams = [])
  {
    $params = ['projectsId' => $projectsId, 'locationsId' => $locationsId, 'applicationsId' => $applicationsId, 'servicesId' => $servicesId, 'versionsId' => $versionsId];
    $params = array_merge($params, $optParams);
    return $this->call('delete', [$params], Operation::class);
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProjectsLocationsApplicationsServicesVersions::class, 'Google_Service_Appengine_Resource_ProjectsLocationsApplicationsServicesVersions');
