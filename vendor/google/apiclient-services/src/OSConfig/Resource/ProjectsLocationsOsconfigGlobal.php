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

namespace Google\Service\OSConfig\Resource;

use Google\Service\OSConfig\ProjectFeatureSettings;

/**
 * The "global" collection of methods.
 * Typical usage is:
 *  <code>
 *   $osconfigService = new Google\Service\OSConfig(...);
 *   $global = $osconfigService->projects_locations_global;
 *  </code>
 */
class ProjectsLocationsOsconfigGlobal extends \Google\Service\Resource
{
  /**
   * GetProjectFeatureSettings returns the VM Manager feature settings for a
   * project. (global.getProjectFeatureSettings)
   *
   * @param string $name Required. Name specifies the URL for the
   * ProjectFeatureSettings resource:
   * projects/project_id/locations/global/projectFeatureSettings.
   * @param array $optParams Optional parameters.
   * @return ProjectFeatureSettings
   * @throws \Google\Service\Exception
   */
  public function getProjectFeatureSettings($name, $optParams = [])
  {
    $params = ['name' => $name];
    $params = array_merge($params, $optParams);
    return $this->call('getProjectFeatureSettings', [$params], ProjectFeatureSettings::class);
  }
  /**
   * UpdateProjectFeatureSettings sets the VM Manager features for a project.
   * (global.updateProjectFeatureSettings)
   *
   * @param string $name Required. Immutable. Name specifies the URL for the
   * ProjectFeatureSettings resource:
   * projects/project_id/locations/global/projectFeatureSettings.
   * @param ProjectFeatureSettings $postBody
   * @param array $optParams Optional parameters.
   *
   * @opt_param string updateMask Optional. Field mask that controls which fields
   * of the ProjectFeatureSettings should be updated.
   * @return ProjectFeatureSettings
   * @throws \Google\Service\Exception
   */
  public function updateProjectFeatureSettings($name, ProjectFeatureSettings $postBody, $optParams = [])
  {
    $params = ['name' => $name, 'postBody' => $postBody];
    $params = array_merge($params, $optParams);
    return $this->call('updateProjectFeatureSettings', [$params], ProjectFeatureSettings::class);
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProjectsLocationsOsconfigGlobal::class, 'Google_Service_OSConfig_Resource_ProjectsLocationsOsconfigGlobal');
