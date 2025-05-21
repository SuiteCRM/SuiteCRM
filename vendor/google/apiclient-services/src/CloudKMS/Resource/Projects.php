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

namespace Google\Service\CloudKMS\Resource;

use Google\Service\CloudKMS\ShowEffectiveAutokeyConfigResponse;

/**
 * The "projects" collection of methods.
 * Typical usage is:
 *  <code>
 *   $cloudkmsService = new Google\Service\CloudKMS(...);
 *   $projects = $cloudkmsService->projects;
 *  </code>
 */
class Projects extends \Google\Service\Resource
{
  /**
   * Returns the effective Cloud KMS Autokey configuration for a given project.
   * (projects.showEffectiveAutokeyConfig)
   *
   * @param string $parent Required. Name of the resource project to the show
   * effective Cloud KMS Autokey configuration for. This may be helpful for
   * interrogating the effect of nested folder configurations on a given resource
   * project.
   * @param array $optParams Optional parameters.
   * @return ShowEffectiveAutokeyConfigResponse
   * @throws \Google\Service\Exception
   */
  public function showEffectiveAutokeyConfig($parent, $optParams = [])
  {
    $params = ['parent' => $parent];
    $params = array_merge($params, $optParams);
    return $this->call('showEffectiveAutokeyConfig', [$params], ShowEffectiveAutokeyConfigResponse::class);
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Projects::class, 'Google_Service_CloudKMS_Resource_Projects');
