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

namespace Google\Service\GoogleMarketingPlatformAdminAPI\Resource;

use Google\Service\GoogleMarketingPlatformAdminAPI\Organization;

/**
 * The "organizations" collection of methods.
 * Typical usage is:
 *  <code>
 *   $marketingplatformadminService = new Google\Service\GoogleMarketingPlatformAdminAPI(...);
 *   $organizations = $marketingplatformadminService->organizations;
 *  </code>
 */
class Organizations extends \Google\Service\Resource
{
  /**
   * Lookup for a single organization. (organizations.get)
   *
   * @param string $name Required. The name of the Organization to retrieve.
   * Format: organizations/{org_id}
   * @param array $optParams Optional parameters.
   * @return Organization
   * @throws \Google\Service\Exception
   */
  public function get($name, $optParams = [])
  {
    $params = ['name' => $name];
    $params = array_merge($params, $optParams);
    return $this->call('get', [$params], Organization::class);
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Organizations::class, 'Google_Service_GoogleMarketingPlatformAdminAPI_Resource_Organizations');
