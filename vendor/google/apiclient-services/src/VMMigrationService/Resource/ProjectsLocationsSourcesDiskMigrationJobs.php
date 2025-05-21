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

namespace Google\Service\VMMigrationService\Resource;

use Google\Service\VMMigrationService\CancelDiskMigrationJobRequest;
use Google\Service\VMMigrationService\Operation;
use Google\Service\VMMigrationService\RunDiskMigrationJobRequest;

/**
 * The "diskMigrationJobs" collection of methods.
 * Typical usage is:
 *  <code>
 *   $vmmigrationService = new Google\Service\VMMigrationService(...);
 *   $diskMigrationJobs = $vmmigrationService->projects_locations_sources_diskMigrationJobs;
 *  </code>
 */
class ProjectsLocationsSourcesDiskMigrationJobs extends \Google\Service\Resource
{
  /**
   * Cancels the disk migration job. (diskMigrationJobs.cancel)
   *
   * @param string $name Required. The name of the DiskMigrationJob.
   * @param CancelDiskMigrationJobRequest $postBody
   * @param array $optParams Optional parameters.
   * @return Operation
   * @throws \Google\Service\Exception
   */
  public function cancel($name, CancelDiskMigrationJobRequest $postBody, $optParams = [])
  {
    $params = ['name' => $name, 'postBody' => $postBody];
    $params = array_merge($params, $optParams);
    return $this->call('cancel', [$params], Operation::class);
  }
  /**
   * Runs the disk migration job. (diskMigrationJobs.run)
   *
   * @param string $name Required. The name of the DiskMigrationJob.
   * @param RunDiskMigrationJobRequest $postBody
   * @param array $optParams Optional parameters.
   * @return Operation
   * @throws \Google\Service\Exception
   */
  public function run($name, RunDiskMigrationJobRequest $postBody, $optParams = [])
  {
    $params = ['name' => $name, 'postBody' => $postBody];
    $params = array_merge($params, $optParams);
    return $this->call('run', [$params], Operation::class);
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProjectsLocationsSourcesDiskMigrationJobs::class, 'Google_Service_VMMigrationService_Resource_ProjectsLocationsSourcesDiskMigrationJobs');
