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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1FhirStoreSource extends \Google\Collection
{
  protected $collection_key = 'resourceTypes';
  /**
   * @var string
   */
  public $fhirStore;
  /**
   * @var string
   */
  public $gcsStagingDir;
  /**
   * @var string[]
   */
  public $resourceTypes;
  /**
   * @var bool
   */
  public $updateFromLatestPredefinedSchema;

  /**
   * @param string
   */
  public function setFhirStore($fhirStore)
  {
    $this->fhirStore = $fhirStore;
  }
  /**
   * @return string
   */
  public function getFhirStore()
  {
    return $this->fhirStore;
  }
  /**
   * @param string
   */
  public function setGcsStagingDir($gcsStagingDir)
  {
    $this->gcsStagingDir = $gcsStagingDir;
  }
  /**
   * @return string
   */
  public function getGcsStagingDir()
  {
    return $this->gcsStagingDir;
  }
  /**
   * @param string[]
   */
  public function setResourceTypes($resourceTypes)
  {
    $this->resourceTypes = $resourceTypes;
  }
  /**
   * @return string[]
   */
  public function getResourceTypes()
  {
    return $this->resourceTypes;
  }
  /**
   * @param bool
   */
  public function setUpdateFromLatestPredefinedSchema($updateFromLatestPredefinedSchema)
  {
    $this->updateFromLatestPredefinedSchema = $updateFromLatestPredefinedSchema;
  }
  /**
   * @return bool
   */
  public function getUpdateFromLatestPredefinedSchema()
  {
    return $this->updateFromLatestPredefinedSchema;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1FhirStoreSource::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1FhirStoreSource');
