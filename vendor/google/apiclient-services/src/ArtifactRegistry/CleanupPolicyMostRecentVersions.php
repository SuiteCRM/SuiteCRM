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

namespace Google\Service\ArtifactRegistry;

class CleanupPolicyMostRecentVersions extends \Google\Collection
{
  protected $collection_key = 'packageNamePrefixes';
  /**
   * @var int
   */
  public $keepCount;
  /**
   * @var string[]
   */
  public $packageNamePrefixes;

  /**
   * @param int
   */
  public function setKeepCount($keepCount)
  {
    $this->keepCount = $keepCount;
  }
  /**
   * @return int
   */
  public function getKeepCount()
  {
    return $this->keepCount;
  }
  /**
   * @param string[]
   */
  public function setPackageNamePrefixes($packageNamePrefixes)
  {
    $this->packageNamePrefixes = $packageNamePrefixes;
  }
  /**
   * @return string[]
   */
  public function getPackageNamePrefixes()
  {
    return $this->packageNamePrefixes;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CleanupPolicyMostRecentVersions::class, 'Google_Service_ArtifactRegistry_CleanupPolicyMostRecentVersions');
