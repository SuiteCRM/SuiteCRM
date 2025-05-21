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

class CleanupPolicyCondition extends \Google\Collection
{
  protected $collection_key = 'versionNamePrefixes';
  /**
   * @var string
   */
  public $newerThan;
  /**
   * @var string
   */
  public $olderThan;
  /**
   * @var string[]
   */
  public $packageNamePrefixes;
  /**
   * @var string[]
   */
  public $tagPrefixes;
  /**
   * @var string
   */
  public $tagState;
  /**
   * @var string[]
   */
  public $versionNamePrefixes;

  /**
   * @param string
   */
  public function setNewerThan($newerThan)
  {
    $this->newerThan = $newerThan;
  }
  /**
   * @return string
   */
  public function getNewerThan()
  {
    return $this->newerThan;
  }
  /**
   * @param string
   */
  public function setOlderThan($olderThan)
  {
    $this->olderThan = $olderThan;
  }
  /**
   * @return string
   */
  public function getOlderThan()
  {
    return $this->olderThan;
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
  /**
   * @param string[]
   */
  public function setTagPrefixes($tagPrefixes)
  {
    $this->tagPrefixes = $tagPrefixes;
  }
  /**
   * @return string[]
   */
  public function getTagPrefixes()
  {
    return $this->tagPrefixes;
  }
  /**
   * @param string
   */
  public function setTagState($tagState)
  {
    $this->tagState = $tagState;
  }
  /**
   * @return string
   */
  public function getTagState()
  {
    return $this->tagState;
  }
  /**
   * @param string[]
   */
  public function setVersionNamePrefixes($versionNamePrefixes)
  {
    $this->versionNamePrefixes = $versionNamePrefixes;
  }
  /**
   * @return string[]
   */
  public function getVersionNamePrefixes()
  {
    return $this->versionNamePrefixes;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CleanupPolicyCondition::class, 'Google_Service_ArtifactRegistry_CleanupPolicyCondition');
