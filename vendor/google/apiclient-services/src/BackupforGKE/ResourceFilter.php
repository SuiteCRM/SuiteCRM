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

namespace Google\Service\BackupforGKE;

class ResourceFilter extends \Google\Collection
{
  protected $collection_key = 'namespaces';
  protected $groupKindsType = GroupKind::class;
  protected $groupKindsDataType = 'array';
  /**
   * @var string
   */
  public $jsonPath;
  /**
   * @var string[]
   */
  public $namespaces;

  /**
   * @param GroupKind[]
   */
  public function setGroupKinds($groupKinds)
  {
    $this->groupKinds = $groupKinds;
  }
  /**
   * @return GroupKind[]
   */
  public function getGroupKinds()
  {
    return $this->groupKinds;
  }
  /**
   * @param string
   */
  public function setJsonPath($jsonPath)
  {
    $this->jsonPath = $jsonPath;
  }
  /**
   * @return string
   */
  public function getJsonPath()
  {
    return $this->jsonPath;
  }
  /**
   * @param string[]
   */
  public function setNamespaces($namespaces)
  {
    $this->namespaces = $namespaces;
  }
  /**
   * @return string[]
   */
  public function getNamespaces()
  {
    return $this->namespaces;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ResourceFilter::class, 'Google_Service_BackupforGKE_ResourceFilter');
