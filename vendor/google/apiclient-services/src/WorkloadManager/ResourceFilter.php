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

namespace Google\Service\WorkloadManager;

class ResourceFilter extends \Google\Collection
{
  protected $collection_key = 'scopes';
  protected $gceInstanceFilterType = GceInstanceFilter::class;
  protected $gceInstanceFilterDataType = '';
  /**
   * @var string[]
   */
  public $inclusionLabels;
  /**
   * @var string[]
   */
  public $resourceIdPatterns;
  /**
   * @var string[]
   */
  public $scopes;

  /**
   * @param GceInstanceFilter
   */
  public function setGceInstanceFilter(GceInstanceFilter $gceInstanceFilter)
  {
    $this->gceInstanceFilter = $gceInstanceFilter;
  }
  /**
   * @return GceInstanceFilter
   */
  public function getGceInstanceFilter()
  {
    return $this->gceInstanceFilter;
  }
  /**
   * @param string[]
   */
  public function setInclusionLabels($inclusionLabels)
  {
    $this->inclusionLabels = $inclusionLabels;
  }
  /**
   * @return string[]
   */
  public function getInclusionLabels()
  {
    return $this->inclusionLabels;
  }
  /**
   * @param string[]
   */
  public function setResourceIdPatterns($resourceIdPatterns)
  {
    $this->resourceIdPatterns = $resourceIdPatterns;
  }
  /**
   * @return string[]
   */
  public function getResourceIdPatterns()
  {
    return $this->resourceIdPatterns;
  }
  /**
   * @param string[]
   */
  public function setScopes($scopes)
  {
    $this->scopes = $scopes;
  }
  /**
   * @return string[]
   */
  public function getScopes()
  {
    return $this->scopes;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ResourceFilter::class, 'Google_Service_WorkloadManager_ResourceFilter');
