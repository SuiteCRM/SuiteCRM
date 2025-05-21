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

class Filter extends \Google\Collection
{
  protected $collection_key = 'inclusionFilters';
  protected $exclusionFiltersType = ResourceSelector::class;
  protected $exclusionFiltersDataType = 'array';
  protected $inclusionFiltersType = ResourceSelector::class;
  protected $inclusionFiltersDataType = 'array';

  /**
   * @param ResourceSelector[]
   */
  public function setExclusionFilters($exclusionFilters)
  {
    $this->exclusionFilters = $exclusionFilters;
  }
  /**
   * @return ResourceSelector[]
   */
  public function getExclusionFilters()
  {
    return $this->exclusionFilters;
  }
  /**
   * @param ResourceSelector[]
   */
  public function setInclusionFilters($inclusionFilters)
  {
    $this->inclusionFilters = $inclusionFilters;
  }
  /**
   * @return ResourceSelector[]
   */
  public function getInclusionFilters()
  {
    return $this->inclusionFilters;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Filter::class, 'Google_Service_BackupforGKE_Filter');
