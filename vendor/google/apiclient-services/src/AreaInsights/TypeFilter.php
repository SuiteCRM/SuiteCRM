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

namespace Google\Service\AreaInsights;

class TypeFilter extends \Google\Collection
{
  protected $collection_key = 'includedTypes';
  /**
   * @var string[]
   */
  public $excludedPrimaryTypes;
  /**
   * @var string[]
   */
  public $excludedTypes;
  /**
   * @var string[]
   */
  public $includedPrimaryTypes;
  /**
   * @var string[]
   */
  public $includedTypes;

  /**
   * @param string[]
   */
  public function setExcludedPrimaryTypes($excludedPrimaryTypes)
  {
    $this->excludedPrimaryTypes = $excludedPrimaryTypes;
  }
  /**
   * @return string[]
   */
  public function getExcludedPrimaryTypes()
  {
    return $this->excludedPrimaryTypes;
  }
  /**
   * @param string[]
   */
  public function setExcludedTypes($excludedTypes)
  {
    $this->excludedTypes = $excludedTypes;
  }
  /**
   * @return string[]
   */
  public function getExcludedTypes()
  {
    return $this->excludedTypes;
  }
  /**
   * @param string[]
   */
  public function setIncludedPrimaryTypes($includedPrimaryTypes)
  {
    $this->includedPrimaryTypes = $includedPrimaryTypes;
  }
  /**
   * @return string[]
   */
  public function getIncludedPrimaryTypes()
  {
    return $this->includedPrimaryTypes;
  }
  /**
   * @param string[]
   */
  public function setIncludedTypes($includedTypes)
  {
    $this->includedTypes = $includedTypes;
  }
  /**
   * @return string[]
   */
  public function getIncludedTypes()
  {
    return $this->includedTypes;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TypeFilter::class, 'Google_Service_AreaInsights_TypeFilter');
