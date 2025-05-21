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

namespace Google\Service\Dataform;

class InvocationConfig extends \Google\Collection
{
  protected $collection_key = 'includedTargets';
  /**
   * @var bool
   */
  public $fullyRefreshIncrementalTablesEnabled;
  /**
   * @var string[]
   */
  public $includedTags;
  protected $includedTargetsType = Target::class;
  protected $includedTargetsDataType = 'array';
  /**
   * @var string
   */
  public $serviceAccount;
  /**
   * @var bool
   */
  public $transitiveDependenciesIncluded;
  /**
   * @var bool
   */
  public $transitiveDependentsIncluded;

  /**
   * @param bool
   */
  public function setFullyRefreshIncrementalTablesEnabled($fullyRefreshIncrementalTablesEnabled)
  {
    $this->fullyRefreshIncrementalTablesEnabled = $fullyRefreshIncrementalTablesEnabled;
  }
  /**
   * @return bool
   */
  public function getFullyRefreshIncrementalTablesEnabled()
  {
    return $this->fullyRefreshIncrementalTablesEnabled;
  }
  /**
   * @param string[]
   */
  public function setIncludedTags($includedTags)
  {
    $this->includedTags = $includedTags;
  }
  /**
   * @return string[]
   */
  public function getIncludedTags()
  {
    return $this->includedTags;
  }
  /**
   * @param Target[]
   */
  public function setIncludedTargets($includedTargets)
  {
    $this->includedTargets = $includedTargets;
  }
  /**
   * @return Target[]
   */
  public function getIncludedTargets()
  {
    return $this->includedTargets;
  }
  /**
   * @param string
   */
  public function setServiceAccount($serviceAccount)
  {
    $this->serviceAccount = $serviceAccount;
  }
  /**
   * @return string
   */
  public function getServiceAccount()
  {
    return $this->serviceAccount;
  }
  /**
   * @param bool
   */
  public function setTransitiveDependenciesIncluded($transitiveDependenciesIncluded)
  {
    $this->transitiveDependenciesIncluded = $transitiveDependenciesIncluded;
  }
  /**
   * @return bool
   */
  public function getTransitiveDependenciesIncluded()
  {
    return $this->transitiveDependenciesIncluded;
  }
  /**
   * @param bool
   */
  public function setTransitiveDependentsIncluded($transitiveDependentsIncluded)
  {
    $this->transitiveDependentsIncluded = $transitiveDependentsIncluded;
  }
  /**
   * @return bool
   */
  public function getTransitiveDependentsIncluded()
  {
    return $this->transitiveDependentsIncluded;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(InvocationConfig::class, 'Google_Service_Dataform_InvocationConfig');
