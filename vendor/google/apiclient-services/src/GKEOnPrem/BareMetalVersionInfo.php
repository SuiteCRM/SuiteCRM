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

namespace Google\Service\GKEOnPrem;

class BareMetalVersionInfo extends \Google\Collection
{
  protected $collection_key = 'dependencies';
  protected $dependenciesType = UpgradeDependency::class;
  protected $dependenciesDataType = 'array';
  /**
   * @var bool
   */
  public $hasDependencies;
  /**
   * @var string
   */
  public $version;

  /**
   * @param UpgradeDependency[]
   */
  public function setDependencies($dependencies)
  {
    $this->dependencies = $dependencies;
  }
  /**
   * @return UpgradeDependency[]
   */
  public function getDependencies()
  {
    return $this->dependencies;
  }
  /**
   * @param bool
   */
  public function setHasDependencies($hasDependencies)
  {
    $this->hasDependencies = $hasDependencies;
  }
  /**
   * @return bool
   */
  public function getHasDependencies()
  {
    return $this->hasDependencies;
  }
  /**
   * @param string
   */
  public function setVersion($version)
  {
    $this->version = $version;
  }
  /**
   * @return string
   */
  public function getVersion()
  {
    return $this->version;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BareMetalVersionInfo::class, 'Google_Service_GKEOnPrem_BareMetalVersionInfo');
