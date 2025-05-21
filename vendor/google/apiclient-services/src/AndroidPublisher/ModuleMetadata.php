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

namespace Google\Service\AndroidPublisher;

class ModuleMetadata extends \Google\Collection
{
  protected $collection_key = 'dependencies';
  /**
   * @var string
   */
  public $deliveryType;
  /**
   * @var string[]
   */
  public $dependencies;
  /**
   * @var string
   */
  public $moduleType;
  /**
   * @var string
   */
  public $name;
  protected $targetingType = ModuleTargeting::class;
  protected $targetingDataType = '';

  /**
   * @param string
   */
  public function setDeliveryType($deliveryType)
  {
    $this->deliveryType = $deliveryType;
  }
  /**
   * @return string
   */
  public function getDeliveryType()
  {
    return $this->deliveryType;
  }
  /**
   * @param string[]
   */
  public function setDependencies($dependencies)
  {
    $this->dependencies = $dependencies;
  }
  /**
   * @return string[]
   */
  public function getDependencies()
  {
    return $this->dependencies;
  }
  /**
   * @param string
   */
  public function setModuleType($moduleType)
  {
    $this->moduleType = $moduleType;
  }
  /**
   * @return string
   */
  public function getModuleType()
  {
    return $this->moduleType;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param ModuleTargeting
   */
  public function setTargeting(ModuleTargeting $targeting)
  {
    $this->targeting = $targeting;
  }
  /**
   * @return ModuleTargeting
   */
  public function getTargeting()
  {
    return $this->targeting;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ModuleMetadata::class, 'Google_Service_AndroidPublisher_ModuleMetadata');
