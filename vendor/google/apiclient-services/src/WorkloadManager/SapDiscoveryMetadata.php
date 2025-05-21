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

class SapDiscoveryMetadata extends \Google\Model
{
  /**
   * @var string
   */
  public $customerRegion;
  /**
   * @var string
   */
  public $definedSystem;
  /**
   * @var string
   */
  public $environmentType;
  /**
   * @var string
   */
  public $sapProduct;

  /**
   * @param string
   */
  public function setCustomerRegion($customerRegion)
  {
    $this->customerRegion = $customerRegion;
  }
  /**
   * @return string
   */
  public function getCustomerRegion()
  {
    return $this->customerRegion;
  }
  /**
   * @param string
   */
  public function setDefinedSystem($definedSystem)
  {
    $this->definedSystem = $definedSystem;
  }
  /**
   * @return string
   */
  public function getDefinedSystem()
  {
    return $this->definedSystem;
  }
  /**
   * @param string
   */
  public function setEnvironmentType($environmentType)
  {
    $this->environmentType = $environmentType;
  }
  /**
   * @return string
   */
  public function getEnvironmentType()
  {
    return $this->environmentType;
  }
  /**
   * @param string
   */
  public function setSapProduct($sapProduct)
  {
    $this->sapProduct = $sapProduct;
  }
  /**
   * @return string
   */
  public function getSapProduct()
  {
    return $this->sapProduct;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SapDiscoveryMetadata::class, 'Google_Service_WorkloadManager_SapDiscoveryMetadata');
