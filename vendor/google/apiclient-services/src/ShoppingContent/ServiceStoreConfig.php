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

namespace Google\Service\ShoppingContent;

class ServiceStoreConfig extends \Google\Collection
{
  protected $collection_key = 'storeCodes';
  protected $cutoffConfigType = ServiceStoreConfigCutoffConfig::class;
  protected $cutoffConfigDataType = '';
  protected $serviceRadiusType = Distance::class;
  protected $serviceRadiusDataType = '';
  /**
   * @var string[]
   */
  public $storeCodes;
  /**
   * @var string
   */
  public $storeServiceType;

  /**
   * @param ServiceStoreConfigCutoffConfig
   */
  public function setCutoffConfig(ServiceStoreConfigCutoffConfig $cutoffConfig)
  {
    $this->cutoffConfig = $cutoffConfig;
  }
  /**
   * @return ServiceStoreConfigCutoffConfig
   */
  public function getCutoffConfig()
  {
    return $this->cutoffConfig;
  }
  /**
   * @param Distance
   */
  public function setServiceRadius(Distance $serviceRadius)
  {
    $this->serviceRadius = $serviceRadius;
  }
  /**
   * @return Distance
   */
  public function getServiceRadius()
  {
    return $this->serviceRadius;
  }
  /**
   * @param string[]
   */
  public function setStoreCodes($storeCodes)
  {
    $this->storeCodes = $storeCodes;
  }
  /**
   * @return string[]
   */
  public function getStoreCodes()
  {
    return $this->storeCodes;
  }
  /**
   * @param string
   */
  public function setStoreServiceType($storeServiceType)
  {
    $this->storeServiceType = $storeServiceType;
  }
  /**
   * @return string
   */
  public function getStoreServiceType()
  {
    return $this->storeServiceType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ServiceStoreConfig::class, 'Google_Service_ShoppingContent_ServiceStoreConfig');
