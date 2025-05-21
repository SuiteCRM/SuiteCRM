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

namespace Google\Service\Networkconnectivity;

class PscConfig extends \Google\Collection
{
  protected $collection_key = 'subnetworks';
  /**
   * @var string[]
   */
  public $allowedGoogleProducersResourceHierarchyLevel;
  /**
   * @var string
   */
  public $limit;
  /**
   * @var string
   */
  public $producerInstanceLocation;
  /**
   * @var string[]
   */
  public $subnetworks;

  /**
   * @param string[]
   */
  public function setAllowedGoogleProducersResourceHierarchyLevel($allowedGoogleProducersResourceHierarchyLevel)
  {
    $this->allowedGoogleProducersResourceHierarchyLevel = $allowedGoogleProducersResourceHierarchyLevel;
  }
  /**
   * @return string[]
   */
  public function getAllowedGoogleProducersResourceHierarchyLevel()
  {
    return $this->allowedGoogleProducersResourceHierarchyLevel;
  }
  /**
   * @param string
   */
  public function setLimit($limit)
  {
    $this->limit = $limit;
  }
  /**
   * @return string
   */
  public function getLimit()
  {
    return $this->limit;
  }
  /**
   * @param string
   */
  public function setProducerInstanceLocation($producerInstanceLocation)
  {
    $this->producerInstanceLocation = $producerInstanceLocation;
  }
  /**
   * @return string
   */
  public function getProducerInstanceLocation()
  {
    return $this->producerInstanceLocation;
  }
  /**
   * @param string[]
   */
  public function setSubnetworks($subnetworks)
  {
    $this->subnetworks = $subnetworks;
  }
  /**
   * @return string[]
   */
  public function getSubnetworks()
  {
    return $this->subnetworks;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PscConfig::class, 'Google_Service_Networkconnectivity_PscConfig');
