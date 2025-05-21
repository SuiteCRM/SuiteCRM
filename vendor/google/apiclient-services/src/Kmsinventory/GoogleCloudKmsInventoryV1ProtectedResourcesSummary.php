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

namespace Google\Service\Kmsinventory;

class GoogleCloudKmsInventoryV1ProtectedResourcesSummary extends \Google\Model
{
  /**
   * @var string[]
   */
  public $cloudProducts;
  /**
   * @var string[]
   */
  public $locations;
  /**
   * @var string
   */
  public $name;
  /**
   * @var int
   */
  public $projectCount;
  /**
   * @var string
   */
  public $resourceCount;
  /**
   * @var string[]
   */
  public $resourceTypes;

  /**
   * @param string[]
   */
  public function setCloudProducts($cloudProducts)
  {
    $this->cloudProducts = $cloudProducts;
  }
  /**
   * @return string[]
   */
  public function getCloudProducts()
  {
    return $this->cloudProducts;
  }
  /**
   * @param string[]
   */
  public function setLocations($locations)
  {
    $this->locations = $locations;
  }
  /**
   * @return string[]
   */
  public function getLocations()
  {
    return $this->locations;
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
   * @param int
   */
  public function setProjectCount($projectCount)
  {
    $this->projectCount = $projectCount;
  }
  /**
   * @return int
   */
  public function getProjectCount()
  {
    return $this->projectCount;
  }
  /**
   * @param string
   */
  public function setResourceCount($resourceCount)
  {
    $this->resourceCount = $resourceCount;
  }
  /**
   * @return string
   */
  public function getResourceCount()
  {
    return $this->resourceCount;
  }
  /**
   * @param string[]
   */
  public function setResourceTypes($resourceTypes)
  {
    $this->resourceTypes = $resourceTypes;
  }
  /**
   * @return string[]
   */
  public function getResourceTypes()
  {
    return $this->resourceTypes;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudKmsInventoryV1ProtectedResourcesSummary::class, 'Google_Service_Kmsinventory_GoogleCloudKmsInventoryV1ProtectedResourcesSummary');
