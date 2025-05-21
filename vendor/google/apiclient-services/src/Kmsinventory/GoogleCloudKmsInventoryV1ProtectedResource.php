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

class GoogleCloudKmsInventoryV1ProtectedResource extends \Google\Collection
{
  protected $collection_key = 'cryptoKeyVersions';
  /**
   * @var string
   */
  public $cloudProduct;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $cryptoKeyVersion;
  /**
   * @var string[]
   */
  public $cryptoKeyVersions;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $location;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $project;
  /**
   * @var string
   */
  public $projectId;
  /**
   * @var string
   */
  public $resourceType;

  /**
   * @param string
   */
  public function setCloudProduct($cloudProduct)
  {
    $this->cloudProduct = $cloudProduct;
  }
  /**
   * @return string
   */
  public function getCloudProduct()
  {
    return $this->cloudProduct;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setCryptoKeyVersion($cryptoKeyVersion)
  {
    $this->cryptoKeyVersion = $cryptoKeyVersion;
  }
  /**
   * @return string
   */
  public function getCryptoKeyVersion()
  {
    return $this->cryptoKeyVersion;
  }
  /**
   * @param string[]
   */
  public function setCryptoKeyVersions($cryptoKeyVersions)
  {
    $this->cryptoKeyVersions = $cryptoKeyVersions;
  }
  /**
   * @return string[]
   */
  public function getCryptoKeyVersions()
  {
    return $this->cryptoKeyVersions;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
  }
  /**
   * @param string
   */
  public function setLocation($location)
  {
    $this->location = $location;
  }
  /**
   * @return string
   */
  public function getLocation()
  {
    return $this->location;
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
   * @param string
   */
  public function setProject($project)
  {
    $this->project = $project;
  }
  /**
   * @return string
   */
  public function getProject()
  {
    return $this->project;
  }
  /**
   * @param string
   */
  public function setProjectId($projectId)
  {
    $this->projectId = $projectId;
  }
  /**
   * @return string
   */
  public function getProjectId()
  {
    return $this->projectId;
  }
  /**
   * @param string
   */
  public function setResourceType($resourceType)
  {
    $this->resourceType = $resourceType;
  }
  /**
   * @return string
   */
  public function getResourceType()
  {
    return $this->resourceType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudKmsInventoryV1ProtectedResource::class, 'Google_Service_Kmsinventory_GoogleCloudKmsInventoryV1ProtectedResource');
