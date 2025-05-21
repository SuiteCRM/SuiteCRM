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

namespace Google\Service\Compute;

class StoragePoolDisk extends \Google\Collection
{
  protected $collection_key = 'resourcePolicies';
  /**
   * @var string[]
   */
  public $attachedInstances;
  /**
   * @var string
   */
  public $creationTimestamp;
  /**
   * @var string
   */
  public $disk;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $provisionedIops;
  /**
   * @var string
   */
  public $provisionedThroughput;
  /**
   * @var string[]
   */
  public $resourcePolicies;
  /**
   * @var string
   */
  public $sizeGb;
  /**
   * @var string
   */
  public $status;
  /**
   * @var string
   */
  public $type;
  /**
   * @var string
   */
  public $usedBytes;

  /**
   * @param string[]
   */
  public function setAttachedInstances($attachedInstances)
  {
    $this->attachedInstances = $attachedInstances;
  }
  /**
   * @return string[]
   */
  public function getAttachedInstances()
  {
    return $this->attachedInstances;
  }
  /**
   * @param string
   */
  public function setCreationTimestamp($creationTimestamp)
  {
    $this->creationTimestamp = $creationTimestamp;
  }
  /**
   * @return string
   */
  public function getCreationTimestamp()
  {
    return $this->creationTimestamp;
  }
  /**
   * @param string
   */
  public function setDisk($disk)
  {
    $this->disk = $disk;
  }
  /**
   * @return string
   */
  public function getDisk()
  {
    return $this->disk;
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
  public function setProvisionedIops($provisionedIops)
  {
    $this->provisionedIops = $provisionedIops;
  }
  /**
   * @return string
   */
  public function getProvisionedIops()
  {
    return $this->provisionedIops;
  }
  /**
   * @param string
   */
  public function setProvisionedThroughput($provisionedThroughput)
  {
    $this->provisionedThroughput = $provisionedThroughput;
  }
  /**
   * @return string
   */
  public function getProvisionedThroughput()
  {
    return $this->provisionedThroughput;
  }
  /**
   * @param string[]
   */
  public function setResourcePolicies($resourcePolicies)
  {
    $this->resourcePolicies = $resourcePolicies;
  }
  /**
   * @return string[]
   */
  public function getResourcePolicies()
  {
    return $this->resourcePolicies;
  }
  /**
   * @param string
   */
  public function setSizeGb($sizeGb)
  {
    $this->sizeGb = $sizeGb;
  }
  /**
   * @return string
   */
  public function getSizeGb()
  {
    return $this->sizeGb;
  }
  /**
   * @param string
   */
  public function setStatus($status)
  {
    $this->status = $status;
  }
  /**
   * @return string
   */
  public function getStatus()
  {
    return $this->status;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
  /**
   * @param string
   */
  public function setUsedBytes($usedBytes)
  {
    $this->usedBytes = $usedBytes;
  }
  /**
   * @return string
   */
  public function getUsedBytes()
  {
    return $this->usedBytes;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(StoragePoolDisk::class, 'Google_Service_Compute_StoragePoolDisk');
