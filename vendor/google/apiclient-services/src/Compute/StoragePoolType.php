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

class StoragePoolType extends \Google\Collection
{
  protected $collection_key = 'supportedDiskTypes';
  /**
   * @var string
   */
  public $creationTimestamp;
  protected $deprecatedType = DeprecationStatus::class;
  protected $deprecatedDataType = '';
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $kind;
  /**
   * @var string
   */
  public $maxPoolProvisionedCapacityGb;
  /**
   * @var string
   */
  public $maxPoolProvisionedIops;
  /**
   * @var string
   */
  public $maxPoolProvisionedThroughput;
  /**
   * @var string
   */
  public $minPoolProvisionedCapacityGb;
  /**
   * @var string
   */
  public $minPoolProvisionedIops;
  /**
   * @var string
   */
  public $minPoolProvisionedThroughput;
  /**
   * @var string
   */
  public $minSizeGb;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $selfLink;
  /**
   * @var string
   */
  public $selfLinkWithId;
  /**
   * @var string[]
   */
  public $supportedDiskTypes;
  /**
   * @var string
   */
  public $zone;

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
   * @param DeprecationStatus
   */
  public function setDeprecated(DeprecationStatus $deprecated)
  {
    $this->deprecated = $deprecated;
  }
  /**
   * @return DeprecationStatus
   */
  public function getDeprecated()
  {
    return $this->deprecated;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
  }
  /**
   * @param string
   */
  public function setKind($kind)
  {
    $this->kind = $kind;
  }
  /**
   * @return string
   */
  public function getKind()
  {
    return $this->kind;
  }
  /**
   * @param string
   */
  public function setMaxPoolProvisionedCapacityGb($maxPoolProvisionedCapacityGb)
  {
    $this->maxPoolProvisionedCapacityGb = $maxPoolProvisionedCapacityGb;
  }
  /**
   * @return string
   */
  public function getMaxPoolProvisionedCapacityGb()
  {
    return $this->maxPoolProvisionedCapacityGb;
  }
  /**
   * @param string
   */
  public function setMaxPoolProvisionedIops($maxPoolProvisionedIops)
  {
    $this->maxPoolProvisionedIops = $maxPoolProvisionedIops;
  }
  /**
   * @return string
   */
  public function getMaxPoolProvisionedIops()
  {
    return $this->maxPoolProvisionedIops;
  }
  /**
   * @param string
   */
  public function setMaxPoolProvisionedThroughput($maxPoolProvisionedThroughput)
  {
    $this->maxPoolProvisionedThroughput = $maxPoolProvisionedThroughput;
  }
  /**
   * @return string
   */
  public function getMaxPoolProvisionedThroughput()
  {
    return $this->maxPoolProvisionedThroughput;
  }
  /**
   * @param string
   */
  public function setMinPoolProvisionedCapacityGb($minPoolProvisionedCapacityGb)
  {
    $this->minPoolProvisionedCapacityGb = $minPoolProvisionedCapacityGb;
  }
  /**
   * @return string
   */
  public function getMinPoolProvisionedCapacityGb()
  {
    return $this->minPoolProvisionedCapacityGb;
  }
  /**
   * @param string
   */
  public function setMinPoolProvisionedIops($minPoolProvisionedIops)
  {
    $this->minPoolProvisionedIops = $minPoolProvisionedIops;
  }
  /**
   * @return string
   */
  public function getMinPoolProvisionedIops()
  {
    return $this->minPoolProvisionedIops;
  }
  /**
   * @param string
   */
  public function setMinPoolProvisionedThroughput($minPoolProvisionedThroughput)
  {
    $this->minPoolProvisionedThroughput = $minPoolProvisionedThroughput;
  }
  /**
   * @return string
   */
  public function getMinPoolProvisionedThroughput()
  {
    return $this->minPoolProvisionedThroughput;
  }
  /**
   * @param string
   */
  public function setMinSizeGb($minSizeGb)
  {
    $this->minSizeGb = $minSizeGb;
  }
  /**
   * @return string
   */
  public function getMinSizeGb()
  {
    return $this->minSizeGb;
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
  public function setSelfLink($selfLink)
  {
    $this->selfLink = $selfLink;
  }
  /**
   * @return string
   */
  public function getSelfLink()
  {
    return $this->selfLink;
  }
  /**
   * @param string
   */
  public function setSelfLinkWithId($selfLinkWithId)
  {
    $this->selfLinkWithId = $selfLinkWithId;
  }
  /**
   * @return string
   */
  public function getSelfLinkWithId()
  {
    return $this->selfLinkWithId;
  }
  /**
   * @param string[]
   */
  public function setSupportedDiskTypes($supportedDiskTypes)
  {
    $this->supportedDiskTypes = $supportedDiskTypes;
  }
  /**
   * @return string[]
   */
  public function getSupportedDiskTypes()
  {
    return $this->supportedDiskTypes;
  }
  /**
   * @param string
   */
  public function setZone($zone)
  {
    $this->zone = $zone;
  }
  /**
   * @return string
   */
  public function getZone()
  {
    return $this->zone;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(StoragePoolType::class, 'Google_Service_Compute_StoragePoolType');
