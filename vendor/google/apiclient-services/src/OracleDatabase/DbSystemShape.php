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

namespace Google\Service\OracleDatabase;

class DbSystemShape extends \Google\Model
{
  /**
   * @var int
   */
  public $availableCoreCountPerNode;
  /**
   * @var int
   */
  public $availableDataStorageTb;
  /**
   * @var int
   */
  public $availableMemoryPerNodeGb;
  /**
   * @var int
   */
  public $maxNodeCount;
  /**
   * @var int
   */
  public $maxStorageCount;
  /**
   * @var int
   */
  public $minCoreCountPerNode;
  /**
   * @var int
   */
  public $minDbNodeStoragePerNodeGb;
  /**
   * @var int
   */
  public $minMemoryPerNodeGb;
  /**
   * @var int
   */
  public $minNodeCount;
  /**
   * @var int
   */
  public $minStorageCount;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $shape;

  /**
   * @param int
   */
  public function setAvailableCoreCountPerNode($availableCoreCountPerNode)
  {
    $this->availableCoreCountPerNode = $availableCoreCountPerNode;
  }
  /**
   * @return int
   */
  public function getAvailableCoreCountPerNode()
  {
    return $this->availableCoreCountPerNode;
  }
  /**
   * @param int
   */
  public function setAvailableDataStorageTb($availableDataStorageTb)
  {
    $this->availableDataStorageTb = $availableDataStorageTb;
  }
  /**
   * @return int
   */
  public function getAvailableDataStorageTb()
  {
    return $this->availableDataStorageTb;
  }
  /**
   * @param int
   */
  public function setAvailableMemoryPerNodeGb($availableMemoryPerNodeGb)
  {
    $this->availableMemoryPerNodeGb = $availableMemoryPerNodeGb;
  }
  /**
   * @return int
   */
  public function getAvailableMemoryPerNodeGb()
  {
    return $this->availableMemoryPerNodeGb;
  }
  /**
   * @param int
   */
  public function setMaxNodeCount($maxNodeCount)
  {
    $this->maxNodeCount = $maxNodeCount;
  }
  /**
   * @return int
   */
  public function getMaxNodeCount()
  {
    return $this->maxNodeCount;
  }
  /**
   * @param int
   */
  public function setMaxStorageCount($maxStorageCount)
  {
    $this->maxStorageCount = $maxStorageCount;
  }
  /**
   * @return int
   */
  public function getMaxStorageCount()
  {
    return $this->maxStorageCount;
  }
  /**
   * @param int
   */
  public function setMinCoreCountPerNode($minCoreCountPerNode)
  {
    $this->minCoreCountPerNode = $minCoreCountPerNode;
  }
  /**
   * @return int
   */
  public function getMinCoreCountPerNode()
  {
    return $this->minCoreCountPerNode;
  }
  /**
   * @param int
   */
  public function setMinDbNodeStoragePerNodeGb($minDbNodeStoragePerNodeGb)
  {
    $this->minDbNodeStoragePerNodeGb = $minDbNodeStoragePerNodeGb;
  }
  /**
   * @return int
   */
  public function getMinDbNodeStoragePerNodeGb()
  {
    return $this->minDbNodeStoragePerNodeGb;
  }
  /**
   * @param int
   */
  public function setMinMemoryPerNodeGb($minMemoryPerNodeGb)
  {
    $this->minMemoryPerNodeGb = $minMemoryPerNodeGb;
  }
  /**
   * @return int
   */
  public function getMinMemoryPerNodeGb()
  {
    return $this->minMemoryPerNodeGb;
  }
  /**
   * @param int
   */
  public function setMinNodeCount($minNodeCount)
  {
    $this->minNodeCount = $minNodeCount;
  }
  /**
   * @return int
   */
  public function getMinNodeCount()
  {
    return $this->minNodeCount;
  }
  /**
   * @param int
   */
  public function setMinStorageCount($minStorageCount)
  {
    $this->minStorageCount = $minStorageCount;
  }
  /**
   * @return int
   */
  public function getMinStorageCount()
  {
    return $this->minStorageCount;
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
  public function setShape($shape)
  {
    $this->shape = $shape;
  }
  /**
   * @return string
   */
  public function getShape()
  {
    return $this->shape;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DbSystemShape::class, 'Google_Service_OracleDatabase_DbSystemShape');
