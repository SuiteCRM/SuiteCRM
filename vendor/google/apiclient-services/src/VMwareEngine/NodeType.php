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

namespace Google\Service\VMwareEngine;

class NodeType extends \Google\Collection
{
  protected $collection_key = 'families';
  /**
   * @var int[]
   */
  public $availableCustomCoreCounts;
  /**
   * @var string[]
   */
  public $capabilities;
  /**
   * @var int
   */
  public $diskSizeGb;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string[]
   */
  public $families;
  /**
   * @var string
   */
  public $kind;
  /**
   * @var int
   */
  public $memoryGb;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $nodeTypeId;
  /**
   * @var int
   */
  public $totalCoreCount;
  /**
   * @var int
   */
  public $virtualCpuCount;

  /**
   * @param int[]
   */
  public function setAvailableCustomCoreCounts($availableCustomCoreCounts)
  {
    $this->availableCustomCoreCounts = $availableCustomCoreCounts;
  }
  /**
   * @return int[]
   */
  public function getAvailableCustomCoreCounts()
  {
    return $this->availableCustomCoreCounts;
  }
  /**
   * @param string[]
   */
  public function setCapabilities($capabilities)
  {
    $this->capabilities = $capabilities;
  }
  /**
   * @return string[]
   */
  public function getCapabilities()
  {
    return $this->capabilities;
  }
  /**
   * @param int
   */
  public function setDiskSizeGb($diskSizeGb)
  {
    $this->diskSizeGb = $diskSizeGb;
  }
  /**
   * @return int
   */
  public function getDiskSizeGb()
  {
    return $this->diskSizeGb;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param string[]
   */
  public function setFamilies($families)
  {
    $this->families = $families;
  }
  /**
   * @return string[]
   */
  public function getFamilies()
  {
    return $this->families;
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
   * @param int
   */
  public function setMemoryGb($memoryGb)
  {
    $this->memoryGb = $memoryGb;
  }
  /**
   * @return int
   */
  public function getMemoryGb()
  {
    return $this->memoryGb;
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
  public function setNodeTypeId($nodeTypeId)
  {
    $this->nodeTypeId = $nodeTypeId;
  }
  /**
   * @return string
   */
  public function getNodeTypeId()
  {
    return $this->nodeTypeId;
  }
  /**
   * @param int
   */
  public function setTotalCoreCount($totalCoreCount)
  {
    $this->totalCoreCount = $totalCoreCount;
  }
  /**
   * @return int
   */
  public function getTotalCoreCount()
  {
    return $this->totalCoreCount;
  }
  /**
   * @param int
   */
  public function setVirtualCpuCount($virtualCpuCount)
  {
    $this->virtualCpuCount = $virtualCpuCount;
  }
  /**
   * @return int
   */
  public function getVirtualCpuCount()
  {
    return $this->virtualCpuCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(NodeType::class, 'Google_Service_VMwareEngine_NodeType');
