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

class InternalRange extends \Google\Collection
{
  protected $collection_key = 'users';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $ipCidrRange;
  /**
   * @var string[]
   */
  public $labels;
  protected $migrationType = Migration::class;
  protected $migrationDataType = '';
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $network;
  /**
   * @var string[]
   */
  public $overlaps;
  /**
   * @var string
   */
  public $peering;
  /**
   * @var int
   */
  public $prefixLength;
  /**
   * @var string[]
   */
  public $targetCidrRange;
  /**
   * @var string
   */
  public $updateTime;
  /**
   * @var string
   */
  public $usage;
  /**
   * @var string[]
   */
  public $users;

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
  public function setIpCidrRange($ipCidrRange)
  {
    $this->ipCidrRange = $ipCidrRange;
  }
  /**
   * @return string
   */
  public function getIpCidrRange()
  {
    return $this->ipCidrRange;
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
   * @param Migration
   */
  public function setMigration(Migration $migration)
  {
    $this->migration = $migration;
  }
  /**
   * @return Migration
   */
  public function getMigration()
  {
    return $this->migration;
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
  public function setNetwork($network)
  {
    $this->network = $network;
  }
  /**
   * @return string
   */
  public function getNetwork()
  {
    return $this->network;
  }
  /**
   * @param string[]
   */
  public function setOverlaps($overlaps)
  {
    $this->overlaps = $overlaps;
  }
  /**
   * @return string[]
   */
  public function getOverlaps()
  {
    return $this->overlaps;
  }
  /**
   * @param string
   */
  public function setPeering($peering)
  {
    $this->peering = $peering;
  }
  /**
   * @return string
   */
  public function getPeering()
  {
    return $this->peering;
  }
  /**
   * @param int
   */
  public function setPrefixLength($prefixLength)
  {
    $this->prefixLength = $prefixLength;
  }
  /**
   * @return int
   */
  public function getPrefixLength()
  {
    return $this->prefixLength;
  }
  /**
   * @param string[]
   */
  public function setTargetCidrRange($targetCidrRange)
  {
    $this->targetCidrRange = $targetCidrRange;
  }
  /**
   * @return string[]
   */
  public function getTargetCidrRange()
  {
    return $this->targetCidrRange;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
  /**
   * @param string
   */
  public function setUsage($usage)
  {
    $this->usage = $usage;
  }
  /**
   * @return string
   */
  public function getUsage()
  {
    return $this->usage;
  }
  /**
   * @param string[]
   */
  public function setUsers($users)
  {
    $this->users = $users;
  }
  /**
   * @return string[]
   */
  public function getUsers()
  {
    return $this->users;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(InternalRange::class, 'Google_Service_Networkconnectivity_InternalRange');
