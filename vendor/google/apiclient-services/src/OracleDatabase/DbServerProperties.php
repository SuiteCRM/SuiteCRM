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

class DbServerProperties extends \Google\Collection
{
  protected $collection_key = 'dbNodeIds';
  /**
   * @var string[]
   */
  public $dbNodeIds;
  /**
   * @var int
   */
  public $dbNodeStorageSizeGb;
  /**
   * @var int
   */
  public $maxDbNodeStorageSizeGb;
  /**
   * @var int
   */
  public $maxMemorySizeGb;
  /**
   * @var int
   */
  public $maxOcpuCount;
  /**
   * @var int
   */
  public $memorySizeGb;
  /**
   * @var string
   */
  public $ocid;
  /**
   * @var int
   */
  public $ocpuCount;
  /**
   * @var string
   */
  public $state;
  /**
   * @var int
   */
  public $vmCount;

  /**
   * @param string[]
   */
  public function setDbNodeIds($dbNodeIds)
  {
    $this->dbNodeIds = $dbNodeIds;
  }
  /**
   * @return string[]
   */
  public function getDbNodeIds()
  {
    return $this->dbNodeIds;
  }
  /**
   * @param int
   */
  public function setDbNodeStorageSizeGb($dbNodeStorageSizeGb)
  {
    $this->dbNodeStorageSizeGb = $dbNodeStorageSizeGb;
  }
  /**
   * @return int
   */
  public function getDbNodeStorageSizeGb()
  {
    return $this->dbNodeStorageSizeGb;
  }
  /**
   * @param int
   */
  public function setMaxDbNodeStorageSizeGb($maxDbNodeStorageSizeGb)
  {
    $this->maxDbNodeStorageSizeGb = $maxDbNodeStorageSizeGb;
  }
  /**
   * @return int
   */
  public function getMaxDbNodeStorageSizeGb()
  {
    return $this->maxDbNodeStorageSizeGb;
  }
  /**
   * @param int
   */
  public function setMaxMemorySizeGb($maxMemorySizeGb)
  {
    $this->maxMemorySizeGb = $maxMemorySizeGb;
  }
  /**
   * @return int
   */
  public function getMaxMemorySizeGb()
  {
    return $this->maxMemorySizeGb;
  }
  /**
   * @param int
   */
  public function setMaxOcpuCount($maxOcpuCount)
  {
    $this->maxOcpuCount = $maxOcpuCount;
  }
  /**
   * @return int
   */
  public function getMaxOcpuCount()
  {
    return $this->maxOcpuCount;
  }
  /**
   * @param int
   */
  public function setMemorySizeGb($memorySizeGb)
  {
    $this->memorySizeGb = $memorySizeGb;
  }
  /**
   * @return int
   */
  public function getMemorySizeGb()
  {
    return $this->memorySizeGb;
  }
  /**
   * @param string
   */
  public function setOcid($ocid)
  {
    $this->ocid = $ocid;
  }
  /**
   * @return string
   */
  public function getOcid()
  {
    return $this->ocid;
  }
  /**
   * @param int
   */
  public function setOcpuCount($ocpuCount)
  {
    $this->ocpuCount = $ocpuCount;
  }
  /**
   * @return int
   */
  public function getOcpuCount()
  {
    return $this->ocpuCount;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param int
   */
  public function setVmCount($vmCount)
  {
    $this->vmCount = $vmCount;
  }
  /**
   * @return int
   */
  public function getVmCount()
  {
    return $this->vmCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DbServerProperties::class, 'Google_Service_OracleDatabase_DbServerProperties');
