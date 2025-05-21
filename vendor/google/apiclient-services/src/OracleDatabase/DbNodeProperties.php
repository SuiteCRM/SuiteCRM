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

class DbNodeProperties extends \Google\Model
{
  /**
   * @var int
   */
  public $dbNodeStorageSizeGb;
  /**
   * @var string
   */
  public $dbServerOcid;
  /**
   * @var string
   */
  public $hostname;
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
  public $totalCpuCoreCount;

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
   * @param string
   */
  public function setDbServerOcid($dbServerOcid)
  {
    $this->dbServerOcid = $dbServerOcid;
  }
  /**
   * @return string
   */
  public function getDbServerOcid()
  {
    return $this->dbServerOcid;
  }
  /**
   * @param string
   */
  public function setHostname($hostname)
  {
    $this->hostname = $hostname;
  }
  /**
   * @return string
   */
  public function getHostname()
  {
    return $this->hostname;
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
  public function setTotalCpuCoreCount($totalCpuCoreCount)
  {
    $this->totalCpuCoreCount = $totalCpuCoreCount;
  }
  /**
   * @return int
   */
  public function getTotalCpuCoreCount()
  {
    return $this->totalCpuCoreCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DbNodeProperties::class, 'Google_Service_OracleDatabase_DbNodeProperties');
