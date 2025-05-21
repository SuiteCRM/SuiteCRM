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

class CloudVmCluster extends \Google\Model
{
  /**
   * @var string
   */
  public $backupSubnetCidr;
  /**
   * @var string
   */
  public $cidr;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $exadataInfrastructure;
  /**
   * @var string
   */
  public $gcpOracleZone;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $network;
  protected $propertiesType = CloudVmClusterProperties::class;
  protected $propertiesDataType = '';

  /**
   * @param string
   */
  public function setBackupSubnetCidr($backupSubnetCidr)
  {
    $this->backupSubnetCidr = $backupSubnetCidr;
  }
  /**
   * @return string
   */
  public function getBackupSubnetCidr()
  {
    return $this->backupSubnetCidr;
  }
  /**
   * @param string
   */
  public function setCidr($cidr)
  {
    $this->cidr = $cidr;
  }
  /**
   * @return string
   */
  public function getCidr()
  {
    return $this->cidr;
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
   * @param string
   */
  public function setExadataInfrastructure($exadataInfrastructure)
  {
    $this->exadataInfrastructure = $exadataInfrastructure;
  }
  /**
   * @return string
   */
  public function getExadataInfrastructure()
  {
    return $this->exadataInfrastructure;
  }
  /**
   * @param string
   */
  public function setGcpOracleZone($gcpOracleZone)
  {
    $this->gcpOracleZone = $gcpOracleZone;
  }
  /**
   * @return string
   */
  public function getGcpOracleZone()
  {
    return $this->gcpOracleZone;
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
   * @param CloudVmClusterProperties
   */
  public function setProperties(CloudVmClusterProperties $properties)
  {
    $this->properties = $properties;
  }
  /**
   * @return CloudVmClusterProperties
   */
  public function getProperties()
  {
    return $this->properties;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CloudVmCluster::class, 'Google_Service_OracleDatabase_CloudVmCluster');
