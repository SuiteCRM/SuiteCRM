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

class AutonomousDatabase extends \Google\Model
{
  /**
   * @var string
   */
  public $adminPassword;
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
  public $database;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $entitlementId;
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
  protected $propertiesType = AutonomousDatabaseProperties::class;
  protected $propertiesDataType = '';

  /**
   * @param string
   */
  public function setAdminPassword($adminPassword)
  {
    $this->adminPassword = $adminPassword;
  }
  /**
   * @return string
   */
  public function getAdminPassword()
  {
    return $this->adminPassword;
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
  public function setDatabase($database)
  {
    $this->database = $database;
  }
  /**
   * @return string
   */
  public function getDatabase()
  {
    return $this->database;
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
  public function setEntitlementId($entitlementId)
  {
    $this->entitlementId = $entitlementId;
  }
  /**
   * @return string
   */
  public function getEntitlementId()
  {
    return $this->entitlementId;
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
   * @param AutonomousDatabaseProperties
   */
  public function setProperties(AutonomousDatabaseProperties $properties)
  {
    $this->properties = $properties;
  }
  /**
   * @return AutonomousDatabaseProperties
   */
  public function getProperties()
  {
    return $this->properties;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AutonomousDatabase::class, 'Google_Service_OracleDatabase_AutonomousDatabase');
