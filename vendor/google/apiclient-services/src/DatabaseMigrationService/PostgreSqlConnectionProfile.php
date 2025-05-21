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

namespace Google\Service\DatabaseMigrationService;

class PostgreSqlConnectionProfile extends \Google\Model
{
  /**
   * @var string
   */
  public $alloydbClusterId;
  /**
   * @var string
   */
  public $cloudSqlId;
  /**
   * @var string
   */
  public $database;
  /**
   * @var string
   */
  public $host;
  /**
   * @var string
   */
  public $networkArchitecture;
  /**
   * @var string
   */
  public $password;
  /**
   * @var bool
   */
  public $passwordSet;
  /**
   * @var int
   */
  public $port;
  protected $privateServiceConnectConnectivityType = PrivateServiceConnectConnectivity::class;
  protected $privateServiceConnectConnectivityDataType = '';
  protected $sslType = SslConfig::class;
  protected $sslDataType = '';
  protected $staticIpConnectivityType = StaticIpConnectivity::class;
  protected $staticIpConnectivityDataType = '';
  /**
   * @var string
   */
  public $username;

  /**
   * @param string
   */
  public function setAlloydbClusterId($alloydbClusterId)
  {
    $this->alloydbClusterId = $alloydbClusterId;
  }
  /**
   * @return string
   */
  public function getAlloydbClusterId()
  {
    return $this->alloydbClusterId;
  }
  /**
   * @param string
   */
  public function setCloudSqlId($cloudSqlId)
  {
    $this->cloudSqlId = $cloudSqlId;
  }
  /**
   * @return string
   */
  public function getCloudSqlId()
  {
    return $this->cloudSqlId;
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
  public function setHost($host)
  {
    $this->host = $host;
  }
  /**
   * @return string
   */
  public function getHost()
  {
    return $this->host;
  }
  /**
   * @param string
   */
  public function setNetworkArchitecture($networkArchitecture)
  {
    $this->networkArchitecture = $networkArchitecture;
  }
  /**
   * @return string
   */
  public function getNetworkArchitecture()
  {
    return $this->networkArchitecture;
  }
  /**
   * @param string
   */
  public function setPassword($password)
  {
    $this->password = $password;
  }
  /**
   * @return string
   */
  public function getPassword()
  {
    return $this->password;
  }
  /**
   * @param bool
   */
  public function setPasswordSet($passwordSet)
  {
    $this->passwordSet = $passwordSet;
  }
  /**
   * @return bool
   */
  public function getPasswordSet()
  {
    return $this->passwordSet;
  }
  /**
   * @param int
   */
  public function setPort($port)
  {
    $this->port = $port;
  }
  /**
   * @return int
   */
  public function getPort()
  {
    return $this->port;
  }
  /**
   * @param PrivateServiceConnectConnectivity
   */
  public function setPrivateServiceConnectConnectivity(PrivateServiceConnectConnectivity $privateServiceConnectConnectivity)
  {
    $this->privateServiceConnectConnectivity = $privateServiceConnectConnectivity;
  }
  /**
   * @return PrivateServiceConnectConnectivity
   */
  public function getPrivateServiceConnectConnectivity()
  {
    return $this->privateServiceConnectConnectivity;
  }
  /**
   * @param SslConfig
   */
  public function setSsl(SslConfig $ssl)
  {
    $this->ssl = $ssl;
  }
  /**
   * @return SslConfig
   */
  public function getSsl()
  {
    return $this->ssl;
  }
  /**
   * @param StaticIpConnectivity
   */
  public function setStaticIpConnectivity(StaticIpConnectivity $staticIpConnectivity)
  {
    $this->staticIpConnectivity = $staticIpConnectivity;
  }
  /**
   * @return StaticIpConnectivity
   */
  public function getStaticIpConnectivity()
  {
    return $this->staticIpConnectivity;
  }
  /**
   * @param string
   */
  public function setUsername($username)
  {
    $this->username = $username;
  }
  /**
   * @return string
   */
  public function getUsername()
  {
    return $this->username;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PostgreSqlConnectionProfile::class, 'Google_Service_DatabaseMigrationService_PostgreSqlConnectionProfile');
