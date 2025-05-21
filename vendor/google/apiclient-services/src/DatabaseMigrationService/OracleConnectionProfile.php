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

class OracleConnectionProfile extends \Google\Model
{
  /**
   * @var string
   */
  public $databaseService;
  protected $forwardSshConnectivityType = ForwardSshTunnelConnectivity::class;
  protected $forwardSshConnectivityDataType = '';
  /**
   * @var string
   */
  public $host;
  protected $oracleAsmConfigType = OracleAsmConfig::class;
  protected $oracleAsmConfigDataType = '';
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
  protected $privateConnectivityType = PrivateConnectivity::class;
  protected $privateConnectivityDataType = '';
  protected $sslType = SslConfig::class;
  protected $sslDataType = '';
  protected $staticServiceIpConnectivityType = StaticServiceIpConnectivity::class;
  protected $staticServiceIpConnectivityDataType = '';
  /**
   * @var string
   */
  public $username;

  /**
   * @param string
   */
  public function setDatabaseService($databaseService)
  {
    $this->databaseService = $databaseService;
  }
  /**
   * @return string
   */
  public function getDatabaseService()
  {
    return $this->databaseService;
  }
  /**
   * @param ForwardSshTunnelConnectivity
   */
  public function setForwardSshConnectivity(ForwardSshTunnelConnectivity $forwardSshConnectivity)
  {
    $this->forwardSshConnectivity = $forwardSshConnectivity;
  }
  /**
   * @return ForwardSshTunnelConnectivity
   */
  public function getForwardSshConnectivity()
  {
    return $this->forwardSshConnectivity;
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
   * @param OracleAsmConfig
   */
  public function setOracleAsmConfig(OracleAsmConfig $oracleAsmConfig)
  {
    $this->oracleAsmConfig = $oracleAsmConfig;
  }
  /**
   * @return OracleAsmConfig
   */
  public function getOracleAsmConfig()
  {
    return $this->oracleAsmConfig;
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
   * @param PrivateConnectivity
   */
  public function setPrivateConnectivity(PrivateConnectivity $privateConnectivity)
  {
    $this->privateConnectivity = $privateConnectivity;
  }
  /**
   * @return PrivateConnectivity
   */
  public function getPrivateConnectivity()
  {
    return $this->privateConnectivity;
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
   * @param StaticServiceIpConnectivity
   */
  public function setStaticServiceIpConnectivity(StaticServiceIpConnectivity $staticServiceIpConnectivity)
  {
    $this->staticServiceIpConnectivity = $staticServiceIpConnectivity;
  }
  /**
   * @return StaticServiceIpConnectivity
   */
  public function getStaticServiceIpConnectivity()
  {
    return $this->staticServiceIpConnectivity;
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
class_alias(OracleConnectionProfile::class, 'Google_Service_DatabaseMigrationService_OracleConnectionProfile');
