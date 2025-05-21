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

namespace Google\Service\SQLAdmin;

class ConnectSettings extends \Google\Collection
{
  protected $collection_key = 'ipAddresses';
  /**
   * @var string
   */
  public $backendType;
  /**
   * @var string[]
   */
  public $customSubjectAlternativeNames;
  /**
   * @var string
   */
  public $databaseVersion;
  /**
   * @var string
   */
  public $dnsName;
  protected $ipAddressesType = IpMapping::class;
  protected $ipAddressesDataType = 'array';
  /**
   * @var string
   */
  public $kind;
  /**
   * @var bool
   */
  public $pscEnabled;
  /**
   * @var string
   */
  public $region;
  protected $serverCaCertType = SslCert::class;
  protected $serverCaCertDataType = '';
  /**
   * @var string
   */
  public $serverCaMode;

  /**
   * @param string
   */
  public function setBackendType($backendType)
  {
    $this->backendType = $backendType;
  }
  /**
   * @return string
   */
  public function getBackendType()
  {
    return $this->backendType;
  }
  /**
   * @param string[]
   */
  public function setCustomSubjectAlternativeNames($customSubjectAlternativeNames)
  {
    $this->customSubjectAlternativeNames = $customSubjectAlternativeNames;
  }
  /**
   * @return string[]
   */
  public function getCustomSubjectAlternativeNames()
  {
    return $this->customSubjectAlternativeNames;
  }
  /**
   * @param string
   */
  public function setDatabaseVersion($databaseVersion)
  {
    $this->databaseVersion = $databaseVersion;
  }
  /**
   * @return string
   */
  public function getDatabaseVersion()
  {
    return $this->databaseVersion;
  }
  /**
   * @param string
   */
  public function setDnsName($dnsName)
  {
    $this->dnsName = $dnsName;
  }
  /**
   * @return string
   */
  public function getDnsName()
  {
    return $this->dnsName;
  }
  /**
   * @param IpMapping[]
   */
  public function setIpAddresses($ipAddresses)
  {
    $this->ipAddresses = $ipAddresses;
  }
  /**
   * @return IpMapping[]
   */
  public function getIpAddresses()
  {
    return $this->ipAddresses;
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
   * @param bool
   */
  public function setPscEnabled($pscEnabled)
  {
    $this->pscEnabled = $pscEnabled;
  }
  /**
   * @return bool
   */
  public function getPscEnabled()
  {
    return $this->pscEnabled;
  }
  /**
   * @param string
   */
  public function setRegion($region)
  {
    $this->region = $region;
  }
  /**
   * @return string
   */
  public function getRegion()
  {
    return $this->region;
  }
  /**
   * @param SslCert
   */
  public function setServerCaCert(SslCert $serverCaCert)
  {
    $this->serverCaCert = $serverCaCert;
  }
  /**
   * @return SslCert
   */
  public function getServerCaCert()
  {
    return $this->serverCaCert;
  }
  /**
   * @param string
   */
  public function setServerCaMode($serverCaMode)
  {
    $this->serverCaMode = $serverCaMode;
  }
  /**
   * @return string
   */
  public function getServerCaMode()
  {
    return $this->serverCaMode;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ConnectSettings::class, 'Google_Service_SQLAdmin_ConnectSettings');
