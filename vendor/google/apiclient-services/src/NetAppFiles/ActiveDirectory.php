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

namespace Google\Service\NetAppFiles;

class ActiveDirectory extends \Google\Collection
{
  protected $collection_key = 'securityOperators';
  /**
   * @var string[]
   */
  public $administrators;
  /**
   * @var bool
   */
  public $aesEncryption;
  /**
   * @var string[]
   */
  public $backupOperators;
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
  public $dns;
  /**
   * @var string
   */
  public $domain;
  /**
   * @var bool
   */
  public $encryptDcConnections;
  /**
   * @var string
   */
  public $kdcHostname;
  /**
   * @var string
   */
  public $kdcIp;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var bool
   */
  public $ldapSigning;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $netBiosPrefix;
  /**
   * @var bool
   */
  public $nfsUsersWithLdap;
  /**
   * @var string
   */
  public $organizationalUnit;
  /**
   * @var string
   */
  public $password;
  /**
   * @var string[]
   */
  public $securityOperators;
  /**
   * @var string
   */
  public $site;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $stateDetails;
  /**
   * @var string
   */
  public $username;

  /**
   * @param string[]
   */
  public function setAdministrators($administrators)
  {
    $this->administrators = $administrators;
  }
  /**
   * @return string[]
   */
  public function getAdministrators()
  {
    return $this->administrators;
  }
  /**
   * @param bool
   */
  public function setAesEncryption($aesEncryption)
  {
    $this->aesEncryption = $aesEncryption;
  }
  /**
   * @return bool
   */
  public function getAesEncryption()
  {
    return $this->aesEncryption;
  }
  /**
   * @param string[]
   */
  public function setBackupOperators($backupOperators)
  {
    $this->backupOperators = $backupOperators;
  }
  /**
   * @return string[]
   */
  public function getBackupOperators()
  {
    return $this->backupOperators;
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
  public function setDns($dns)
  {
    $this->dns = $dns;
  }
  /**
   * @return string
   */
  public function getDns()
  {
    return $this->dns;
  }
  /**
   * @param string
   */
  public function setDomain($domain)
  {
    $this->domain = $domain;
  }
  /**
   * @return string
   */
  public function getDomain()
  {
    return $this->domain;
  }
  /**
   * @param bool
   */
  public function setEncryptDcConnections($encryptDcConnections)
  {
    $this->encryptDcConnections = $encryptDcConnections;
  }
  /**
   * @return bool
   */
  public function getEncryptDcConnections()
  {
    return $this->encryptDcConnections;
  }
  /**
   * @param string
   */
  public function setKdcHostname($kdcHostname)
  {
    $this->kdcHostname = $kdcHostname;
  }
  /**
   * @return string
   */
  public function getKdcHostname()
  {
    return $this->kdcHostname;
  }
  /**
   * @param string
   */
  public function setKdcIp($kdcIp)
  {
    $this->kdcIp = $kdcIp;
  }
  /**
   * @return string
   */
  public function getKdcIp()
  {
    return $this->kdcIp;
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
   * @param bool
   */
  public function setLdapSigning($ldapSigning)
  {
    $this->ldapSigning = $ldapSigning;
  }
  /**
   * @return bool
   */
  public function getLdapSigning()
  {
    return $this->ldapSigning;
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
  public function setNetBiosPrefix($netBiosPrefix)
  {
    $this->netBiosPrefix = $netBiosPrefix;
  }
  /**
   * @return string
   */
  public function getNetBiosPrefix()
  {
    return $this->netBiosPrefix;
  }
  /**
   * @param bool
   */
  public function setNfsUsersWithLdap($nfsUsersWithLdap)
  {
    $this->nfsUsersWithLdap = $nfsUsersWithLdap;
  }
  /**
   * @return bool
   */
  public function getNfsUsersWithLdap()
  {
    return $this->nfsUsersWithLdap;
  }
  /**
   * @param string
   */
  public function setOrganizationalUnit($organizationalUnit)
  {
    $this->organizationalUnit = $organizationalUnit;
  }
  /**
   * @return string
   */
  public function getOrganizationalUnit()
  {
    return $this->organizationalUnit;
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
   * @param string[]
   */
  public function setSecurityOperators($securityOperators)
  {
    $this->securityOperators = $securityOperators;
  }
  /**
   * @return string[]
   */
  public function getSecurityOperators()
  {
    return $this->securityOperators;
  }
  /**
   * @param string
   */
  public function setSite($site)
  {
    $this->site = $site;
  }
  /**
   * @return string
   */
  public function getSite()
  {
    return $this->site;
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
   * @param string
   */
  public function setStateDetails($stateDetails)
  {
    $this->stateDetails = $stateDetails;
  }
  /**
   * @return string
   */
  public function getStateDetails()
  {
    return $this->stateDetails;
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
class_alias(ActiveDirectory::class, 'Google_Service_NetAppFiles_ActiveDirectory');
