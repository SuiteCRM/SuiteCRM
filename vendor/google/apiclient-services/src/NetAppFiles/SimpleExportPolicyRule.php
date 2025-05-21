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

class SimpleExportPolicyRule extends \Google\Model
{
  /**
   * @var string
   */
  public $accessType;
  /**
   * @var string
   */
  public $allowedClients;
  /**
   * @var string
   */
  public $hasRootAccess;
  /**
   * @var bool
   */
  public $kerberos5ReadOnly;
  /**
   * @var bool
   */
  public $kerberos5ReadWrite;
  /**
   * @var bool
   */
  public $kerberos5iReadOnly;
  /**
   * @var bool
   */
  public $kerberos5iReadWrite;
  /**
   * @var bool
   */
  public $kerberos5pReadOnly;
  /**
   * @var bool
   */
  public $kerberos5pReadWrite;
  /**
   * @var bool
   */
  public $nfsv3;
  /**
   * @var bool
   */
  public $nfsv4;

  /**
   * @param string
   */
  public function setAccessType($accessType)
  {
    $this->accessType = $accessType;
  }
  /**
   * @return string
   */
  public function getAccessType()
  {
    return $this->accessType;
  }
  /**
   * @param string
   */
  public function setAllowedClients($allowedClients)
  {
    $this->allowedClients = $allowedClients;
  }
  /**
   * @return string
   */
  public function getAllowedClients()
  {
    return $this->allowedClients;
  }
  /**
   * @param string
   */
  public function setHasRootAccess($hasRootAccess)
  {
    $this->hasRootAccess = $hasRootAccess;
  }
  /**
   * @return string
   */
  public function getHasRootAccess()
  {
    return $this->hasRootAccess;
  }
  /**
   * @param bool
   */
  public function setKerberos5ReadOnly($kerberos5ReadOnly)
  {
    $this->kerberos5ReadOnly = $kerberos5ReadOnly;
  }
  /**
   * @return bool
   */
  public function getKerberos5ReadOnly()
  {
    return $this->kerberos5ReadOnly;
  }
  /**
   * @param bool
   */
  public function setKerberos5ReadWrite($kerberos5ReadWrite)
  {
    $this->kerberos5ReadWrite = $kerberos5ReadWrite;
  }
  /**
   * @return bool
   */
  public function getKerberos5ReadWrite()
  {
    return $this->kerberos5ReadWrite;
  }
  /**
   * @param bool
   */
  public function setKerberos5iReadOnly($kerberos5iReadOnly)
  {
    $this->kerberos5iReadOnly = $kerberos5iReadOnly;
  }
  /**
   * @return bool
   */
  public function getKerberos5iReadOnly()
  {
    return $this->kerberos5iReadOnly;
  }
  /**
   * @param bool
   */
  public function setKerberos5iReadWrite($kerberos5iReadWrite)
  {
    $this->kerberos5iReadWrite = $kerberos5iReadWrite;
  }
  /**
   * @return bool
   */
  public function getKerberos5iReadWrite()
  {
    return $this->kerberos5iReadWrite;
  }
  /**
   * @param bool
   */
  public function setKerberos5pReadOnly($kerberos5pReadOnly)
  {
    $this->kerberos5pReadOnly = $kerberos5pReadOnly;
  }
  /**
   * @return bool
   */
  public function getKerberos5pReadOnly()
  {
    return $this->kerberos5pReadOnly;
  }
  /**
   * @param bool
   */
  public function setKerberos5pReadWrite($kerberos5pReadWrite)
  {
    $this->kerberos5pReadWrite = $kerberos5pReadWrite;
  }
  /**
   * @return bool
   */
  public function getKerberos5pReadWrite()
  {
    return $this->kerberos5pReadWrite;
  }
  /**
   * @param bool
   */
  public function setNfsv3($nfsv3)
  {
    $this->nfsv3 = $nfsv3;
  }
  /**
   * @return bool
   */
  public function getNfsv3()
  {
    return $this->nfsv3;
  }
  /**
   * @param bool
   */
  public function setNfsv4($nfsv4)
  {
    $this->nfsv4 = $nfsv4;
  }
  /**
   * @return bool
   */
  public function getNfsv4()
  {
    return $this->nfsv4;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SimpleExportPolicyRule::class, 'Google_Service_NetAppFiles_SimpleExportPolicyRule');
