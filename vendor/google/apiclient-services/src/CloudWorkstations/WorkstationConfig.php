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

namespace Google\Service\CloudWorkstations;

class WorkstationConfig extends \Google\Collection
{
  protected $collection_key = 'replicaZones';
  protected $allowedPortsType = PortRange::class;
  protected $allowedPortsDataType = 'array';
  /**
   * @var string[]
   */
  public $annotations;
  protected $conditionsType = Status::class;
  protected $conditionsDataType = 'array';
  protected $containerType = Container::class;
  protected $containerDataType = '';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var bool
   */
  public $degraded;
  /**
   * @var string
   */
  public $deleteTime;
  /**
   * @var bool
   */
  public $disableTcpConnections;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var bool
   */
  public $enableAuditAgent;
  protected $encryptionKeyType = CustomerEncryptionKey::class;
  protected $encryptionKeyDataType = '';
  protected $ephemeralDirectoriesType = EphemeralDirectory::class;
  protected $ephemeralDirectoriesDataType = 'array';
  /**
   * @var string
   */
  public $etag;
  /**
   * @var bool
   */
  public $grantWorkstationAdminRoleOnCreate;
  protected $hostType = Host::class;
  protected $hostDataType = '';
  /**
   * @var string
   */
  public $idleTimeout;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var int
   */
  public $maxUsableWorkstations;
  /**
   * @var string
   */
  public $name;
  protected $persistentDirectoriesType = PersistentDirectory::class;
  protected $persistentDirectoriesDataType = 'array';
  protected $readinessChecksType = ReadinessCheck::class;
  protected $readinessChecksDataType = 'array';
  /**
   * @var bool
   */
  public $reconciling;
  /**
   * @var string[]
   */
  public $replicaZones;
  /**
   * @var string
   */
  public $runningTimeout;
  /**
   * @var string
   */
  public $uid;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param PortRange[]
   */
  public function setAllowedPorts($allowedPorts)
  {
    $this->allowedPorts = $allowedPorts;
  }
  /**
   * @return PortRange[]
   */
  public function getAllowedPorts()
  {
    return $this->allowedPorts;
  }
  /**
   * @param string[]
   */
  public function setAnnotations($annotations)
  {
    $this->annotations = $annotations;
  }
  /**
   * @return string[]
   */
  public function getAnnotations()
  {
    return $this->annotations;
  }
  /**
   * @param Status[]
   */
  public function setConditions($conditions)
  {
    $this->conditions = $conditions;
  }
  /**
   * @return Status[]
   */
  public function getConditions()
  {
    return $this->conditions;
  }
  /**
   * @param Container
   */
  public function setContainer(Container $container)
  {
    $this->container = $container;
  }
  /**
   * @return Container
   */
  public function getContainer()
  {
    return $this->container;
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
   * @param bool
   */
  public function setDegraded($degraded)
  {
    $this->degraded = $degraded;
  }
  /**
   * @return bool
   */
  public function getDegraded()
  {
    return $this->degraded;
  }
  /**
   * @param string
   */
  public function setDeleteTime($deleteTime)
  {
    $this->deleteTime = $deleteTime;
  }
  /**
   * @return string
   */
  public function getDeleteTime()
  {
    return $this->deleteTime;
  }
  /**
   * @param bool
   */
  public function setDisableTcpConnections($disableTcpConnections)
  {
    $this->disableTcpConnections = $disableTcpConnections;
  }
  /**
   * @return bool
   */
  public function getDisableTcpConnections()
  {
    return $this->disableTcpConnections;
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
   * @param bool
   */
  public function setEnableAuditAgent($enableAuditAgent)
  {
    $this->enableAuditAgent = $enableAuditAgent;
  }
  /**
   * @return bool
   */
  public function getEnableAuditAgent()
  {
    return $this->enableAuditAgent;
  }
  /**
   * @param CustomerEncryptionKey
   */
  public function setEncryptionKey(CustomerEncryptionKey $encryptionKey)
  {
    $this->encryptionKey = $encryptionKey;
  }
  /**
   * @return CustomerEncryptionKey
   */
  public function getEncryptionKey()
  {
    return $this->encryptionKey;
  }
  /**
   * @param EphemeralDirectory[]
   */
  public function setEphemeralDirectories($ephemeralDirectories)
  {
    $this->ephemeralDirectories = $ephemeralDirectories;
  }
  /**
   * @return EphemeralDirectory[]
   */
  public function getEphemeralDirectories()
  {
    return $this->ephemeralDirectories;
  }
  /**
   * @param string
   */
  public function setEtag($etag)
  {
    $this->etag = $etag;
  }
  /**
   * @return string
   */
  public function getEtag()
  {
    return $this->etag;
  }
  /**
   * @param bool
   */
  public function setGrantWorkstationAdminRoleOnCreate($grantWorkstationAdminRoleOnCreate)
  {
    $this->grantWorkstationAdminRoleOnCreate = $grantWorkstationAdminRoleOnCreate;
  }
  /**
   * @return bool
   */
  public function getGrantWorkstationAdminRoleOnCreate()
  {
    return $this->grantWorkstationAdminRoleOnCreate;
  }
  /**
   * @param Host
   */
  public function setHost(Host $host)
  {
    $this->host = $host;
  }
  /**
   * @return Host
   */
  public function getHost()
  {
    return $this->host;
  }
  /**
   * @param string
   */
  public function setIdleTimeout($idleTimeout)
  {
    $this->idleTimeout = $idleTimeout;
  }
  /**
   * @return string
   */
  public function getIdleTimeout()
  {
    return $this->idleTimeout;
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
   * @param int
   */
  public function setMaxUsableWorkstations($maxUsableWorkstations)
  {
    $this->maxUsableWorkstations = $maxUsableWorkstations;
  }
  /**
   * @return int
   */
  public function getMaxUsableWorkstations()
  {
    return $this->maxUsableWorkstations;
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
   * @param PersistentDirectory[]
   */
  public function setPersistentDirectories($persistentDirectories)
  {
    $this->persistentDirectories = $persistentDirectories;
  }
  /**
   * @return PersistentDirectory[]
   */
  public function getPersistentDirectories()
  {
    return $this->persistentDirectories;
  }
  /**
   * @param ReadinessCheck[]
   */
  public function setReadinessChecks($readinessChecks)
  {
    $this->readinessChecks = $readinessChecks;
  }
  /**
   * @return ReadinessCheck[]
   */
  public function getReadinessChecks()
  {
    return $this->readinessChecks;
  }
  /**
   * @param bool
   */
  public function setReconciling($reconciling)
  {
    $this->reconciling = $reconciling;
  }
  /**
   * @return bool
   */
  public function getReconciling()
  {
    return $this->reconciling;
  }
  /**
   * @param string[]
   */
  public function setReplicaZones($replicaZones)
  {
    $this->replicaZones = $replicaZones;
  }
  /**
   * @return string[]
   */
  public function getReplicaZones()
  {
    return $this->replicaZones;
  }
  /**
   * @param string
   */
  public function setRunningTimeout($runningTimeout)
  {
    $this->runningTimeout = $runningTimeout;
  }
  /**
   * @return string
   */
  public function getRunningTimeout()
  {
    return $this->runningTimeout;
  }
  /**
   * @param string
   */
  public function setUid($uid)
  {
    $this->uid = $uid;
  }
  /**
   * @return string
   */
  public function getUid()
  {
    return $this->uid;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(WorkstationConfig::class, 'Google_Service_CloudWorkstations_WorkstationConfig');
