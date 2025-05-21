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

namespace Google\Service\CloudAlloyDBAdmin;

class Instance extends \Google\Collection
{
  protected $collection_key = 'outboundPublicIpAddresses';
  /**
   * @var string[]
   */
  public $annotations;
  /**
   * @var string
   */
  public $availabilityType;
  protected $clientConnectionConfigType = ClientConnectionConfig::class;
  protected $clientConnectionConfigDataType = '';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string[]
   */
  public $databaseFlags;
  /**
   * @var string
   */
  public $deleteTime;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $etag;
  /**
   * @var string
   */
  public $gceZone;
  /**
   * @var string
   */
  public $instanceType;
  /**
   * @var string
   */
  public $ipAddress;
  /**
   * @var string[]
   */
  public $labels;
  protected $machineConfigType = MachineConfig::class;
  protected $machineConfigDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $networkConfigType = InstanceNetworkConfig::class;
  protected $networkConfigDataType = '';
  protected $nodesType = Node::class;
  protected $nodesDataType = 'array';
  /**
   * @var string[]
   */
  public $outboundPublicIpAddresses;
  protected $pscInstanceConfigType = PscInstanceConfig::class;
  protected $pscInstanceConfigDataType = '';
  /**
   * @var string
   */
  public $publicIpAddress;
  protected $queryInsightsConfigType = QueryInsightsInstanceConfig::class;
  protected $queryInsightsConfigDataType = '';
  protected $readPoolConfigType = ReadPoolConfig::class;
  protected $readPoolConfigDataType = '';
  /**
   * @var bool
   */
  public $reconciling;
  /**
   * @var bool
   */
  public $satisfiesPzs;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $uid;
  /**
   * @var string
   */
  public $updateTime;
  protected $writableNodeType = Node::class;
  protected $writableNodeDataType = '';

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
   * @param string
   */
  public function setAvailabilityType($availabilityType)
  {
    $this->availabilityType = $availabilityType;
  }
  /**
   * @return string
   */
  public function getAvailabilityType()
  {
    return $this->availabilityType;
  }
  /**
   * @param ClientConnectionConfig
   */
  public function setClientConnectionConfig(ClientConnectionConfig $clientConnectionConfig)
  {
    $this->clientConnectionConfig = $clientConnectionConfig;
  }
  /**
   * @return ClientConnectionConfig
   */
  public function getClientConnectionConfig()
  {
    return $this->clientConnectionConfig;
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
   * @param string[]
   */
  public function setDatabaseFlags($databaseFlags)
  {
    $this->databaseFlags = $databaseFlags;
  }
  /**
   * @return string[]
   */
  public function getDatabaseFlags()
  {
    return $this->databaseFlags;
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
   * @param string
   */
  public function setGceZone($gceZone)
  {
    $this->gceZone = $gceZone;
  }
  /**
   * @return string
   */
  public function getGceZone()
  {
    return $this->gceZone;
  }
  /**
   * @param string
   */
  public function setInstanceType($instanceType)
  {
    $this->instanceType = $instanceType;
  }
  /**
   * @return string
   */
  public function getInstanceType()
  {
    return $this->instanceType;
  }
  /**
   * @param string
   */
  public function setIpAddress($ipAddress)
  {
    $this->ipAddress = $ipAddress;
  }
  /**
   * @return string
   */
  public function getIpAddress()
  {
    return $this->ipAddress;
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
   * @param MachineConfig
   */
  public function setMachineConfig(MachineConfig $machineConfig)
  {
    $this->machineConfig = $machineConfig;
  }
  /**
   * @return MachineConfig
   */
  public function getMachineConfig()
  {
    return $this->machineConfig;
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
   * @param InstanceNetworkConfig
   */
  public function setNetworkConfig(InstanceNetworkConfig $networkConfig)
  {
    $this->networkConfig = $networkConfig;
  }
  /**
   * @return InstanceNetworkConfig
   */
  public function getNetworkConfig()
  {
    return $this->networkConfig;
  }
  /**
   * @param Node[]
   */
  public function setNodes($nodes)
  {
    $this->nodes = $nodes;
  }
  /**
   * @return Node[]
   */
  public function getNodes()
  {
    return $this->nodes;
  }
  /**
   * @param string[]
   */
  public function setOutboundPublicIpAddresses($outboundPublicIpAddresses)
  {
    $this->outboundPublicIpAddresses = $outboundPublicIpAddresses;
  }
  /**
   * @return string[]
   */
  public function getOutboundPublicIpAddresses()
  {
    return $this->outboundPublicIpAddresses;
  }
  /**
   * @param PscInstanceConfig
   */
  public function setPscInstanceConfig(PscInstanceConfig $pscInstanceConfig)
  {
    $this->pscInstanceConfig = $pscInstanceConfig;
  }
  /**
   * @return PscInstanceConfig
   */
  public function getPscInstanceConfig()
  {
    return $this->pscInstanceConfig;
  }
  /**
   * @param string
   */
  public function setPublicIpAddress($publicIpAddress)
  {
    $this->publicIpAddress = $publicIpAddress;
  }
  /**
   * @return string
   */
  public function getPublicIpAddress()
  {
    return $this->publicIpAddress;
  }
  /**
   * @param QueryInsightsInstanceConfig
   */
  public function setQueryInsightsConfig(QueryInsightsInstanceConfig $queryInsightsConfig)
  {
    $this->queryInsightsConfig = $queryInsightsConfig;
  }
  /**
   * @return QueryInsightsInstanceConfig
   */
  public function getQueryInsightsConfig()
  {
    return $this->queryInsightsConfig;
  }
  /**
   * @param ReadPoolConfig
   */
  public function setReadPoolConfig(ReadPoolConfig $readPoolConfig)
  {
    $this->readPoolConfig = $readPoolConfig;
  }
  /**
   * @return ReadPoolConfig
   */
  public function getReadPoolConfig()
  {
    return $this->readPoolConfig;
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
   * @param bool
   */
  public function setSatisfiesPzs($satisfiesPzs)
  {
    $this->satisfiesPzs = $satisfiesPzs;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzs()
  {
    return $this->satisfiesPzs;
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
  /**
   * @param Node
   */
  public function setWritableNode(Node $writableNode)
  {
    $this->writableNode = $writableNode;
  }
  /**
   * @return Node
   */
  public function getWritableNode()
  {
    return $this->writableNode;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Instance::class, 'Google_Service_CloudAlloyDBAdmin_Instance');
