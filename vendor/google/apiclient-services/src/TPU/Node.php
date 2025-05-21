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

namespace Google\Service\TPU;

class Node extends \Google\Collection
{
  protected $collection_key = 'tags';
  protected $acceleratorConfigType = AcceleratorConfig::class;
  protected $acceleratorConfigDataType = '';
  /**
   * @var string
   */
  public $acceleratorType;
  /**
   * @var string
   */
  public $apiVersion;
  /**
   * @var string
   */
  public $cidrBlock;
  /**
   * @var string
   */
  public $createTime;
  protected $dataDisksType = AttachedDisk::class;
  protected $dataDisksDataType = 'array';
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $health;
  /**
   * @var string
   */
  public $healthDescription;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string[]
   */
  public $metadata;
  /**
   * @var bool
   */
  public $multisliceNode;
  /**
   * @var string
   */
  public $name;
  protected $networkConfigType = NetworkConfig::class;
  protected $networkConfigDataType = '';
  protected $networkConfigsType = NetworkConfig::class;
  protected $networkConfigsDataType = 'array';
  protected $networkEndpointsType = NetworkEndpoint::class;
  protected $networkEndpointsDataType = 'array';
  /**
   * @var string
   */
  public $queuedResource;
  /**
   * @var string
   */
  public $runtimeVersion;
  protected $schedulingConfigType = SchedulingConfig::class;
  protected $schedulingConfigDataType = '';
  protected $serviceAccountType = ServiceAccount::class;
  protected $serviceAccountDataType = '';
  protected $shieldedInstanceConfigType = ShieldedInstanceConfig::class;
  protected $shieldedInstanceConfigDataType = '';
  /**
   * @var string
   */
  public $state;
  protected $symptomsType = Symptom::class;
  protected $symptomsDataType = 'array';
  /**
   * @var string[]
   */
  public $tags;

  /**
   * @param AcceleratorConfig
   */
  public function setAcceleratorConfig(AcceleratorConfig $acceleratorConfig)
  {
    $this->acceleratorConfig = $acceleratorConfig;
  }
  /**
   * @return AcceleratorConfig
   */
  public function getAcceleratorConfig()
  {
    return $this->acceleratorConfig;
  }
  /**
   * @param string
   */
  public function setAcceleratorType($acceleratorType)
  {
    $this->acceleratorType = $acceleratorType;
  }
  /**
   * @return string
   */
  public function getAcceleratorType()
  {
    return $this->acceleratorType;
  }
  /**
   * @param string
   */
  public function setApiVersion($apiVersion)
  {
    $this->apiVersion = $apiVersion;
  }
  /**
   * @return string
   */
  public function getApiVersion()
  {
    return $this->apiVersion;
  }
  /**
   * @param string
   */
  public function setCidrBlock($cidrBlock)
  {
    $this->cidrBlock = $cidrBlock;
  }
  /**
   * @return string
   */
  public function getCidrBlock()
  {
    return $this->cidrBlock;
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
   * @param AttachedDisk[]
   */
  public function setDataDisks($dataDisks)
  {
    $this->dataDisks = $dataDisks;
  }
  /**
   * @return AttachedDisk[]
   */
  public function getDataDisks()
  {
    return $this->dataDisks;
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
  public function setHealth($health)
  {
    $this->health = $health;
  }
  /**
   * @return string
   */
  public function getHealth()
  {
    return $this->health;
  }
  /**
   * @param string
   */
  public function setHealthDescription($healthDescription)
  {
    $this->healthDescription = $healthDescription;
  }
  /**
   * @return string
   */
  public function getHealthDescription()
  {
    return $this->healthDescription;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
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
   * @param string[]
   */
  public function setMetadata($metadata)
  {
    $this->metadata = $metadata;
  }
  /**
   * @return string[]
   */
  public function getMetadata()
  {
    return $this->metadata;
  }
  /**
   * @param bool
   */
  public function setMultisliceNode($multisliceNode)
  {
    $this->multisliceNode = $multisliceNode;
  }
  /**
   * @return bool
   */
  public function getMultisliceNode()
  {
    return $this->multisliceNode;
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
   * @param NetworkConfig
   */
  public function setNetworkConfig(NetworkConfig $networkConfig)
  {
    $this->networkConfig = $networkConfig;
  }
  /**
   * @return NetworkConfig
   */
  public function getNetworkConfig()
  {
    return $this->networkConfig;
  }
  /**
   * @param NetworkConfig[]
   */
  public function setNetworkConfigs($networkConfigs)
  {
    $this->networkConfigs = $networkConfigs;
  }
  /**
   * @return NetworkConfig[]
   */
  public function getNetworkConfigs()
  {
    return $this->networkConfigs;
  }
  /**
   * @param NetworkEndpoint[]
   */
  public function setNetworkEndpoints($networkEndpoints)
  {
    $this->networkEndpoints = $networkEndpoints;
  }
  /**
   * @return NetworkEndpoint[]
   */
  public function getNetworkEndpoints()
  {
    return $this->networkEndpoints;
  }
  /**
   * @param string
   */
  public function setQueuedResource($queuedResource)
  {
    $this->queuedResource = $queuedResource;
  }
  /**
   * @return string
   */
  public function getQueuedResource()
  {
    return $this->queuedResource;
  }
  /**
   * @param string
   */
  public function setRuntimeVersion($runtimeVersion)
  {
    $this->runtimeVersion = $runtimeVersion;
  }
  /**
   * @return string
   */
  public function getRuntimeVersion()
  {
    return $this->runtimeVersion;
  }
  /**
   * @param SchedulingConfig
   */
  public function setSchedulingConfig(SchedulingConfig $schedulingConfig)
  {
    $this->schedulingConfig = $schedulingConfig;
  }
  /**
   * @return SchedulingConfig
   */
  public function getSchedulingConfig()
  {
    return $this->schedulingConfig;
  }
  /**
   * @param ServiceAccount
   */
  public function setServiceAccount(ServiceAccount $serviceAccount)
  {
    $this->serviceAccount = $serviceAccount;
  }
  /**
   * @return ServiceAccount
   */
  public function getServiceAccount()
  {
    return $this->serviceAccount;
  }
  /**
   * @param ShieldedInstanceConfig
   */
  public function setShieldedInstanceConfig(ShieldedInstanceConfig $shieldedInstanceConfig)
  {
    $this->shieldedInstanceConfig = $shieldedInstanceConfig;
  }
  /**
   * @return ShieldedInstanceConfig
   */
  public function getShieldedInstanceConfig()
  {
    return $this->shieldedInstanceConfig;
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
   * @param Symptom[]
   */
  public function setSymptoms($symptoms)
  {
    $this->symptoms = $symptoms;
  }
  /**
   * @return Symptom[]
   */
  public function getSymptoms()
  {
    return $this->symptoms;
  }
  /**
   * @param string[]
   */
  public function setTags($tags)
  {
    $this->tags = $tags;
  }
  /**
   * @return string[]
   */
  public function getTags()
  {
    return $this->tags;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Node::class, 'Google_Service_TPU_Node');
