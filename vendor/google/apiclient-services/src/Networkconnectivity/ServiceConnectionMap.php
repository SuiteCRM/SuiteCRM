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

namespace Google\Service\Networkconnectivity;

class ServiceConnectionMap extends \Google\Collection
{
  protected $collection_key = 'producerPscConfigs';
  protected $consumerPscConfigsType = ConsumerPscConfig::class;
  protected $consumerPscConfigsDataType = 'array';
  protected $consumerPscConnectionsType = ConsumerPscConnection::class;
  protected $consumerPscConnectionsDataType = 'array';
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
  public $etag;
  /**
   * @var string
   */
  public $infrastructure;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  protected $producerPscConfigsType = ProducerPscConfig::class;
  protected $producerPscConfigsDataType = 'array';
  /**
   * @var string
   */
  public $serviceClass;
  /**
   * @var string
   */
  public $serviceClassUri;
  /**
   * @var string
   */
  public $token;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param ConsumerPscConfig[]
   */
  public function setConsumerPscConfigs($consumerPscConfigs)
  {
    $this->consumerPscConfigs = $consumerPscConfigs;
  }
  /**
   * @return ConsumerPscConfig[]
   */
  public function getConsumerPscConfigs()
  {
    return $this->consumerPscConfigs;
  }
  /**
   * @param ConsumerPscConnection[]
   */
  public function setConsumerPscConnections($consumerPscConnections)
  {
    $this->consumerPscConnections = $consumerPscConnections;
  }
  /**
   * @return ConsumerPscConnection[]
   */
  public function getConsumerPscConnections()
  {
    return $this->consumerPscConnections;
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
  public function setInfrastructure($infrastructure)
  {
    $this->infrastructure = $infrastructure;
  }
  /**
   * @return string
   */
  public function getInfrastructure()
  {
    return $this->infrastructure;
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
   * @param ProducerPscConfig[]
   */
  public function setProducerPscConfigs($producerPscConfigs)
  {
    $this->producerPscConfigs = $producerPscConfigs;
  }
  /**
   * @return ProducerPscConfig[]
   */
  public function getProducerPscConfigs()
  {
    return $this->producerPscConfigs;
  }
  /**
   * @param string
   */
  public function setServiceClass($serviceClass)
  {
    $this->serviceClass = $serviceClass;
  }
  /**
   * @return string
   */
  public function getServiceClass()
  {
    return $this->serviceClass;
  }
  /**
   * @param string
   */
  public function setServiceClassUri($serviceClassUri)
  {
    $this->serviceClassUri = $serviceClassUri;
  }
  /**
   * @return string
   */
  public function getServiceClassUri()
  {
    return $this->serviceClassUri;
  }
  /**
   * @param string
   */
  public function setToken($token)
  {
    $this->token = $token;
  }
  /**
   * @return string
   */
  public function getToken()
  {
    return $this->token;
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
class_alias(ServiceConnectionMap::class, 'Google_Service_Networkconnectivity_ServiceConnectionMap');
