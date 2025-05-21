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

namespace Google\Service\NetworkManagement;

class Endpoint extends \Google\Model
{
  protected $appEngineVersionType = AppEngineVersionEndpoint::class;
  protected $appEngineVersionDataType = '';
  protected $cloudFunctionType = CloudFunctionEndpoint::class;
  protected $cloudFunctionDataType = '';
  protected $cloudRunRevisionType = CloudRunRevisionEndpoint::class;
  protected $cloudRunRevisionDataType = '';
  /**
   * @var string
   */
  public $cloudSqlInstance;
  /**
   * @var string
   */
  public $forwardingRule;
  /**
   * @var string
   */
  public $forwardingRuleTarget;
  /**
   * @var string
   */
  public $fqdn;
  /**
   * @var string
   */
  public $gkeMasterCluster;
  /**
   * @var string
   */
  public $instance;
  /**
   * @var string
   */
  public $ipAddress;
  /**
   * @var string
   */
  public $loadBalancerId;
  /**
   * @var string
   */
  public $loadBalancerType;
  /**
   * @var string
   */
  public $network;
  /**
   * @var string
   */
  public $networkType;
  /**
   * @var int
   */
  public $port;
  /**
   * @var string
   */
  public $projectId;
  /**
   * @var string
   */
  public $redisCluster;
  /**
   * @var string
   */
  public $redisInstance;

  /**
   * @param AppEngineVersionEndpoint
   */
  public function setAppEngineVersion(AppEngineVersionEndpoint $appEngineVersion)
  {
    $this->appEngineVersion = $appEngineVersion;
  }
  /**
   * @return AppEngineVersionEndpoint
   */
  public function getAppEngineVersion()
  {
    return $this->appEngineVersion;
  }
  /**
   * @param CloudFunctionEndpoint
   */
  public function setCloudFunction(CloudFunctionEndpoint $cloudFunction)
  {
    $this->cloudFunction = $cloudFunction;
  }
  /**
   * @return CloudFunctionEndpoint
   */
  public function getCloudFunction()
  {
    return $this->cloudFunction;
  }
  /**
   * @param CloudRunRevisionEndpoint
   */
  public function setCloudRunRevision(CloudRunRevisionEndpoint $cloudRunRevision)
  {
    $this->cloudRunRevision = $cloudRunRevision;
  }
  /**
   * @return CloudRunRevisionEndpoint
   */
  public function getCloudRunRevision()
  {
    return $this->cloudRunRevision;
  }
  /**
   * @param string
   */
  public function setCloudSqlInstance($cloudSqlInstance)
  {
    $this->cloudSqlInstance = $cloudSqlInstance;
  }
  /**
   * @return string
   */
  public function getCloudSqlInstance()
  {
    return $this->cloudSqlInstance;
  }
  /**
   * @param string
   */
  public function setForwardingRule($forwardingRule)
  {
    $this->forwardingRule = $forwardingRule;
  }
  /**
   * @return string
   */
  public function getForwardingRule()
  {
    return $this->forwardingRule;
  }
  /**
   * @param string
   */
  public function setForwardingRuleTarget($forwardingRuleTarget)
  {
    $this->forwardingRuleTarget = $forwardingRuleTarget;
  }
  /**
   * @return string
   */
  public function getForwardingRuleTarget()
  {
    return $this->forwardingRuleTarget;
  }
  /**
   * @param string
   */
  public function setFqdn($fqdn)
  {
    $this->fqdn = $fqdn;
  }
  /**
   * @return string
   */
  public function getFqdn()
  {
    return $this->fqdn;
  }
  /**
   * @param string
   */
  public function setGkeMasterCluster($gkeMasterCluster)
  {
    $this->gkeMasterCluster = $gkeMasterCluster;
  }
  /**
   * @return string
   */
  public function getGkeMasterCluster()
  {
    return $this->gkeMasterCluster;
  }
  /**
   * @param string
   */
  public function setInstance($instance)
  {
    $this->instance = $instance;
  }
  /**
   * @return string
   */
  public function getInstance()
  {
    return $this->instance;
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
   * @param string
   */
  public function setLoadBalancerId($loadBalancerId)
  {
    $this->loadBalancerId = $loadBalancerId;
  }
  /**
   * @return string
   */
  public function getLoadBalancerId()
  {
    return $this->loadBalancerId;
  }
  /**
   * @param string
   */
  public function setLoadBalancerType($loadBalancerType)
  {
    $this->loadBalancerType = $loadBalancerType;
  }
  /**
   * @return string
   */
  public function getLoadBalancerType()
  {
    return $this->loadBalancerType;
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
   * @param string
   */
  public function setNetworkType($networkType)
  {
    $this->networkType = $networkType;
  }
  /**
   * @return string
   */
  public function getNetworkType()
  {
    return $this->networkType;
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
   * @param string
   */
  public function setProjectId($projectId)
  {
    $this->projectId = $projectId;
  }
  /**
   * @return string
   */
  public function getProjectId()
  {
    return $this->projectId;
  }
  /**
   * @param string
   */
  public function setRedisCluster($redisCluster)
  {
    $this->redisCluster = $redisCluster;
  }
  /**
   * @return string
   */
  public function getRedisCluster()
  {
    return $this->redisCluster;
  }
  /**
   * @param string
   */
  public function setRedisInstance($redisInstance)
  {
    $this->redisInstance = $redisInstance;
  }
  /**
   * @return string
   */
  public function getRedisInstance()
  {
    return $this->redisInstance;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Endpoint::class, 'Google_Service_NetworkManagement_Endpoint');
