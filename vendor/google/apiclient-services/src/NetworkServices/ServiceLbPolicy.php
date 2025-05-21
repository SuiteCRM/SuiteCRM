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

namespace Google\Service\NetworkServices;

class ServiceLbPolicy extends \Google\Model
{
  protected $autoCapacityDrainType = ServiceLbPolicyAutoCapacityDrain::class;
  protected $autoCapacityDrainDataType = '';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  protected $failoverConfigType = ServiceLbPolicyFailoverConfig::class;
  protected $failoverConfigDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $loadBalancingAlgorithm;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param ServiceLbPolicyAutoCapacityDrain
   */
  public function setAutoCapacityDrain(ServiceLbPolicyAutoCapacityDrain $autoCapacityDrain)
  {
    $this->autoCapacityDrain = $autoCapacityDrain;
  }
  /**
   * @return ServiceLbPolicyAutoCapacityDrain
   */
  public function getAutoCapacityDrain()
  {
    return $this->autoCapacityDrain;
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
   * @param ServiceLbPolicyFailoverConfig
   */
  public function setFailoverConfig(ServiceLbPolicyFailoverConfig $failoverConfig)
  {
    $this->failoverConfig = $failoverConfig;
  }
  /**
   * @return ServiceLbPolicyFailoverConfig
   */
  public function getFailoverConfig()
  {
    return $this->failoverConfig;
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
  public function setLoadBalancingAlgorithm($loadBalancingAlgorithm)
  {
    $this->loadBalancingAlgorithm = $loadBalancingAlgorithm;
  }
  /**
   * @return string
   */
  public function getLoadBalancingAlgorithm()
  {
    return $this->loadBalancingAlgorithm;
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
class_alias(ServiceLbPolicy::class, 'Google_Service_NetworkServices_ServiceLbPolicy');
