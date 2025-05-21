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

class AuthzExtension extends \Google\Collection
{
  protected $collection_key = 'forwardHeaders';
  /**
   * @var string
   */
  public $authority;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var bool
   */
  public $failOpen;
  /**
   * @var string[]
   */
  public $forwardHeaders;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $loadBalancingScheme;
  /**
   * @var array[]
   */
  public $metadata;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $service;
  /**
   * @var string
   */
  public $timeout;
  /**
   * @var string
   */
  public $updateTime;
  /**
   * @var string
   */
  public $wireFormat;

  /**
   * @param string
   */
  public function setAuthority($authority)
  {
    $this->authority = $authority;
  }
  /**
   * @return string
   */
  public function getAuthority()
  {
    return $this->authority;
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
   * @param bool
   */
  public function setFailOpen($failOpen)
  {
    $this->failOpen = $failOpen;
  }
  /**
   * @return bool
   */
  public function getFailOpen()
  {
    return $this->failOpen;
  }
  /**
   * @param string[]
   */
  public function setForwardHeaders($forwardHeaders)
  {
    $this->forwardHeaders = $forwardHeaders;
  }
  /**
   * @return string[]
   */
  public function getForwardHeaders()
  {
    return $this->forwardHeaders;
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
  public function setLoadBalancingScheme($loadBalancingScheme)
  {
    $this->loadBalancingScheme = $loadBalancingScheme;
  }
  /**
   * @return string
   */
  public function getLoadBalancingScheme()
  {
    return $this->loadBalancingScheme;
  }
  /**
   * @param array[]
   */
  public function setMetadata($metadata)
  {
    $this->metadata = $metadata;
  }
  /**
   * @return array[]
   */
  public function getMetadata()
  {
    return $this->metadata;
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
  public function setService($service)
  {
    $this->service = $service;
  }
  /**
   * @return string
   */
  public function getService()
  {
    return $this->service;
  }
  /**
   * @param string
   */
  public function setTimeout($timeout)
  {
    $this->timeout = $timeout;
  }
  /**
   * @return string
   */
  public function getTimeout()
  {
    return $this->timeout;
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
   * @param string
   */
  public function setWireFormat($wireFormat)
  {
    $this->wireFormat = $wireFormat;
  }
  /**
   * @return string
   */
  public function getWireFormat()
  {
    return $this->wireFormat;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AuthzExtension::class, 'Google_Service_NetworkServices_AuthzExtension');
