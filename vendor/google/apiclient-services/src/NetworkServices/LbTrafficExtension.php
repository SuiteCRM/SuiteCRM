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

class LbTrafficExtension extends \Google\Collection
{
  protected $collection_key = 'forwardingRules';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  protected $extensionChainsType = ExtensionChain::class;
  protected $extensionChainsDataType = 'array';
  /**
   * @var string[]
   */
  public $forwardingRules;
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
  public $updateTime;

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
   * @param ExtensionChain[]
   */
  public function setExtensionChains($extensionChains)
  {
    $this->extensionChains = $extensionChains;
  }
  /**
   * @return ExtensionChain[]
   */
  public function getExtensionChains()
  {
    return $this->extensionChains;
  }
  /**
   * @param string[]
   */
  public function setForwardingRules($forwardingRules)
  {
    $this->forwardingRules = $forwardingRules;
  }
  /**
   * @return string[]
   */
  public function getForwardingRules()
  {
    return $this->forwardingRules;
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
class_alias(LbTrafficExtension::class, 'Google_Service_NetworkServices_LbTrafficExtension');
