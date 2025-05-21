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

class Spoke extends \Google\Collection
{
  protected $collection_key = 'reasons';
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
  public $group;
  /**
   * @var string
   */
  public $hub;
  /**
   * @var string[]
   */
  public $labels;
  protected $linkedInterconnectAttachmentsType = LinkedInterconnectAttachments::class;
  protected $linkedInterconnectAttachmentsDataType = '';
  protected $linkedProducerVpcNetworkType = LinkedProducerVpcNetwork::class;
  protected $linkedProducerVpcNetworkDataType = '';
  protected $linkedRouterApplianceInstancesType = LinkedRouterApplianceInstances::class;
  protected $linkedRouterApplianceInstancesDataType = '';
  protected $linkedVpcNetworkType = LinkedVpcNetwork::class;
  protected $linkedVpcNetworkDataType = '';
  protected $linkedVpnTunnelsType = LinkedVpnTunnels::class;
  protected $linkedVpnTunnelsDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $reasonsType = StateReason::class;
  protected $reasonsDataType = 'array';
  /**
   * @var string
   */
  public $spokeType;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $uniqueId;
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
   * @param string
   */
  public function setGroup($group)
  {
    $this->group = $group;
  }
  /**
   * @return string
   */
  public function getGroup()
  {
    return $this->group;
  }
  /**
   * @param string
   */
  public function setHub($hub)
  {
    $this->hub = $hub;
  }
  /**
   * @return string
   */
  public function getHub()
  {
    return $this->hub;
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
   * @param LinkedInterconnectAttachments
   */
  public function setLinkedInterconnectAttachments(LinkedInterconnectAttachments $linkedInterconnectAttachments)
  {
    $this->linkedInterconnectAttachments = $linkedInterconnectAttachments;
  }
  /**
   * @return LinkedInterconnectAttachments
   */
  public function getLinkedInterconnectAttachments()
  {
    return $this->linkedInterconnectAttachments;
  }
  /**
   * @param LinkedProducerVpcNetwork
   */
  public function setLinkedProducerVpcNetwork(LinkedProducerVpcNetwork $linkedProducerVpcNetwork)
  {
    $this->linkedProducerVpcNetwork = $linkedProducerVpcNetwork;
  }
  /**
   * @return LinkedProducerVpcNetwork
   */
  public function getLinkedProducerVpcNetwork()
  {
    return $this->linkedProducerVpcNetwork;
  }
  /**
   * @param LinkedRouterApplianceInstances
   */
  public function setLinkedRouterApplianceInstances(LinkedRouterApplianceInstances $linkedRouterApplianceInstances)
  {
    $this->linkedRouterApplianceInstances = $linkedRouterApplianceInstances;
  }
  /**
   * @return LinkedRouterApplianceInstances
   */
  public function getLinkedRouterApplianceInstances()
  {
    return $this->linkedRouterApplianceInstances;
  }
  /**
   * @param LinkedVpcNetwork
   */
  public function setLinkedVpcNetwork(LinkedVpcNetwork $linkedVpcNetwork)
  {
    $this->linkedVpcNetwork = $linkedVpcNetwork;
  }
  /**
   * @return LinkedVpcNetwork
   */
  public function getLinkedVpcNetwork()
  {
    return $this->linkedVpcNetwork;
  }
  /**
   * @param LinkedVpnTunnels
   */
  public function setLinkedVpnTunnels(LinkedVpnTunnels $linkedVpnTunnels)
  {
    $this->linkedVpnTunnels = $linkedVpnTunnels;
  }
  /**
   * @return LinkedVpnTunnels
   */
  public function getLinkedVpnTunnels()
  {
    return $this->linkedVpnTunnels;
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
   * @param StateReason[]
   */
  public function setReasons($reasons)
  {
    $this->reasons = $reasons;
  }
  /**
   * @return StateReason[]
   */
  public function getReasons()
  {
    return $this->reasons;
  }
  /**
   * @param string
   */
  public function setSpokeType($spokeType)
  {
    $this->spokeType = $spokeType;
  }
  /**
   * @return string
   */
  public function getSpokeType()
  {
    return $this->spokeType;
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
  public function setUniqueId($uniqueId)
  {
    $this->uniqueId = $uniqueId;
  }
  /**
   * @return string
   */
  public function getUniqueId()
  {
    return $this->uniqueId;
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
class_alias(Spoke::class, 'Google_Service_Networkconnectivity_Spoke');
