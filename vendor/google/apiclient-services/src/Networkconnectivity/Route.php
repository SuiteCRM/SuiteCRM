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

class Route extends \Google\Model
{
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
  public $ipCidrRange;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $location;
  /**
   * @var string
   */
  public $name;
  protected $nextHopInterconnectAttachmentType = NextHopInterconnectAttachment::class;
  protected $nextHopInterconnectAttachmentDataType = '';
  protected $nextHopRouterApplianceInstanceType = NextHopRouterApplianceInstance::class;
  protected $nextHopRouterApplianceInstanceDataType = '';
  protected $nextHopVpcNetworkType = NextHopVpcNetwork::class;
  protected $nextHopVpcNetworkDataType = '';
  protected $nextHopVpnTunnelType = NextHopVPNTunnel::class;
  protected $nextHopVpnTunnelDataType = '';
  /**
   * @var string
   */
  public $priority;
  /**
   * @var string
   */
  public $spoke;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $type;
  /**
   * @var string
   */
  public $uid;
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
  public function setIpCidrRange($ipCidrRange)
  {
    $this->ipCidrRange = $ipCidrRange;
  }
  /**
   * @return string
   */
  public function getIpCidrRange()
  {
    return $this->ipCidrRange;
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
  public function setLocation($location)
  {
    $this->location = $location;
  }
  /**
   * @return string
   */
  public function getLocation()
  {
    return $this->location;
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
   * @param NextHopInterconnectAttachment
   */
  public function setNextHopInterconnectAttachment(NextHopInterconnectAttachment $nextHopInterconnectAttachment)
  {
    $this->nextHopInterconnectAttachment = $nextHopInterconnectAttachment;
  }
  /**
   * @return NextHopInterconnectAttachment
   */
  public function getNextHopInterconnectAttachment()
  {
    return $this->nextHopInterconnectAttachment;
  }
  /**
   * @param NextHopRouterApplianceInstance
   */
  public function setNextHopRouterApplianceInstance(NextHopRouterApplianceInstance $nextHopRouterApplianceInstance)
  {
    $this->nextHopRouterApplianceInstance = $nextHopRouterApplianceInstance;
  }
  /**
   * @return NextHopRouterApplianceInstance
   */
  public function getNextHopRouterApplianceInstance()
  {
    return $this->nextHopRouterApplianceInstance;
  }
  /**
   * @param NextHopVpcNetwork
   */
  public function setNextHopVpcNetwork(NextHopVpcNetwork $nextHopVpcNetwork)
  {
    $this->nextHopVpcNetwork = $nextHopVpcNetwork;
  }
  /**
   * @return NextHopVpcNetwork
   */
  public function getNextHopVpcNetwork()
  {
    return $this->nextHopVpcNetwork;
  }
  /**
   * @param NextHopVPNTunnel
   */
  public function setNextHopVpnTunnel(NextHopVPNTunnel $nextHopVpnTunnel)
  {
    $this->nextHopVpnTunnel = $nextHopVpnTunnel;
  }
  /**
   * @return NextHopVPNTunnel
   */
  public function getNextHopVpnTunnel()
  {
    return $this->nextHopVpnTunnel;
  }
  /**
   * @param string
   */
  public function setPriority($priority)
  {
    $this->priority = $priority;
  }
  /**
   * @return string
   */
  public function getPriority()
  {
    return $this->priority;
  }
  /**
   * @param string
   */
  public function setSpoke($spoke)
  {
    $this->spoke = $spoke;
  }
  /**
   * @return string
   */
  public function getSpoke()
  {
    return $this->spoke;
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
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
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
class_alias(Route::class, 'Google_Service_Networkconnectivity_Route');
