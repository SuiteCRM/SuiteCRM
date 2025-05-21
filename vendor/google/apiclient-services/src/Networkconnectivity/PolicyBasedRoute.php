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

class PolicyBasedRoute extends \Google\Collection
{
  protected $collection_key = 'warnings';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  protected $filterType = Filter::class;
  protected $filterDataType = '';
  protected $interconnectAttachmentType = InterconnectAttachment::class;
  protected $interconnectAttachmentDataType = '';
  /**
   * @var string
   */
  public $kind;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $network;
  /**
   * @var string
   */
  public $nextHopIlbIp;
  /**
   * @var string
   */
  public $nextHopOtherRoutes;
  /**
   * @var int
   */
  public $priority;
  /**
   * @var string
   */
  public $selfLink;
  /**
   * @var string
   */
  public $updateTime;
  protected $virtualMachineType = VirtualMachine::class;
  protected $virtualMachineDataType = '';
  protected $warningsType = Warnings::class;
  protected $warningsDataType = 'array';

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
   * @param Filter
   */
  public function setFilter(Filter $filter)
  {
    $this->filter = $filter;
  }
  /**
   * @return Filter
   */
  public function getFilter()
  {
    return $this->filter;
  }
  /**
   * @param InterconnectAttachment
   */
  public function setInterconnectAttachment(InterconnectAttachment $interconnectAttachment)
  {
    $this->interconnectAttachment = $interconnectAttachment;
  }
  /**
   * @return InterconnectAttachment
   */
  public function getInterconnectAttachment()
  {
    return $this->interconnectAttachment;
  }
  /**
   * @param string
   */
  public function setKind($kind)
  {
    $this->kind = $kind;
  }
  /**
   * @return string
   */
  public function getKind()
  {
    return $this->kind;
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
  public function setNextHopIlbIp($nextHopIlbIp)
  {
    $this->nextHopIlbIp = $nextHopIlbIp;
  }
  /**
   * @return string
   */
  public function getNextHopIlbIp()
  {
    return $this->nextHopIlbIp;
  }
  /**
   * @param string
   */
  public function setNextHopOtherRoutes($nextHopOtherRoutes)
  {
    $this->nextHopOtherRoutes = $nextHopOtherRoutes;
  }
  /**
   * @return string
   */
  public function getNextHopOtherRoutes()
  {
    return $this->nextHopOtherRoutes;
  }
  /**
   * @param int
   */
  public function setPriority($priority)
  {
    $this->priority = $priority;
  }
  /**
   * @return int
   */
  public function getPriority()
  {
    return $this->priority;
  }
  /**
   * @param string
   */
  public function setSelfLink($selfLink)
  {
    $this->selfLink = $selfLink;
  }
  /**
   * @return string
   */
  public function getSelfLink()
  {
    return $this->selfLink;
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
   * @param VirtualMachine
   */
  public function setVirtualMachine(VirtualMachine $virtualMachine)
  {
    $this->virtualMachine = $virtualMachine;
  }
  /**
   * @return VirtualMachine
   */
  public function getVirtualMachine()
  {
    return $this->virtualMachine;
  }
  /**
   * @param Warnings[]
   */
  public function setWarnings($warnings)
  {
    $this->warnings = $warnings;
  }
  /**
   * @return Warnings[]
   */
  public function getWarnings()
  {
    return $this->warnings;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PolicyBasedRoute::class, 'Google_Service_Networkconnectivity_PolicyBasedRoute');
