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

namespace Google\Service\VMwareEngine;

class ExternalAccessRule extends \Google\Collection
{
  protected $collection_key = 'sourcePorts';
  /**
   * @var string
   */
  public $action;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  protected $destinationIpRangesType = IpRange::class;
  protected $destinationIpRangesDataType = 'array';
  /**
   * @var string[]
   */
  public $destinationPorts;
  /**
   * @var string
   */
  public $ipProtocol;
  /**
   * @var string
   */
  public $name;
  /**
   * @var int
   */
  public $priority;
  protected $sourceIpRangesType = IpRange::class;
  protected $sourceIpRangesDataType = 'array';
  /**
   * @var string[]
   */
  public $sourcePorts;
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

  /**
   * @param string
   */
  public function setAction($action)
  {
    $this->action = $action;
  }
  /**
   * @return string
   */
  public function getAction()
  {
    return $this->action;
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
   * @param IpRange[]
   */
  public function setDestinationIpRanges($destinationIpRanges)
  {
    $this->destinationIpRanges = $destinationIpRanges;
  }
  /**
   * @return IpRange[]
   */
  public function getDestinationIpRanges()
  {
    return $this->destinationIpRanges;
  }
  /**
   * @param string[]
   */
  public function setDestinationPorts($destinationPorts)
  {
    $this->destinationPorts = $destinationPorts;
  }
  /**
   * @return string[]
   */
  public function getDestinationPorts()
  {
    return $this->destinationPorts;
  }
  /**
   * @param string
   */
  public function setIpProtocol($ipProtocol)
  {
    $this->ipProtocol = $ipProtocol;
  }
  /**
   * @return string
   */
  public function getIpProtocol()
  {
    return $this->ipProtocol;
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
   * @param IpRange[]
   */
  public function setSourceIpRanges($sourceIpRanges)
  {
    $this->sourceIpRanges = $sourceIpRanges;
  }
  /**
   * @return IpRange[]
   */
  public function getSourceIpRanges()
  {
    return $this->sourceIpRanges;
  }
  /**
   * @param string[]
   */
  public function setSourcePorts($sourcePorts)
  {
    $this->sourcePorts = $sourcePorts;
  }
  /**
   * @return string[]
   */
  public function getSourcePorts()
  {
    return $this->sourcePorts;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ExternalAccessRule::class, 'Google_Service_VMwareEngine_ExternalAccessRule');
