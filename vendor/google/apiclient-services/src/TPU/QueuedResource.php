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

class QueuedResource extends \Google\Model
{
  /**
   * @var string
   */
  public $createTime;
  protected $guaranteedType = Guaranteed::class;
  protected $guaranteedDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $queueingPolicyType = QueueingPolicy::class;
  protected $queueingPolicyDataType = '';
  /**
   * @var string
   */
  public $reservationName;
  protected $spotType = Spot::class;
  protected $spotDataType = '';
  protected $stateType = QueuedResourceState::class;
  protected $stateDataType = '';
  protected $tpuType = Tpu::class;
  protected $tpuDataType = '';

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
   * @param Guaranteed
   */
  public function setGuaranteed(Guaranteed $guaranteed)
  {
    $this->guaranteed = $guaranteed;
  }
  /**
   * @return Guaranteed
   */
  public function getGuaranteed()
  {
    return $this->guaranteed;
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
   * @param QueueingPolicy
   */
  public function setQueueingPolicy(QueueingPolicy $queueingPolicy)
  {
    $this->queueingPolicy = $queueingPolicy;
  }
  /**
   * @return QueueingPolicy
   */
  public function getQueueingPolicy()
  {
    return $this->queueingPolicy;
  }
  /**
   * @param string
   */
  public function setReservationName($reservationName)
  {
    $this->reservationName = $reservationName;
  }
  /**
   * @return string
   */
  public function getReservationName()
  {
    return $this->reservationName;
  }
  /**
   * @param Spot
   */
  public function setSpot(Spot $spot)
  {
    $this->spot = $spot;
  }
  /**
   * @return Spot
   */
  public function getSpot()
  {
    return $this->spot;
  }
  /**
   * @param QueuedResourceState
   */
  public function setState(QueuedResourceState $state)
  {
    $this->state = $state;
  }
  /**
   * @return QueuedResourceState
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param Tpu
   */
  public function setTpu(Tpu $tpu)
  {
    $this->tpu = $tpu;
  }
  /**
   * @return Tpu
   */
  public function getTpu()
  {
    return $this->tpu;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(QueuedResource::class, 'Google_Service_TPU_QueuedResource');
