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

class QueuedResourceState extends \Google\Model
{
  protected $acceptedDataType = AcceptedData::class;
  protected $acceptedDataDataType = '';
  protected $activeDataType = ActiveData::class;
  protected $activeDataDataType = '';
  protected $creatingDataType = CreatingData::class;
  protected $creatingDataDataType = '';
  protected $deletingDataType = DeletingData::class;
  protected $deletingDataDataType = '';
  protected $failedDataType = FailedData::class;
  protected $failedDataDataType = '';
  protected $provisioningDataType = ProvisioningData::class;
  protected $provisioningDataDataType = '';
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $stateInitiator;
  protected $suspendedDataType = SuspendedData::class;
  protected $suspendedDataDataType = '';
  protected $suspendingDataType = SuspendingData::class;
  protected $suspendingDataDataType = '';

  /**
   * @param AcceptedData
   */
  public function setAcceptedData(AcceptedData $acceptedData)
  {
    $this->acceptedData = $acceptedData;
  }
  /**
   * @return AcceptedData
   */
  public function getAcceptedData()
  {
    return $this->acceptedData;
  }
  /**
   * @param ActiveData
   */
  public function setActiveData(ActiveData $activeData)
  {
    $this->activeData = $activeData;
  }
  /**
   * @return ActiveData
   */
  public function getActiveData()
  {
    return $this->activeData;
  }
  /**
   * @param CreatingData
   */
  public function setCreatingData(CreatingData $creatingData)
  {
    $this->creatingData = $creatingData;
  }
  /**
   * @return CreatingData
   */
  public function getCreatingData()
  {
    return $this->creatingData;
  }
  /**
   * @param DeletingData
   */
  public function setDeletingData(DeletingData $deletingData)
  {
    $this->deletingData = $deletingData;
  }
  /**
   * @return DeletingData
   */
  public function getDeletingData()
  {
    return $this->deletingData;
  }
  /**
   * @param FailedData
   */
  public function setFailedData(FailedData $failedData)
  {
    $this->failedData = $failedData;
  }
  /**
   * @return FailedData
   */
  public function getFailedData()
  {
    return $this->failedData;
  }
  /**
   * @param ProvisioningData
   */
  public function setProvisioningData(ProvisioningData $provisioningData)
  {
    $this->provisioningData = $provisioningData;
  }
  /**
   * @return ProvisioningData
   */
  public function getProvisioningData()
  {
    return $this->provisioningData;
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
  public function setStateInitiator($stateInitiator)
  {
    $this->stateInitiator = $stateInitiator;
  }
  /**
   * @return string
   */
  public function getStateInitiator()
  {
    return $this->stateInitiator;
  }
  /**
   * @param SuspendedData
   */
  public function setSuspendedData(SuspendedData $suspendedData)
  {
    $this->suspendedData = $suspendedData;
  }
  /**
   * @return SuspendedData
   */
  public function getSuspendedData()
  {
    return $this->suspendedData;
  }
  /**
   * @param SuspendingData
   */
  public function setSuspendingData(SuspendingData $suspendingData)
  {
    $this->suspendingData = $suspendingData;
  }
  /**
   * @return SuspendingData
   */
  public function getSuspendingData()
  {
    return $this->suspendingData;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(QueuedResourceState::class, 'Google_Service_TPU_QueuedResourceState');
