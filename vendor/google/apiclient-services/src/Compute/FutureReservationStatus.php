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

namespace Google\Service\Compute;

class FutureReservationStatus extends \Google\Collection
{
  protected $collection_key = 'autoCreatedReservations';
  /**
   * @var string
   */
  public $amendmentStatus;
  /**
   * @var string[]
   */
  public $autoCreatedReservations;
  protected $existingMatchingUsageInfoType = FutureReservationStatusExistingMatchingUsageInfo::class;
  protected $existingMatchingUsageInfoDataType = '';
  /**
   * @var string
   */
  public $fulfilledCount;
  protected $lastKnownGoodStateType = FutureReservationStatusLastKnownGoodState::class;
  protected $lastKnownGoodStateDataType = '';
  /**
   * @var string
   */
  public $lockTime;
  /**
   * @var string
   */
  public $procurementStatus;
  protected $specificSkuPropertiesType = FutureReservationStatusSpecificSKUProperties::class;
  protected $specificSkuPropertiesDataType = '';

  /**
   * @param string
   */
  public function setAmendmentStatus($amendmentStatus)
  {
    $this->amendmentStatus = $amendmentStatus;
  }
  /**
   * @return string
   */
  public function getAmendmentStatus()
  {
    return $this->amendmentStatus;
  }
  /**
   * @param string[]
   */
  public function setAutoCreatedReservations($autoCreatedReservations)
  {
    $this->autoCreatedReservations = $autoCreatedReservations;
  }
  /**
   * @return string[]
   */
  public function getAutoCreatedReservations()
  {
    return $this->autoCreatedReservations;
  }
  /**
   * @param FutureReservationStatusExistingMatchingUsageInfo
   */
  public function setExistingMatchingUsageInfo(FutureReservationStatusExistingMatchingUsageInfo $existingMatchingUsageInfo)
  {
    $this->existingMatchingUsageInfo = $existingMatchingUsageInfo;
  }
  /**
   * @return FutureReservationStatusExistingMatchingUsageInfo
   */
  public function getExistingMatchingUsageInfo()
  {
    return $this->existingMatchingUsageInfo;
  }
  /**
   * @param string
   */
  public function setFulfilledCount($fulfilledCount)
  {
    $this->fulfilledCount = $fulfilledCount;
  }
  /**
   * @return string
   */
  public function getFulfilledCount()
  {
    return $this->fulfilledCount;
  }
  /**
   * @param FutureReservationStatusLastKnownGoodState
   */
  public function setLastKnownGoodState(FutureReservationStatusLastKnownGoodState $lastKnownGoodState)
  {
    $this->lastKnownGoodState = $lastKnownGoodState;
  }
  /**
   * @return FutureReservationStatusLastKnownGoodState
   */
  public function getLastKnownGoodState()
  {
    return $this->lastKnownGoodState;
  }
  /**
   * @param string
   */
  public function setLockTime($lockTime)
  {
    $this->lockTime = $lockTime;
  }
  /**
   * @return string
   */
  public function getLockTime()
  {
    return $this->lockTime;
  }
  /**
   * @param string
   */
  public function setProcurementStatus($procurementStatus)
  {
    $this->procurementStatus = $procurementStatus;
  }
  /**
   * @return string
   */
  public function getProcurementStatus()
  {
    return $this->procurementStatus;
  }
  /**
   * @param FutureReservationStatusSpecificSKUProperties
   */
  public function setSpecificSkuProperties(FutureReservationStatusSpecificSKUProperties $specificSkuProperties)
  {
    $this->specificSkuProperties = $specificSkuProperties;
  }
  /**
   * @return FutureReservationStatusSpecificSKUProperties
   */
  public function getSpecificSkuProperties()
  {
    return $this->specificSkuProperties;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(FutureReservationStatus::class, 'Google_Service_Compute_FutureReservationStatus');
