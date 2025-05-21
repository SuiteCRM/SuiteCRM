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

class FutureReservationStatusLastKnownGoodState extends \Google\Model
{
  /**
   * @var string
   */
  public $description;
  protected $existingMatchingUsageInfoType = FutureReservationStatusExistingMatchingUsageInfo::class;
  protected $existingMatchingUsageInfoDataType = '';
  protected $futureReservationSpecsType = FutureReservationStatusLastKnownGoodStateFutureReservationSpecs::class;
  protected $futureReservationSpecsDataType = '';
  /**
   * @var string
   */
  public $lockTime;
  /**
   * @var string
   */
  public $namePrefix;
  /**
   * @var string
   */
  public $procurementStatus;

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
   * @param FutureReservationStatusLastKnownGoodStateFutureReservationSpecs
   */
  public function setFutureReservationSpecs(FutureReservationStatusLastKnownGoodStateFutureReservationSpecs $futureReservationSpecs)
  {
    $this->futureReservationSpecs = $futureReservationSpecs;
  }
  /**
   * @return FutureReservationStatusLastKnownGoodStateFutureReservationSpecs
   */
  public function getFutureReservationSpecs()
  {
    return $this->futureReservationSpecs;
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
  public function setNamePrefix($namePrefix)
  {
    $this->namePrefix = $namePrefix;
  }
  /**
   * @return string
   */
  public function getNamePrefix()
  {
    return $this->namePrefix;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(FutureReservationStatusLastKnownGoodState::class, 'Google_Service_Compute_FutureReservationStatusLastKnownGoodState');
