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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1MachineSpec extends \Google\Model
{
  /**
   * @var int
   */
  public $acceleratorCount;
  /**
   * @var string
   */
  public $acceleratorType;
  /**
   * @var string
   */
  public $machineType;
  /**
   * @var int
   */
  public $multihostGpuNodeCount;
  protected $reservationAffinityType = GoogleCloudAiplatformV1ReservationAffinity::class;
  protected $reservationAffinityDataType = '';
  /**
   * @var string
   */
  public $tpuTopology;

  /**
   * @param int
   */
  public function setAcceleratorCount($acceleratorCount)
  {
    $this->acceleratorCount = $acceleratorCount;
  }
  /**
   * @return int
   */
  public function getAcceleratorCount()
  {
    return $this->acceleratorCount;
  }
  /**
   * @param string
   */
  public function setAcceleratorType($acceleratorType)
  {
    $this->acceleratorType = $acceleratorType;
  }
  /**
   * @return string
   */
  public function getAcceleratorType()
  {
    return $this->acceleratorType;
  }
  /**
   * @param string
   */
  public function setMachineType($machineType)
  {
    $this->machineType = $machineType;
  }
  /**
   * @return string
   */
  public function getMachineType()
  {
    return $this->machineType;
  }
  /**
   * @param int
   */
  public function setMultihostGpuNodeCount($multihostGpuNodeCount)
  {
    $this->multihostGpuNodeCount = $multihostGpuNodeCount;
  }
  /**
   * @return int
   */
  public function getMultihostGpuNodeCount()
  {
    return $this->multihostGpuNodeCount;
  }
  /**
   * @param GoogleCloudAiplatformV1ReservationAffinity
   */
  public function setReservationAffinity(GoogleCloudAiplatformV1ReservationAffinity $reservationAffinity)
  {
    $this->reservationAffinity = $reservationAffinity;
  }
  /**
   * @return GoogleCloudAiplatformV1ReservationAffinity
   */
  public function getReservationAffinity()
  {
    return $this->reservationAffinity;
  }
  /**
   * @param string
   */
  public function setTpuTopology($tpuTopology)
  {
    $this->tpuTopology = $tpuTopology;
  }
  /**
   * @return string
   */
  public function getTpuTopology()
  {
    return $this->tpuTopology;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1MachineSpec::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1MachineSpec');
