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

class AllocationAggregateReservation extends \Google\Collection
{
  protected $collection_key = 'reservedResources';
  protected $inUseResourcesType = AllocationAggregateReservationReservedResourceInfo::class;
  protected $inUseResourcesDataType = 'array';
  protected $reservedResourcesType = AllocationAggregateReservationReservedResourceInfo::class;
  protected $reservedResourcesDataType = 'array';
  /**
   * @var string
   */
  public $vmFamily;
  /**
   * @var string
   */
  public $workloadType;

  /**
   * @param AllocationAggregateReservationReservedResourceInfo[]
   */
  public function setInUseResources($inUseResources)
  {
    $this->inUseResources = $inUseResources;
  }
  /**
   * @return AllocationAggregateReservationReservedResourceInfo[]
   */
  public function getInUseResources()
  {
    return $this->inUseResources;
  }
  /**
   * @param AllocationAggregateReservationReservedResourceInfo[]
   */
  public function setReservedResources($reservedResources)
  {
    $this->reservedResources = $reservedResources;
  }
  /**
   * @return AllocationAggregateReservationReservedResourceInfo[]
   */
  public function getReservedResources()
  {
    return $this->reservedResources;
  }
  /**
   * @param string
   */
  public function setVmFamily($vmFamily)
  {
    $this->vmFamily = $vmFamily;
  }
  /**
   * @return string
   */
  public function getVmFamily()
  {
    return $this->vmFamily;
  }
  /**
   * @param string
   */
  public function setWorkloadType($workloadType)
  {
    $this->workloadType = $workloadType;
  }
  /**
   * @return string
   */
  public function getWorkloadType()
  {
    return $this->workloadType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AllocationAggregateReservation::class, 'Google_Service_Compute_AllocationAggregateReservation');
