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

class FutureReservationSpecificSKUProperties extends \Google\Model
{
  protected $instancePropertiesType = AllocationSpecificSKUAllocationReservedInstanceProperties::class;
  protected $instancePropertiesDataType = '';
  /**
   * @var string
   */
  public $sourceInstanceTemplate;
  /**
   * @var string
   */
  public $totalCount;

  /**
   * @param AllocationSpecificSKUAllocationReservedInstanceProperties
   */
  public function setInstanceProperties(AllocationSpecificSKUAllocationReservedInstanceProperties $instanceProperties)
  {
    $this->instanceProperties = $instanceProperties;
  }
  /**
   * @return AllocationSpecificSKUAllocationReservedInstanceProperties
   */
  public function getInstanceProperties()
  {
    return $this->instanceProperties;
  }
  /**
   * @param string
   */
  public function setSourceInstanceTemplate($sourceInstanceTemplate)
  {
    $this->sourceInstanceTemplate = $sourceInstanceTemplate;
  }
  /**
   * @return string
   */
  public function getSourceInstanceTemplate()
  {
    return $this->sourceInstanceTemplate;
  }
  /**
   * @param string
   */
  public function setTotalCount($totalCount)
  {
    $this->totalCount = $totalCount;
  }
  /**
   * @return string
   */
  public function getTotalCount()
  {
    return $this->totalCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(FutureReservationSpecificSKUProperties::class, 'Google_Service_Compute_FutureReservationSpecificSKUProperties');
