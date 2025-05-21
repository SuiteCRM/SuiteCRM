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

namespace Google\Service\MigrationCenterAPI;

class ReportSummaryVmwareEngineFinding extends \Google\Collection
{
  protected $collection_key = 'nodeAllocations';
  /**
   * @var string
   */
  public $allocatedAssetCount;
  /**
   * @var string[]
   */
  public $allocatedRegions;
  protected $nodeAllocationsType = ReportSummaryVmwareNodeAllocation::class;
  protected $nodeAllocationsDataType = 'array';

  /**
   * @param string
   */
  public function setAllocatedAssetCount($allocatedAssetCount)
  {
    $this->allocatedAssetCount = $allocatedAssetCount;
  }
  /**
   * @return string
   */
  public function getAllocatedAssetCount()
  {
    return $this->allocatedAssetCount;
  }
  /**
   * @param string[]
   */
  public function setAllocatedRegions($allocatedRegions)
  {
    $this->allocatedRegions = $allocatedRegions;
  }
  /**
   * @return string[]
   */
  public function getAllocatedRegions()
  {
    return $this->allocatedRegions;
  }
  /**
   * @param ReportSummaryVmwareNodeAllocation[]
   */
  public function setNodeAllocations($nodeAllocations)
  {
    $this->nodeAllocations = $nodeAllocations;
  }
  /**
   * @return ReportSummaryVmwareNodeAllocation[]
   */
  public function getNodeAllocations()
  {
    return $this->nodeAllocations;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ReportSummaryVmwareEngineFinding::class, 'Google_Service_MigrationCenterAPI_ReportSummaryVmwareEngineFinding');
