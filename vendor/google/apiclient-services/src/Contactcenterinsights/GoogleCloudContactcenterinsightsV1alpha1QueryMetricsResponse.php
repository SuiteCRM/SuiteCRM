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

namespace Google\Service\Contactcenterinsights;

class GoogleCloudContactcenterinsightsV1alpha1QueryMetricsResponse extends \Google\Collection
{
  protected $collection_key = 'slices';
  /**
   * @var string
   */
  public $location;
  protected $macroAverageSliceType = GoogleCloudContactcenterinsightsV1alpha1QueryMetricsResponseSlice::class;
  protected $macroAverageSliceDataType = '';
  protected $slicesType = GoogleCloudContactcenterinsightsV1alpha1QueryMetricsResponseSlice::class;
  protected $slicesDataType = 'array';
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string
   */
  public function setLocation($location)
  {
    $this->location = $location;
  }
  /**
   * @return string
   */
  public function getLocation()
  {
    return $this->location;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1alpha1QueryMetricsResponseSlice
   */
  public function setMacroAverageSlice(GoogleCloudContactcenterinsightsV1alpha1QueryMetricsResponseSlice $macroAverageSlice)
  {
    $this->macroAverageSlice = $macroAverageSlice;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1alpha1QueryMetricsResponseSlice
   */
  public function getMacroAverageSlice()
  {
    return $this->macroAverageSlice;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1alpha1QueryMetricsResponseSlice[]
   */
  public function setSlices($slices)
  {
    $this->slices = $slices;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1alpha1QueryMetricsResponseSlice[]
   */
  public function getSlices()
  {
    return $this->slices;
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
class_alias(GoogleCloudContactcenterinsightsV1alpha1QueryMetricsResponse::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1alpha1QueryMetricsResponse');
