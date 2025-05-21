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

class GoogleCloudContactcenterinsightsV1QueryMetricsRequest extends \Google\Collection
{
  protected $collection_key = 'dimensions';
  protected $dimensionsType = GoogleCloudContactcenterinsightsV1Dimension::class;
  protected $dimensionsDataType = 'array';
  /**
   * @var string
   */
  public $filter;
  /**
   * @var string
   */
  public $measureMask;
  /**
   * @var string
   */
  public $timeGranularity;

  /**
   * @param GoogleCloudContactcenterinsightsV1Dimension[]
   */
  public function setDimensions($dimensions)
  {
    $this->dimensions = $dimensions;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1Dimension[]
   */
  public function getDimensions()
  {
    return $this->dimensions;
  }
  /**
   * @param string
   */
  public function setFilter($filter)
  {
    $this->filter = $filter;
  }
  /**
   * @return string
   */
  public function getFilter()
  {
    return $this->filter;
  }
  /**
   * @param string
   */
  public function setMeasureMask($measureMask)
  {
    $this->measureMask = $measureMask;
  }
  /**
   * @return string
   */
  public function getMeasureMask()
  {
    return $this->measureMask;
  }
  /**
   * @param string
   */
  public function setTimeGranularity($timeGranularity)
  {
    $this->timeGranularity = $timeGranularity;
  }
  /**
   * @return string
   */
  public function getTimeGranularity()
  {
    return $this->timeGranularity;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1QueryMetricsRequest::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1QueryMetricsRequest');
