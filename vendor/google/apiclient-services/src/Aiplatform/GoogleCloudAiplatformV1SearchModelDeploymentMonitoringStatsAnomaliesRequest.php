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

class GoogleCloudAiplatformV1SearchModelDeploymentMonitoringStatsAnomaliesRequest extends \Google\Collection
{
  protected $collection_key = 'objectives';
  /**
   * @var string
   */
  public $deployedModelId;
  /**
   * @var string
   */
  public $endTime;
  /**
   * @var string
   */
  public $featureDisplayName;
  protected $objectivesType = GoogleCloudAiplatformV1SearchModelDeploymentMonitoringStatsAnomaliesRequestStatsAnomaliesObjective::class;
  protected $objectivesDataType = 'array';
  /**
   * @var int
   */
  public $pageSize;
  /**
   * @var string
   */
  public $pageToken;
  /**
   * @var string
   */
  public $startTime;

  /**
   * @param string
   */
  public function setDeployedModelId($deployedModelId)
  {
    $this->deployedModelId = $deployedModelId;
  }
  /**
   * @return string
   */
  public function getDeployedModelId()
  {
    return $this->deployedModelId;
  }
  /**
   * @param string
   */
  public function setEndTime($endTime)
  {
    $this->endTime = $endTime;
  }
  /**
   * @return string
   */
  public function getEndTime()
  {
    return $this->endTime;
  }
  /**
   * @param string
   */
  public function setFeatureDisplayName($featureDisplayName)
  {
    $this->featureDisplayName = $featureDisplayName;
  }
  /**
   * @return string
   */
  public function getFeatureDisplayName()
  {
    return $this->featureDisplayName;
  }
  /**
   * @param GoogleCloudAiplatformV1SearchModelDeploymentMonitoringStatsAnomaliesRequestStatsAnomaliesObjective[]
   */
  public function setObjectives($objectives)
  {
    $this->objectives = $objectives;
  }
  /**
   * @return GoogleCloudAiplatformV1SearchModelDeploymentMonitoringStatsAnomaliesRequestStatsAnomaliesObjective[]
   */
  public function getObjectives()
  {
    return $this->objectives;
  }
  /**
   * @param int
   */
  public function setPageSize($pageSize)
  {
    $this->pageSize = $pageSize;
  }
  /**
   * @return int
   */
  public function getPageSize()
  {
    return $this->pageSize;
  }
  /**
   * @param string
   */
  public function setPageToken($pageToken)
  {
    $this->pageToken = $pageToken;
  }
  /**
   * @return string
   */
  public function getPageToken()
  {
    return $this->pageToken;
  }
  /**
   * @param string
   */
  public function setStartTime($startTime)
  {
    $this->startTime = $startTime;
  }
  /**
   * @return string
   */
  public function getStartTime()
  {
    return $this->startTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SearchModelDeploymentMonitoringStatsAnomaliesRequest::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SearchModelDeploymentMonitoringStatsAnomaliesRequest');
