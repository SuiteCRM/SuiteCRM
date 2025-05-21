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

class GoogleCloudAiplatformV1ModelMonitoringStatsAnomalies extends \Google\Collection
{
  protected $collection_key = 'featureStats';
  /**
   * @var int
   */
  public $anomalyCount;
  /**
   * @var string
   */
  public $deployedModelId;
  protected $featureStatsType = GoogleCloudAiplatformV1ModelMonitoringStatsAnomaliesFeatureHistoricStatsAnomalies::class;
  protected $featureStatsDataType = 'array';
  /**
   * @var string
   */
  public $objective;

  /**
   * @param int
   */
  public function setAnomalyCount($anomalyCount)
  {
    $this->anomalyCount = $anomalyCount;
  }
  /**
   * @return int
   */
  public function getAnomalyCount()
  {
    return $this->anomalyCount;
  }
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
   * @param GoogleCloudAiplatformV1ModelMonitoringStatsAnomaliesFeatureHistoricStatsAnomalies[]
   */
  public function setFeatureStats($featureStats)
  {
    $this->featureStats = $featureStats;
  }
  /**
   * @return GoogleCloudAiplatformV1ModelMonitoringStatsAnomaliesFeatureHistoricStatsAnomalies[]
   */
  public function getFeatureStats()
  {
    return $this->featureStats;
  }
  /**
   * @param string
   */
  public function setObjective($objective)
  {
    $this->objective = $objective;
  }
  /**
   * @return string
   */
  public function getObjective()
  {
    return $this->objective;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ModelMonitoringStatsAnomalies::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ModelMonitoringStatsAnomalies');
