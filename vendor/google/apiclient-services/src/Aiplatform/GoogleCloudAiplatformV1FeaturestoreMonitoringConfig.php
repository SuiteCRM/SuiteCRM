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

class GoogleCloudAiplatformV1FeaturestoreMonitoringConfig extends \Google\Model
{
  protected $categoricalThresholdConfigType = GoogleCloudAiplatformV1FeaturestoreMonitoringConfigThresholdConfig::class;
  protected $categoricalThresholdConfigDataType = '';
  protected $importFeaturesAnalysisType = GoogleCloudAiplatformV1FeaturestoreMonitoringConfigImportFeaturesAnalysis::class;
  protected $importFeaturesAnalysisDataType = '';
  protected $numericalThresholdConfigType = GoogleCloudAiplatformV1FeaturestoreMonitoringConfigThresholdConfig::class;
  protected $numericalThresholdConfigDataType = '';
  protected $snapshotAnalysisType = GoogleCloudAiplatformV1FeaturestoreMonitoringConfigSnapshotAnalysis::class;
  protected $snapshotAnalysisDataType = '';

  /**
   * @param GoogleCloudAiplatformV1FeaturestoreMonitoringConfigThresholdConfig
   */
  public function setCategoricalThresholdConfig(GoogleCloudAiplatformV1FeaturestoreMonitoringConfigThresholdConfig $categoricalThresholdConfig)
  {
    $this->categoricalThresholdConfig = $categoricalThresholdConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1FeaturestoreMonitoringConfigThresholdConfig
   */
  public function getCategoricalThresholdConfig()
  {
    return $this->categoricalThresholdConfig;
  }
  /**
   * @param GoogleCloudAiplatformV1FeaturestoreMonitoringConfigImportFeaturesAnalysis
   */
  public function setImportFeaturesAnalysis(GoogleCloudAiplatformV1FeaturestoreMonitoringConfigImportFeaturesAnalysis $importFeaturesAnalysis)
  {
    $this->importFeaturesAnalysis = $importFeaturesAnalysis;
  }
  /**
   * @return GoogleCloudAiplatformV1FeaturestoreMonitoringConfigImportFeaturesAnalysis
   */
  public function getImportFeaturesAnalysis()
  {
    return $this->importFeaturesAnalysis;
  }
  /**
   * @param GoogleCloudAiplatformV1FeaturestoreMonitoringConfigThresholdConfig
   */
  public function setNumericalThresholdConfig(GoogleCloudAiplatformV1FeaturestoreMonitoringConfigThresholdConfig $numericalThresholdConfig)
  {
    $this->numericalThresholdConfig = $numericalThresholdConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1FeaturestoreMonitoringConfigThresholdConfig
   */
  public function getNumericalThresholdConfig()
  {
    return $this->numericalThresholdConfig;
  }
  /**
   * @param GoogleCloudAiplatformV1FeaturestoreMonitoringConfigSnapshotAnalysis
   */
  public function setSnapshotAnalysis(GoogleCloudAiplatformV1FeaturestoreMonitoringConfigSnapshotAnalysis $snapshotAnalysis)
  {
    $this->snapshotAnalysis = $snapshotAnalysis;
  }
  /**
   * @return GoogleCloudAiplatformV1FeaturestoreMonitoringConfigSnapshotAnalysis
   */
  public function getSnapshotAnalysis()
  {
    return $this->snapshotAnalysis;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1FeaturestoreMonitoringConfig::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1FeaturestoreMonitoringConfig');
