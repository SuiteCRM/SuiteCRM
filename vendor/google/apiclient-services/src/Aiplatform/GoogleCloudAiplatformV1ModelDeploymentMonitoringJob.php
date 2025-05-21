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

class GoogleCloudAiplatformV1ModelDeploymentMonitoringJob extends \Google\Collection
{
  protected $collection_key = 'modelDeploymentMonitoringObjectiveConfigs';
  /**
   * @var string
   */
  public $analysisInstanceSchemaUri;
  protected $bigqueryTablesType = GoogleCloudAiplatformV1ModelDeploymentMonitoringBigQueryTable::class;
  protected $bigqueryTablesDataType = 'array';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var bool
   */
  public $enableMonitoringPipelineLogs;
  protected $encryptionSpecType = GoogleCloudAiplatformV1EncryptionSpec::class;
  protected $encryptionSpecDataType = '';
  /**
   * @var string
   */
  public $endpoint;
  protected $errorType = GoogleRpcStatus::class;
  protected $errorDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  protected $latestMonitoringPipelineMetadataType = GoogleCloudAiplatformV1ModelDeploymentMonitoringJobLatestMonitoringPipelineMetadata::class;
  protected $latestMonitoringPipelineMetadataDataType = '';
  /**
   * @var string
   */
  public $logTtl;
  protected $loggingSamplingStrategyType = GoogleCloudAiplatformV1SamplingStrategy::class;
  protected $loggingSamplingStrategyDataType = '';
  protected $modelDeploymentMonitoringObjectiveConfigsType = GoogleCloudAiplatformV1ModelDeploymentMonitoringObjectiveConfig::class;
  protected $modelDeploymentMonitoringObjectiveConfigsDataType = 'array';
  protected $modelDeploymentMonitoringScheduleConfigType = GoogleCloudAiplatformV1ModelDeploymentMonitoringScheduleConfig::class;
  protected $modelDeploymentMonitoringScheduleConfigDataType = '';
  protected $modelMonitoringAlertConfigType = GoogleCloudAiplatformV1ModelMonitoringAlertConfig::class;
  protected $modelMonitoringAlertConfigDataType = '';
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $nextScheduleTime;
  /**
   * @var string
   */
  public $predictInstanceSchemaUri;
  /**
   * @var array
   */
  public $samplePredictInstance;
  /**
   * @var bool
   */
  public $satisfiesPzi;
  /**
   * @var bool
   */
  public $satisfiesPzs;
  /**
   * @var string
   */
  public $scheduleState;
  /**
   * @var string
   */
  public $state;
  protected $statsAnomaliesBaseDirectoryType = GoogleCloudAiplatformV1GcsDestination::class;
  protected $statsAnomaliesBaseDirectoryDataType = '';
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string
   */
  public function setAnalysisInstanceSchemaUri($analysisInstanceSchemaUri)
  {
    $this->analysisInstanceSchemaUri = $analysisInstanceSchemaUri;
  }
  /**
   * @return string
   */
  public function getAnalysisInstanceSchemaUri()
  {
    return $this->analysisInstanceSchemaUri;
  }
  /**
   * @param GoogleCloudAiplatformV1ModelDeploymentMonitoringBigQueryTable[]
   */
  public function setBigqueryTables($bigqueryTables)
  {
    $this->bigqueryTables = $bigqueryTables;
  }
  /**
   * @return GoogleCloudAiplatformV1ModelDeploymentMonitoringBigQueryTable[]
   */
  public function getBigqueryTables()
  {
    return $this->bigqueryTables;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param bool
   */
  public function setEnableMonitoringPipelineLogs($enableMonitoringPipelineLogs)
  {
    $this->enableMonitoringPipelineLogs = $enableMonitoringPipelineLogs;
  }
  /**
   * @return bool
   */
  public function getEnableMonitoringPipelineLogs()
  {
    return $this->enableMonitoringPipelineLogs;
  }
  /**
   * @param GoogleCloudAiplatformV1EncryptionSpec
   */
  public function setEncryptionSpec(GoogleCloudAiplatformV1EncryptionSpec $encryptionSpec)
  {
    $this->encryptionSpec = $encryptionSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1EncryptionSpec
   */
  public function getEncryptionSpec()
  {
    return $this->encryptionSpec;
  }
  /**
   * @param string
   */
  public function setEndpoint($endpoint)
  {
    $this->endpoint = $endpoint;
  }
  /**
   * @return string
   */
  public function getEndpoint()
  {
    return $this->endpoint;
  }
  /**
   * @param GoogleRpcStatus
   */
  public function setError(GoogleRpcStatus $error)
  {
    $this->error = $error;
  }
  /**
   * @return GoogleRpcStatus
   */
  public function getError()
  {
    return $this->error;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
  }
  /**
   * @param GoogleCloudAiplatformV1ModelDeploymentMonitoringJobLatestMonitoringPipelineMetadata
   */
  public function setLatestMonitoringPipelineMetadata(GoogleCloudAiplatformV1ModelDeploymentMonitoringJobLatestMonitoringPipelineMetadata $latestMonitoringPipelineMetadata)
  {
    $this->latestMonitoringPipelineMetadata = $latestMonitoringPipelineMetadata;
  }
  /**
   * @return GoogleCloudAiplatformV1ModelDeploymentMonitoringJobLatestMonitoringPipelineMetadata
   */
  public function getLatestMonitoringPipelineMetadata()
  {
    return $this->latestMonitoringPipelineMetadata;
  }
  /**
   * @param string
   */
  public function setLogTtl($logTtl)
  {
    $this->logTtl = $logTtl;
  }
  /**
   * @return string
   */
  public function getLogTtl()
  {
    return $this->logTtl;
  }
  /**
   * @param GoogleCloudAiplatformV1SamplingStrategy
   */
  public function setLoggingSamplingStrategy(GoogleCloudAiplatformV1SamplingStrategy $loggingSamplingStrategy)
  {
    $this->loggingSamplingStrategy = $loggingSamplingStrategy;
  }
  /**
   * @return GoogleCloudAiplatformV1SamplingStrategy
   */
  public function getLoggingSamplingStrategy()
  {
    return $this->loggingSamplingStrategy;
  }
  /**
   * @param GoogleCloudAiplatformV1ModelDeploymentMonitoringObjectiveConfig[]
   */
  public function setModelDeploymentMonitoringObjectiveConfigs($modelDeploymentMonitoringObjectiveConfigs)
  {
    $this->modelDeploymentMonitoringObjectiveConfigs = $modelDeploymentMonitoringObjectiveConfigs;
  }
  /**
   * @return GoogleCloudAiplatformV1ModelDeploymentMonitoringObjectiveConfig[]
   */
  public function getModelDeploymentMonitoringObjectiveConfigs()
  {
    return $this->modelDeploymentMonitoringObjectiveConfigs;
  }
  /**
   * @param GoogleCloudAiplatformV1ModelDeploymentMonitoringScheduleConfig
   */
  public function setModelDeploymentMonitoringScheduleConfig(GoogleCloudAiplatformV1ModelDeploymentMonitoringScheduleConfig $modelDeploymentMonitoringScheduleConfig)
  {
    $this->modelDeploymentMonitoringScheduleConfig = $modelDeploymentMonitoringScheduleConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1ModelDeploymentMonitoringScheduleConfig
   */
  public function getModelDeploymentMonitoringScheduleConfig()
  {
    return $this->modelDeploymentMonitoringScheduleConfig;
  }
  /**
   * @param GoogleCloudAiplatformV1ModelMonitoringAlertConfig
   */
  public function setModelMonitoringAlertConfig(GoogleCloudAiplatformV1ModelMonitoringAlertConfig $modelMonitoringAlertConfig)
  {
    $this->modelMonitoringAlertConfig = $modelMonitoringAlertConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1ModelMonitoringAlertConfig
   */
  public function getModelMonitoringAlertConfig()
  {
    return $this->modelMonitoringAlertConfig;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setNextScheduleTime($nextScheduleTime)
  {
    $this->nextScheduleTime = $nextScheduleTime;
  }
  /**
   * @return string
   */
  public function getNextScheduleTime()
  {
    return $this->nextScheduleTime;
  }
  /**
   * @param string
   */
  public function setPredictInstanceSchemaUri($predictInstanceSchemaUri)
  {
    $this->predictInstanceSchemaUri = $predictInstanceSchemaUri;
  }
  /**
   * @return string
   */
  public function getPredictInstanceSchemaUri()
  {
    return $this->predictInstanceSchemaUri;
  }
  /**
   * @param array
   */
  public function setSamplePredictInstance($samplePredictInstance)
  {
    $this->samplePredictInstance = $samplePredictInstance;
  }
  /**
   * @return array
   */
  public function getSamplePredictInstance()
  {
    return $this->samplePredictInstance;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzi($satisfiesPzi)
  {
    $this->satisfiesPzi = $satisfiesPzi;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzi()
  {
    return $this->satisfiesPzi;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzs($satisfiesPzs)
  {
    $this->satisfiesPzs = $satisfiesPzs;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzs()
  {
    return $this->satisfiesPzs;
  }
  /**
   * @param string
   */
  public function setScheduleState($scheduleState)
  {
    $this->scheduleState = $scheduleState;
  }
  /**
   * @return string
   */
  public function getScheduleState()
  {
    return $this->scheduleState;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param GoogleCloudAiplatformV1GcsDestination
   */
  public function setStatsAnomaliesBaseDirectory(GoogleCloudAiplatformV1GcsDestination $statsAnomaliesBaseDirectory)
  {
    $this->statsAnomaliesBaseDirectory = $statsAnomaliesBaseDirectory;
  }
  /**
   * @return GoogleCloudAiplatformV1GcsDestination
   */
  public function getStatsAnomaliesBaseDirectory()
  {
    return $this->statsAnomaliesBaseDirectory;
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
class_alias(GoogleCloudAiplatformV1ModelDeploymentMonitoringJob::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ModelDeploymentMonitoringJob');
