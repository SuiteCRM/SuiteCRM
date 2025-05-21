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

class GoogleCloudAiplatformV1BatchPredictionJob extends \Google\Collection
{
  protected $collection_key = 'partialFailures';
  protected $completionStatsType = GoogleCloudAiplatformV1CompletionStats::class;
  protected $completionStatsDataType = '';
  /**
   * @var string
   */
  public $createTime;
  protected $dedicatedResourcesType = GoogleCloudAiplatformV1BatchDedicatedResources::class;
  protected $dedicatedResourcesDataType = '';
  /**
   * @var bool
   */
  public $disableContainerLogging;
  /**
   * @var string
   */
  public $displayName;
  protected $encryptionSpecType = GoogleCloudAiplatformV1EncryptionSpec::class;
  protected $encryptionSpecDataType = '';
  /**
   * @var string
   */
  public $endTime;
  protected $errorType = GoogleRpcStatus::class;
  protected $errorDataType = '';
  protected $explanationSpecType = GoogleCloudAiplatformV1ExplanationSpec::class;
  protected $explanationSpecDataType = '';
  /**
   * @var bool
   */
  public $generateExplanation;
  protected $inputConfigType = GoogleCloudAiplatformV1BatchPredictionJobInputConfig::class;
  protected $inputConfigDataType = '';
  protected $instanceConfigType = GoogleCloudAiplatformV1BatchPredictionJobInstanceConfig::class;
  protected $instanceConfigDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  protected $manualBatchTuningParametersType = GoogleCloudAiplatformV1ManualBatchTuningParameters::class;
  protected $manualBatchTuningParametersDataType = '';
  /**
   * @var string
   */
  public $model;
  /**
   * @var array
   */
  public $modelParameters;
  /**
   * @var string
   */
  public $modelVersionId;
  /**
   * @var string
   */
  public $name;
  protected $outputConfigType = GoogleCloudAiplatformV1BatchPredictionJobOutputConfig::class;
  protected $outputConfigDataType = '';
  protected $outputInfoType = GoogleCloudAiplatformV1BatchPredictionJobOutputInfo::class;
  protected $outputInfoDataType = '';
  protected $partialFailuresType = GoogleRpcStatus::class;
  protected $partialFailuresDataType = 'array';
  protected $resourcesConsumedType = GoogleCloudAiplatformV1ResourcesConsumed::class;
  protected $resourcesConsumedDataType = '';
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
  public $serviceAccount;
  /**
   * @var string
   */
  public $startTime;
  /**
   * @var string
   */
  public $state;
  protected $unmanagedContainerModelType = GoogleCloudAiplatformV1UnmanagedContainerModel::class;
  protected $unmanagedContainerModelDataType = '';
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param GoogleCloudAiplatformV1CompletionStats
   */
  public function setCompletionStats(GoogleCloudAiplatformV1CompletionStats $completionStats)
  {
    $this->completionStats = $completionStats;
  }
  /**
   * @return GoogleCloudAiplatformV1CompletionStats
   */
  public function getCompletionStats()
  {
    return $this->completionStats;
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
   * @param GoogleCloudAiplatformV1BatchDedicatedResources
   */
  public function setDedicatedResources(GoogleCloudAiplatformV1BatchDedicatedResources $dedicatedResources)
  {
    $this->dedicatedResources = $dedicatedResources;
  }
  /**
   * @return GoogleCloudAiplatformV1BatchDedicatedResources
   */
  public function getDedicatedResources()
  {
    return $this->dedicatedResources;
  }
  /**
   * @param bool
   */
  public function setDisableContainerLogging($disableContainerLogging)
  {
    $this->disableContainerLogging = $disableContainerLogging;
  }
  /**
   * @return bool
   */
  public function getDisableContainerLogging()
  {
    return $this->disableContainerLogging;
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
   * @param GoogleCloudAiplatformV1ExplanationSpec
   */
  public function setExplanationSpec(GoogleCloudAiplatformV1ExplanationSpec $explanationSpec)
  {
    $this->explanationSpec = $explanationSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1ExplanationSpec
   */
  public function getExplanationSpec()
  {
    return $this->explanationSpec;
  }
  /**
   * @param bool
   */
  public function setGenerateExplanation($generateExplanation)
  {
    $this->generateExplanation = $generateExplanation;
  }
  /**
   * @return bool
   */
  public function getGenerateExplanation()
  {
    return $this->generateExplanation;
  }
  /**
   * @param GoogleCloudAiplatformV1BatchPredictionJobInputConfig
   */
  public function setInputConfig(GoogleCloudAiplatformV1BatchPredictionJobInputConfig $inputConfig)
  {
    $this->inputConfig = $inputConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1BatchPredictionJobInputConfig
   */
  public function getInputConfig()
  {
    return $this->inputConfig;
  }
  /**
   * @param GoogleCloudAiplatformV1BatchPredictionJobInstanceConfig
   */
  public function setInstanceConfig(GoogleCloudAiplatformV1BatchPredictionJobInstanceConfig $instanceConfig)
  {
    $this->instanceConfig = $instanceConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1BatchPredictionJobInstanceConfig
   */
  public function getInstanceConfig()
  {
    return $this->instanceConfig;
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
   * @param GoogleCloudAiplatformV1ManualBatchTuningParameters
   */
  public function setManualBatchTuningParameters(GoogleCloudAiplatformV1ManualBatchTuningParameters $manualBatchTuningParameters)
  {
    $this->manualBatchTuningParameters = $manualBatchTuningParameters;
  }
  /**
   * @return GoogleCloudAiplatformV1ManualBatchTuningParameters
   */
  public function getManualBatchTuningParameters()
  {
    return $this->manualBatchTuningParameters;
  }
  /**
   * @param string
   */
  public function setModel($model)
  {
    $this->model = $model;
  }
  /**
   * @return string
   */
  public function getModel()
  {
    return $this->model;
  }
  /**
   * @param array
   */
  public function setModelParameters($modelParameters)
  {
    $this->modelParameters = $modelParameters;
  }
  /**
   * @return array
   */
  public function getModelParameters()
  {
    return $this->modelParameters;
  }
  /**
   * @param string
   */
  public function setModelVersionId($modelVersionId)
  {
    $this->modelVersionId = $modelVersionId;
  }
  /**
   * @return string
   */
  public function getModelVersionId()
  {
    return $this->modelVersionId;
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
   * @param GoogleCloudAiplatformV1BatchPredictionJobOutputConfig
   */
  public function setOutputConfig(GoogleCloudAiplatformV1BatchPredictionJobOutputConfig $outputConfig)
  {
    $this->outputConfig = $outputConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1BatchPredictionJobOutputConfig
   */
  public function getOutputConfig()
  {
    return $this->outputConfig;
  }
  /**
   * @param GoogleCloudAiplatformV1BatchPredictionJobOutputInfo
   */
  public function setOutputInfo(GoogleCloudAiplatformV1BatchPredictionJobOutputInfo $outputInfo)
  {
    $this->outputInfo = $outputInfo;
  }
  /**
   * @return GoogleCloudAiplatformV1BatchPredictionJobOutputInfo
   */
  public function getOutputInfo()
  {
    return $this->outputInfo;
  }
  /**
   * @param GoogleRpcStatus[]
   */
  public function setPartialFailures($partialFailures)
  {
    $this->partialFailures = $partialFailures;
  }
  /**
   * @return GoogleRpcStatus[]
   */
  public function getPartialFailures()
  {
    return $this->partialFailures;
  }
  /**
   * @param GoogleCloudAiplatformV1ResourcesConsumed
   */
  public function setResourcesConsumed(GoogleCloudAiplatformV1ResourcesConsumed $resourcesConsumed)
  {
    $this->resourcesConsumed = $resourcesConsumed;
  }
  /**
   * @return GoogleCloudAiplatformV1ResourcesConsumed
   */
  public function getResourcesConsumed()
  {
    return $this->resourcesConsumed;
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
  public function setServiceAccount($serviceAccount)
  {
    $this->serviceAccount = $serviceAccount;
  }
  /**
   * @return string
   */
  public function getServiceAccount()
  {
    return $this->serviceAccount;
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
   * @param GoogleCloudAiplatformV1UnmanagedContainerModel
   */
  public function setUnmanagedContainerModel(GoogleCloudAiplatformV1UnmanagedContainerModel $unmanagedContainerModel)
  {
    $this->unmanagedContainerModel = $unmanagedContainerModel;
  }
  /**
   * @return GoogleCloudAiplatformV1UnmanagedContainerModel
   */
  public function getUnmanagedContainerModel()
  {
    return $this->unmanagedContainerModel;
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
class_alias(GoogleCloudAiplatformV1BatchPredictionJob::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1BatchPredictionJob');
