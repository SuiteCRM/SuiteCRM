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

class GoogleCloudAiplatformV1TrainingPipeline extends \Google\Model
{
  /**
   * @var string
   */
  public $createTime;
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
  protected $inputDataConfigType = GoogleCloudAiplatformV1InputDataConfig::class;
  protected $inputDataConfigDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $modelId;
  protected $modelToUploadType = GoogleCloudAiplatformV1Model::class;
  protected $modelToUploadDataType = '';
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $parentModel;
  /**
   * @var string
   */
  public $startTime;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $trainingTaskDefinition;
  /**
   * @var array
   */
  public $trainingTaskInputs;
  /**
   * @var array
   */
  public $trainingTaskMetadata;
  /**
   * @var string
   */
  public $updateTime;

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
   * @param GoogleCloudAiplatformV1InputDataConfig
   */
  public function setInputDataConfig(GoogleCloudAiplatformV1InputDataConfig $inputDataConfig)
  {
    $this->inputDataConfig = $inputDataConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1InputDataConfig
   */
  public function getInputDataConfig()
  {
    return $this->inputDataConfig;
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
   * @param string
   */
  public function setModelId($modelId)
  {
    $this->modelId = $modelId;
  }
  /**
   * @return string
   */
  public function getModelId()
  {
    return $this->modelId;
  }
  /**
   * @param GoogleCloudAiplatformV1Model
   */
  public function setModelToUpload(GoogleCloudAiplatformV1Model $modelToUpload)
  {
    $this->modelToUpload = $modelToUpload;
  }
  /**
   * @return GoogleCloudAiplatformV1Model
   */
  public function getModelToUpload()
  {
    return $this->modelToUpload;
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
  public function setParentModel($parentModel)
  {
    $this->parentModel = $parentModel;
  }
  /**
   * @return string
   */
  public function getParentModel()
  {
    return $this->parentModel;
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
   * @param string
   */
  public function setTrainingTaskDefinition($trainingTaskDefinition)
  {
    $this->trainingTaskDefinition = $trainingTaskDefinition;
  }
  /**
   * @return string
   */
  public function getTrainingTaskDefinition()
  {
    return $this->trainingTaskDefinition;
  }
  /**
   * @param array
   */
  public function setTrainingTaskInputs($trainingTaskInputs)
  {
    $this->trainingTaskInputs = $trainingTaskInputs;
  }
  /**
   * @return array
   */
  public function getTrainingTaskInputs()
  {
    return $this->trainingTaskInputs;
  }
  /**
   * @param array
   */
  public function setTrainingTaskMetadata($trainingTaskMetadata)
  {
    $this->trainingTaskMetadata = $trainingTaskMetadata;
  }
  /**
   * @return array
   */
  public function getTrainingTaskMetadata()
  {
    return $this->trainingTaskMetadata;
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
class_alias(GoogleCloudAiplatformV1TrainingPipeline::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1TrainingPipeline');
