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

class GoogleCloudAiplatformV1DataLabelingJob extends \Google\Collection
{
  protected $collection_key = 'specialistPools';
  protected $activeLearningConfigType = GoogleCloudAiplatformV1ActiveLearningConfig::class;
  protected $activeLearningConfigDataType = '';
  /**
   * @var string[]
   */
  public $annotationLabels;
  /**
   * @var string
   */
  public $createTime;
  protected $currentSpendType = GoogleTypeMoney::class;
  protected $currentSpendDataType = '';
  /**
   * @var string[]
   */
  public $datasets;
  /**
   * @var string
   */
  public $displayName;
  protected $encryptionSpecType = GoogleCloudAiplatformV1EncryptionSpec::class;
  protected $encryptionSpecDataType = '';
  protected $errorType = GoogleRpcStatus::class;
  protected $errorDataType = '';
  /**
   * @var array
   */
  public $inputs;
  /**
   * @var string
   */
  public $inputsSchemaUri;
  /**
   * @var string
   */
  public $instructionUri;
  /**
   * @var int
   */
  public $labelerCount;
  /**
   * @var int
   */
  public $labelingProgress;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string[]
   */
  public $specialistPools;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param GoogleCloudAiplatformV1ActiveLearningConfig
   */
  public function setActiveLearningConfig(GoogleCloudAiplatformV1ActiveLearningConfig $activeLearningConfig)
  {
    $this->activeLearningConfig = $activeLearningConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1ActiveLearningConfig
   */
  public function getActiveLearningConfig()
  {
    return $this->activeLearningConfig;
  }
  /**
   * @param string[]
   */
  public function setAnnotationLabels($annotationLabels)
  {
    $this->annotationLabels = $annotationLabels;
  }
  /**
   * @return string[]
   */
  public function getAnnotationLabels()
  {
    return $this->annotationLabels;
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
   * @param GoogleTypeMoney
   */
  public function setCurrentSpend(GoogleTypeMoney $currentSpend)
  {
    $this->currentSpend = $currentSpend;
  }
  /**
   * @return GoogleTypeMoney
   */
  public function getCurrentSpend()
  {
    return $this->currentSpend;
  }
  /**
   * @param string[]
   */
  public function setDatasets($datasets)
  {
    $this->datasets = $datasets;
  }
  /**
   * @return string[]
   */
  public function getDatasets()
  {
    return $this->datasets;
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
   * @param array
   */
  public function setInputs($inputs)
  {
    $this->inputs = $inputs;
  }
  /**
   * @return array
   */
  public function getInputs()
  {
    return $this->inputs;
  }
  /**
   * @param string
   */
  public function setInputsSchemaUri($inputsSchemaUri)
  {
    $this->inputsSchemaUri = $inputsSchemaUri;
  }
  /**
   * @return string
   */
  public function getInputsSchemaUri()
  {
    return $this->inputsSchemaUri;
  }
  /**
   * @param string
   */
  public function setInstructionUri($instructionUri)
  {
    $this->instructionUri = $instructionUri;
  }
  /**
   * @return string
   */
  public function getInstructionUri()
  {
    return $this->instructionUri;
  }
  /**
   * @param int
   */
  public function setLabelerCount($labelerCount)
  {
    $this->labelerCount = $labelerCount;
  }
  /**
   * @return int
   */
  public function getLabelerCount()
  {
    return $this->labelerCount;
  }
  /**
   * @param int
   */
  public function setLabelingProgress($labelingProgress)
  {
    $this->labelingProgress = $labelingProgress;
  }
  /**
   * @return int
   */
  public function getLabelingProgress()
  {
    return $this->labelingProgress;
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
   * @param string[]
   */
  public function setSpecialistPools($specialistPools)
  {
    $this->specialistPools = $specialistPools;
  }
  /**
   * @return string[]
   */
  public function getSpecialistPools()
  {
    return $this->specialistPools;
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
class_alias(GoogleCloudAiplatformV1DataLabelingJob::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1DataLabelingJob');
