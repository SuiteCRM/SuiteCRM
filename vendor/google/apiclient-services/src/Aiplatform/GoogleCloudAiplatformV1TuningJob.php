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

class GoogleCloudAiplatformV1TuningJob extends \Google\Model
{
  /**
   * @var string
   */
  public $baseModel;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  protected $encryptionSpecType = GoogleCloudAiplatformV1EncryptionSpec::class;
  protected $encryptionSpecDataType = '';
  /**
   * @var string
   */
  public $endTime;
  protected $errorType = GoogleRpcStatus::class;
  protected $errorDataType = '';
  /**
   * @var string
   */
  public $experiment;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
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
  protected $supervisedTuningSpecType = GoogleCloudAiplatformV1SupervisedTuningSpec::class;
  protected $supervisedTuningSpecDataType = '';
  protected $tunedModelType = GoogleCloudAiplatformV1TunedModel::class;
  protected $tunedModelDataType = '';
  /**
   * @var string
   */
  public $tunedModelDisplayName;
  protected $tuningDataStatsType = GoogleCloudAiplatformV1TuningDataStats::class;
  protected $tuningDataStatsDataType = '';
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string
   */
  public function setBaseModel($baseModel)
  {
    $this->baseModel = $baseModel;
  }
  /**
   * @return string
   */
  public function getBaseModel()
  {
    return $this->baseModel;
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
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
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
   * @param string
   */
  public function setExperiment($experiment)
  {
    $this->experiment = $experiment;
  }
  /**
   * @return string
   */
  public function getExperiment()
  {
    return $this->experiment;
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
   * @param GoogleCloudAiplatformV1SupervisedTuningSpec
   */
  public function setSupervisedTuningSpec(GoogleCloudAiplatformV1SupervisedTuningSpec $supervisedTuningSpec)
  {
    $this->supervisedTuningSpec = $supervisedTuningSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1SupervisedTuningSpec
   */
  public function getSupervisedTuningSpec()
  {
    return $this->supervisedTuningSpec;
  }
  /**
   * @param GoogleCloudAiplatformV1TunedModel
   */
  public function setTunedModel(GoogleCloudAiplatformV1TunedModel $tunedModel)
  {
    $this->tunedModel = $tunedModel;
  }
  /**
   * @return GoogleCloudAiplatformV1TunedModel
   */
  public function getTunedModel()
  {
    return $this->tunedModel;
  }
  /**
   * @param string
   */
  public function setTunedModelDisplayName($tunedModelDisplayName)
  {
    $this->tunedModelDisplayName = $tunedModelDisplayName;
  }
  /**
   * @return string
   */
  public function getTunedModelDisplayName()
  {
    return $this->tunedModelDisplayName;
  }
  /**
   * @param GoogleCloudAiplatformV1TuningDataStats
   */
  public function setTuningDataStats(GoogleCloudAiplatformV1TuningDataStats $tuningDataStats)
  {
    $this->tuningDataStats = $tuningDataStats;
  }
  /**
   * @return GoogleCloudAiplatformV1TuningDataStats
   */
  public function getTuningDataStats()
  {
    return $this->tuningDataStats;
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
class_alias(GoogleCloudAiplatformV1TuningJob::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1TuningJob');
