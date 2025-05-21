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

class GoogleCloudAiplatformV1HyperparameterTuningJob extends \Google\Collection
{
  protected $collection_key = 'trials';
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
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var int
   */
  public $maxFailedTrialCount;
  /**
   * @var int
   */
  public $maxTrialCount;
  /**
   * @var string
   */
  public $name;
  /**
   * @var int
   */
  public $parallelTrialCount;
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
  public $startTime;
  /**
   * @var string
   */
  public $state;
  protected $studySpecType = GoogleCloudAiplatformV1StudySpec::class;
  protected $studySpecDataType = '';
  protected $trialJobSpecType = GoogleCloudAiplatformV1CustomJobSpec::class;
  protected $trialJobSpecDataType = '';
  protected $trialsType = GoogleCloudAiplatformV1Trial::class;
  protected $trialsDataType = 'array';
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
   * @param int
   */
  public function setMaxFailedTrialCount($maxFailedTrialCount)
  {
    $this->maxFailedTrialCount = $maxFailedTrialCount;
  }
  /**
   * @return int
   */
  public function getMaxFailedTrialCount()
  {
    return $this->maxFailedTrialCount;
  }
  /**
   * @param int
   */
  public function setMaxTrialCount($maxTrialCount)
  {
    $this->maxTrialCount = $maxTrialCount;
  }
  /**
   * @return int
   */
  public function getMaxTrialCount()
  {
    return $this->maxTrialCount;
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
   * @param int
   */
  public function setParallelTrialCount($parallelTrialCount)
  {
    $this->parallelTrialCount = $parallelTrialCount;
  }
  /**
   * @return int
   */
  public function getParallelTrialCount()
  {
    return $this->parallelTrialCount;
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
   * @param GoogleCloudAiplatformV1StudySpec
   */
  public function setStudySpec(GoogleCloudAiplatformV1StudySpec $studySpec)
  {
    $this->studySpec = $studySpec;
  }
  /**
   * @return GoogleCloudAiplatformV1StudySpec
   */
  public function getStudySpec()
  {
    return $this->studySpec;
  }
  /**
   * @param GoogleCloudAiplatformV1CustomJobSpec
   */
  public function setTrialJobSpec(GoogleCloudAiplatformV1CustomJobSpec $trialJobSpec)
  {
    $this->trialJobSpec = $trialJobSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1CustomJobSpec
   */
  public function getTrialJobSpec()
  {
    return $this->trialJobSpec;
  }
  /**
   * @param GoogleCloudAiplatformV1Trial[]
   */
  public function setTrials($trials)
  {
    $this->trials = $trials;
  }
  /**
   * @return GoogleCloudAiplatformV1Trial[]
   */
  public function getTrials()
  {
    return $this->trials;
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
class_alias(GoogleCloudAiplatformV1HyperparameterTuningJob::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1HyperparameterTuningJob');
