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

class GoogleCloudAiplatformV1NotebookExecutionJob extends \Google\Model
{
  /**
   * @var string
   */
  public $createTime;
  protected $customEnvironmentSpecType = GoogleCloudAiplatformV1NotebookExecutionJobCustomEnvironmentSpec::class;
  protected $customEnvironmentSpecDataType = '';
  protected $dataformRepositorySourceType = GoogleCloudAiplatformV1NotebookExecutionJobDataformRepositorySource::class;
  protected $dataformRepositorySourceDataType = '';
  protected $directNotebookSourceType = GoogleCloudAiplatformV1NotebookExecutionJobDirectNotebookSource::class;
  protected $directNotebookSourceDataType = '';
  /**
   * @var string
   */
  public $displayName;
  protected $encryptionSpecType = GoogleCloudAiplatformV1EncryptionSpec::class;
  protected $encryptionSpecDataType = '';
  /**
   * @var string
   */
  public $executionTimeout;
  /**
   * @var string
   */
  public $executionUser;
  protected $gcsNotebookSourceType = GoogleCloudAiplatformV1NotebookExecutionJobGcsNotebookSource::class;
  protected $gcsNotebookSourceDataType = '';
  /**
   * @var string
   */
  public $gcsOutputUri;
  /**
   * @var string
   */
  public $jobState;
  /**
   * @var string
   */
  public $kernelName;
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
  public $notebookRuntimeTemplateResourceName;
  /**
   * @var string
   */
  public $scheduleResourceName;
  /**
   * @var string
   */
  public $serviceAccount;
  protected $statusType = GoogleRpcStatus::class;
  protected $statusDataType = '';
  /**
   * @var string
   */
  public $updateTime;
  protected $workbenchRuntimeType = GoogleCloudAiplatformV1NotebookExecutionJobWorkbenchRuntime::class;
  protected $workbenchRuntimeDataType = '';

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
   * @param GoogleCloudAiplatformV1NotebookExecutionJobCustomEnvironmentSpec
   */
  public function setCustomEnvironmentSpec(GoogleCloudAiplatformV1NotebookExecutionJobCustomEnvironmentSpec $customEnvironmentSpec)
  {
    $this->customEnvironmentSpec = $customEnvironmentSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1NotebookExecutionJobCustomEnvironmentSpec
   */
  public function getCustomEnvironmentSpec()
  {
    return $this->customEnvironmentSpec;
  }
  /**
   * @param GoogleCloudAiplatformV1NotebookExecutionJobDataformRepositorySource
   */
  public function setDataformRepositorySource(GoogleCloudAiplatformV1NotebookExecutionJobDataformRepositorySource $dataformRepositorySource)
  {
    $this->dataformRepositorySource = $dataformRepositorySource;
  }
  /**
   * @return GoogleCloudAiplatformV1NotebookExecutionJobDataformRepositorySource
   */
  public function getDataformRepositorySource()
  {
    return $this->dataformRepositorySource;
  }
  /**
   * @param GoogleCloudAiplatformV1NotebookExecutionJobDirectNotebookSource
   */
  public function setDirectNotebookSource(GoogleCloudAiplatformV1NotebookExecutionJobDirectNotebookSource $directNotebookSource)
  {
    $this->directNotebookSource = $directNotebookSource;
  }
  /**
   * @return GoogleCloudAiplatformV1NotebookExecutionJobDirectNotebookSource
   */
  public function getDirectNotebookSource()
  {
    return $this->directNotebookSource;
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
  public function setExecutionTimeout($executionTimeout)
  {
    $this->executionTimeout = $executionTimeout;
  }
  /**
   * @return string
   */
  public function getExecutionTimeout()
  {
    return $this->executionTimeout;
  }
  /**
   * @param string
   */
  public function setExecutionUser($executionUser)
  {
    $this->executionUser = $executionUser;
  }
  /**
   * @return string
   */
  public function getExecutionUser()
  {
    return $this->executionUser;
  }
  /**
   * @param GoogleCloudAiplatformV1NotebookExecutionJobGcsNotebookSource
   */
  public function setGcsNotebookSource(GoogleCloudAiplatformV1NotebookExecutionJobGcsNotebookSource $gcsNotebookSource)
  {
    $this->gcsNotebookSource = $gcsNotebookSource;
  }
  /**
   * @return GoogleCloudAiplatformV1NotebookExecutionJobGcsNotebookSource
   */
  public function getGcsNotebookSource()
  {
    return $this->gcsNotebookSource;
  }
  /**
   * @param string
   */
  public function setGcsOutputUri($gcsOutputUri)
  {
    $this->gcsOutputUri = $gcsOutputUri;
  }
  /**
   * @return string
   */
  public function getGcsOutputUri()
  {
    return $this->gcsOutputUri;
  }
  /**
   * @param string
   */
  public function setJobState($jobState)
  {
    $this->jobState = $jobState;
  }
  /**
   * @return string
   */
  public function getJobState()
  {
    return $this->jobState;
  }
  /**
   * @param string
   */
  public function setKernelName($kernelName)
  {
    $this->kernelName = $kernelName;
  }
  /**
   * @return string
   */
  public function getKernelName()
  {
    return $this->kernelName;
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
  public function setNotebookRuntimeTemplateResourceName($notebookRuntimeTemplateResourceName)
  {
    $this->notebookRuntimeTemplateResourceName = $notebookRuntimeTemplateResourceName;
  }
  /**
   * @return string
   */
  public function getNotebookRuntimeTemplateResourceName()
  {
    return $this->notebookRuntimeTemplateResourceName;
  }
  /**
   * @param string
   */
  public function setScheduleResourceName($scheduleResourceName)
  {
    $this->scheduleResourceName = $scheduleResourceName;
  }
  /**
   * @return string
   */
  public function getScheduleResourceName()
  {
    return $this->scheduleResourceName;
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
   * @param GoogleRpcStatus
   */
  public function setStatus(GoogleRpcStatus $status)
  {
    $this->status = $status;
  }
  /**
   * @return GoogleRpcStatus
   */
  public function getStatus()
  {
    return $this->status;
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
  /**
   * @param GoogleCloudAiplatformV1NotebookExecutionJobWorkbenchRuntime
   */
  public function setWorkbenchRuntime(GoogleCloudAiplatformV1NotebookExecutionJobWorkbenchRuntime $workbenchRuntime)
  {
    $this->workbenchRuntime = $workbenchRuntime;
  }
  /**
   * @return GoogleCloudAiplatformV1NotebookExecutionJobWorkbenchRuntime
   */
  public function getWorkbenchRuntime()
  {
    return $this->workbenchRuntime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1NotebookExecutionJob::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1NotebookExecutionJob');
