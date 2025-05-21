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

class GoogleCloudAiplatformV1PipelineJob extends \Google\Collection
{
  protected $collection_key = 'reservedIpRanges';
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
  protected $jobDetailType = GoogleCloudAiplatformV1PipelineJobDetail::class;
  protected $jobDetailDataType = '';
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
  public $network;
  /**
   * @var array[]
   */
  public $pipelineSpec;
  /**
   * @var bool
   */
  public $preflightValidations;
  /**
   * @var string[]
   */
  public $reservedIpRanges;
  protected $runtimeConfigType = GoogleCloudAiplatformV1PipelineJobRuntimeConfig::class;
  protected $runtimeConfigDataType = '';
  /**
   * @var string
   */
  public $scheduleName;
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
  protected $templateMetadataType = GoogleCloudAiplatformV1PipelineTemplateMetadata::class;
  protected $templateMetadataDataType = '';
  /**
   * @var string
   */
  public $templateUri;
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
   * @param GoogleCloudAiplatformV1PipelineJobDetail
   */
  public function setJobDetail(GoogleCloudAiplatformV1PipelineJobDetail $jobDetail)
  {
    $this->jobDetail = $jobDetail;
  }
  /**
   * @return GoogleCloudAiplatformV1PipelineJobDetail
   */
  public function getJobDetail()
  {
    return $this->jobDetail;
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
  public function setNetwork($network)
  {
    $this->network = $network;
  }
  /**
   * @return string
   */
  public function getNetwork()
  {
    return $this->network;
  }
  /**
   * @param array[]
   */
  public function setPipelineSpec($pipelineSpec)
  {
    $this->pipelineSpec = $pipelineSpec;
  }
  /**
   * @return array[]
   */
  public function getPipelineSpec()
  {
    return $this->pipelineSpec;
  }
  /**
   * @param bool
   */
  public function setPreflightValidations($preflightValidations)
  {
    $this->preflightValidations = $preflightValidations;
  }
  /**
   * @return bool
   */
  public function getPreflightValidations()
  {
    return $this->preflightValidations;
  }
  /**
   * @param string[]
   */
  public function setReservedIpRanges($reservedIpRanges)
  {
    $this->reservedIpRanges = $reservedIpRanges;
  }
  /**
   * @return string[]
   */
  public function getReservedIpRanges()
  {
    return $this->reservedIpRanges;
  }
  /**
   * @param GoogleCloudAiplatformV1PipelineJobRuntimeConfig
   */
  public function setRuntimeConfig(GoogleCloudAiplatformV1PipelineJobRuntimeConfig $runtimeConfig)
  {
    $this->runtimeConfig = $runtimeConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1PipelineJobRuntimeConfig
   */
  public function getRuntimeConfig()
  {
    return $this->runtimeConfig;
  }
  /**
   * @param string
   */
  public function setScheduleName($scheduleName)
  {
    $this->scheduleName = $scheduleName;
  }
  /**
   * @return string
   */
  public function getScheduleName()
  {
    return $this->scheduleName;
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
   * @param GoogleCloudAiplatformV1PipelineTemplateMetadata
   */
  public function setTemplateMetadata(GoogleCloudAiplatformV1PipelineTemplateMetadata $templateMetadata)
  {
    $this->templateMetadata = $templateMetadata;
  }
  /**
   * @return GoogleCloudAiplatformV1PipelineTemplateMetadata
   */
  public function getTemplateMetadata()
  {
    return $this->templateMetadata;
  }
  /**
   * @param string
   */
  public function setTemplateUri($templateUri)
  {
    $this->templateUri = $templateUri;
  }
  /**
   * @return string
   */
  public function getTemplateUri()
  {
    return $this->templateUri;
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
class_alias(GoogleCloudAiplatformV1PipelineJob::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1PipelineJob');
