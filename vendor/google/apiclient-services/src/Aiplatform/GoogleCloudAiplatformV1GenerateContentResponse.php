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

class GoogleCloudAiplatformV1GenerateContentResponse extends \Google\Collection
{
  protected $collection_key = 'candidates';
  protected $candidatesType = GoogleCloudAiplatformV1Candidate::class;
  protected $candidatesDataType = 'array';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $modelVersion;
  protected $promptFeedbackType = GoogleCloudAiplatformV1GenerateContentResponsePromptFeedback::class;
  protected $promptFeedbackDataType = '';
  /**
   * @var string
   */
  public $responseId;
  protected $usageMetadataType = GoogleCloudAiplatformV1GenerateContentResponseUsageMetadata::class;
  protected $usageMetadataDataType = '';

  /**
   * @param GoogleCloudAiplatformV1Candidate[]
   */
  public function setCandidates($candidates)
  {
    $this->candidates = $candidates;
  }
  /**
   * @return GoogleCloudAiplatformV1Candidate[]
   */
  public function getCandidates()
  {
    return $this->candidates;
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
  public function setModelVersion($modelVersion)
  {
    $this->modelVersion = $modelVersion;
  }
  /**
   * @return string
   */
  public function getModelVersion()
  {
    return $this->modelVersion;
  }
  /**
   * @param GoogleCloudAiplatformV1GenerateContentResponsePromptFeedback
   */
  public function setPromptFeedback(GoogleCloudAiplatformV1GenerateContentResponsePromptFeedback $promptFeedback)
  {
    $this->promptFeedback = $promptFeedback;
  }
  /**
   * @return GoogleCloudAiplatformV1GenerateContentResponsePromptFeedback
   */
  public function getPromptFeedback()
  {
    return $this->promptFeedback;
  }
  /**
   * @param string
   */
  public function setResponseId($responseId)
  {
    $this->responseId = $responseId;
  }
  /**
   * @return string
   */
  public function getResponseId()
  {
    return $this->responseId;
  }
  /**
   * @param GoogleCloudAiplatformV1GenerateContentResponseUsageMetadata
   */
  public function setUsageMetadata(GoogleCloudAiplatformV1GenerateContentResponseUsageMetadata $usageMetadata)
  {
    $this->usageMetadata = $usageMetadata;
  }
  /**
   * @return GoogleCloudAiplatformV1GenerateContentResponseUsageMetadata
   */
  public function getUsageMetadata()
  {
    return $this->usageMetadata;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1GenerateContentResponse::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1GenerateContentResponse');
