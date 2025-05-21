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

class GoogleCloudAiplatformV1GenerateContentResponseUsageMetadata extends \Google\Collection
{
  protected $collection_key = 'promptTokensDetails';
  protected $cacheTokensDetailsType = GoogleCloudAiplatformV1ModalityTokenCount::class;
  protected $cacheTokensDetailsDataType = 'array';
  /**
   * @var int
   */
  public $cachedContentTokenCount;
  /**
   * @var int
   */
  public $candidatesTokenCount;
  protected $candidatesTokensDetailsType = GoogleCloudAiplatformV1ModalityTokenCount::class;
  protected $candidatesTokensDetailsDataType = 'array';
  /**
   * @var int
   */
  public $promptTokenCount;
  protected $promptTokensDetailsType = GoogleCloudAiplatformV1ModalityTokenCount::class;
  protected $promptTokensDetailsDataType = 'array';
  /**
   * @var int
   */
  public $totalTokenCount;

  /**
   * @param GoogleCloudAiplatformV1ModalityTokenCount[]
   */
  public function setCacheTokensDetails($cacheTokensDetails)
  {
    $this->cacheTokensDetails = $cacheTokensDetails;
  }
  /**
   * @return GoogleCloudAiplatformV1ModalityTokenCount[]
   */
  public function getCacheTokensDetails()
  {
    return $this->cacheTokensDetails;
  }
  /**
   * @param int
   */
  public function setCachedContentTokenCount($cachedContentTokenCount)
  {
    $this->cachedContentTokenCount = $cachedContentTokenCount;
  }
  /**
   * @return int
   */
  public function getCachedContentTokenCount()
  {
    return $this->cachedContentTokenCount;
  }
  /**
   * @param int
   */
  public function setCandidatesTokenCount($candidatesTokenCount)
  {
    $this->candidatesTokenCount = $candidatesTokenCount;
  }
  /**
   * @return int
   */
  public function getCandidatesTokenCount()
  {
    return $this->candidatesTokenCount;
  }
  /**
   * @param GoogleCloudAiplatformV1ModalityTokenCount[]
   */
  public function setCandidatesTokensDetails($candidatesTokensDetails)
  {
    $this->candidatesTokensDetails = $candidatesTokensDetails;
  }
  /**
   * @return GoogleCloudAiplatformV1ModalityTokenCount[]
   */
  public function getCandidatesTokensDetails()
  {
    return $this->candidatesTokensDetails;
  }
  /**
   * @param int
   */
  public function setPromptTokenCount($promptTokenCount)
  {
    $this->promptTokenCount = $promptTokenCount;
  }
  /**
   * @return int
   */
  public function getPromptTokenCount()
  {
    return $this->promptTokenCount;
  }
  /**
   * @param GoogleCloudAiplatformV1ModalityTokenCount[]
   */
  public function setPromptTokensDetails($promptTokensDetails)
  {
    $this->promptTokensDetails = $promptTokensDetails;
  }
  /**
   * @return GoogleCloudAiplatformV1ModalityTokenCount[]
   */
  public function getPromptTokensDetails()
  {
    return $this->promptTokensDetails;
  }
  /**
   * @param int
   */
  public function setTotalTokenCount($totalTokenCount)
  {
    $this->totalTokenCount = $totalTokenCount;
  }
  /**
   * @return int
   */
  public function getTotalTokenCount()
  {
    return $this->totalTokenCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1GenerateContentResponseUsageMetadata::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1GenerateContentResponseUsageMetadata');
