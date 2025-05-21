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

class GoogleCloudAiplatformV1NearestNeighborSearchOperationMetadataRecordError extends \Google\Model
{
  /**
   * @var string
   */
  public $embeddingId;
  /**
   * @var string
   */
  public $errorMessage;
  /**
   * @var string
   */
  public $errorType;
  /**
   * @var string
   */
  public $rawRecord;
  /**
   * @var string
   */
  public $sourceGcsUri;

  /**
   * @param string
   */
  public function setEmbeddingId($embeddingId)
  {
    $this->embeddingId = $embeddingId;
  }
  /**
   * @return string
   */
  public function getEmbeddingId()
  {
    return $this->embeddingId;
  }
  /**
   * @param string
   */
  public function setErrorMessage($errorMessage)
  {
    $this->errorMessage = $errorMessage;
  }
  /**
   * @return string
   */
  public function getErrorMessage()
  {
    return $this->errorMessage;
  }
  /**
   * @param string
   */
  public function setErrorType($errorType)
  {
    $this->errorType = $errorType;
  }
  /**
   * @return string
   */
  public function getErrorType()
  {
    return $this->errorType;
  }
  /**
   * @param string
   */
  public function setRawRecord($rawRecord)
  {
    $this->rawRecord = $rawRecord;
  }
  /**
   * @return string
   */
  public function getRawRecord()
  {
    return $this->rawRecord;
  }
  /**
   * @param string
   */
  public function setSourceGcsUri($sourceGcsUri)
  {
    $this->sourceGcsUri = $sourceGcsUri;
  }
  /**
   * @return string
   */
  public function getSourceGcsUri()
  {
    return $this->sourceGcsUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1NearestNeighborSearchOperationMetadataRecordError::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1NearestNeighborSearchOperationMetadataRecordError');
