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

class GoogleCloudAiplatformV1NearestNeighborSearchOperationMetadataContentValidationStats extends \Google\Collection
{
  protected $collection_key = 'partialErrors';
  /**
   * @var string
   */
  public $invalidRecordCount;
  /**
   * @var string
   */
  public $invalidSparseRecordCount;
  protected $partialErrorsType = GoogleCloudAiplatformV1NearestNeighborSearchOperationMetadataRecordError::class;
  protected $partialErrorsDataType = 'array';
  /**
   * @var string
   */
  public $sourceGcsUri;
  /**
   * @var string
   */
  public $validRecordCount;
  /**
   * @var string
   */
  public $validSparseRecordCount;

  /**
   * @param string
   */
  public function setInvalidRecordCount($invalidRecordCount)
  {
    $this->invalidRecordCount = $invalidRecordCount;
  }
  /**
   * @return string
   */
  public function getInvalidRecordCount()
  {
    return $this->invalidRecordCount;
  }
  /**
   * @param string
   */
  public function setInvalidSparseRecordCount($invalidSparseRecordCount)
  {
    $this->invalidSparseRecordCount = $invalidSparseRecordCount;
  }
  /**
   * @return string
   */
  public function getInvalidSparseRecordCount()
  {
    return $this->invalidSparseRecordCount;
  }
  /**
   * @param GoogleCloudAiplatformV1NearestNeighborSearchOperationMetadataRecordError[]
   */
  public function setPartialErrors($partialErrors)
  {
    $this->partialErrors = $partialErrors;
  }
  /**
   * @return GoogleCloudAiplatformV1NearestNeighborSearchOperationMetadataRecordError[]
   */
  public function getPartialErrors()
  {
    return $this->partialErrors;
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
  /**
   * @param string
   */
  public function setValidRecordCount($validRecordCount)
  {
    $this->validRecordCount = $validRecordCount;
  }
  /**
   * @return string
   */
  public function getValidRecordCount()
  {
    return $this->validRecordCount;
  }
  /**
   * @param string
   */
  public function setValidSparseRecordCount($validSparseRecordCount)
  {
    $this->validSparseRecordCount = $validSparseRecordCount;
  }
  /**
   * @return string
   */
  public function getValidSparseRecordCount()
  {
    return $this->validSparseRecordCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1NearestNeighborSearchOperationMetadataContentValidationStats::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1NearestNeighborSearchOperationMetadataContentValidationStats');
