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

class GoogleCloudAiplatformV1ImportFeatureValuesOperationMetadata extends \Google\Collection
{
  protected $collection_key = 'sourceUris';
  /**
   * @var string[]
   */
  public $blockingOperationIds;
  protected $genericMetadataType = GoogleCloudAiplatformV1GenericOperationMetadata::class;
  protected $genericMetadataDataType = '';
  /**
   * @var string
   */
  public $importedEntityCount;
  /**
   * @var string
   */
  public $importedFeatureValueCount;
  /**
   * @var string
   */
  public $invalidRowCount;
  /**
   * @var string[]
   */
  public $sourceUris;
  /**
   * @var string
   */
  public $timestampOutsideRetentionRowsCount;

  /**
   * @param string[]
   */
  public function setBlockingOperationIds($blockingOperationIds)
  {
    $this->blockingOperationIds = $blockingOperationIds;
  }
  /**
   * @return string[]
   */
  public function getBlockingOperationIds()
  {
    return $this->blockingOperationIds;
  }
  /**
   * @param GoogleCloudAiplatformV1GenericOperationMetadata
   */
  public function setGenericMetadata(GoogleCloudAiplatformV1GenericOperationMetadata $genericMetadata)
  {
    $this->genericMetadata = $genericMetadata;
  }
  /**
   * @return GoogleCloudAiplatformV1GenericOperationMetadata
   */
  public function getGenericMetadata()
  {
    return $this->genericMetadata;
  }
  /**
   * @param string
   */
  public function setImportedEntityCount($importedEntityCount)
  {
    $this->importedEntityCount = $importedEntityCount;
  }
  /**
   * @return string
   */
  public function getImportedEntityCount()
  {
    return $this->importedEntityCount;
  }
  /**
   * @param string
   */
  public function setImportedFeatureValueCount($importedFeatureValueCount)
  {
    $this->importedFeatureValueCount = $importedFeatureValueCount;
  }
  /**
   * @return string
   */
  public function getImportedFeatureValueCount()
  {
    return $this->importedFeatureValueCount;
  }
  /**
   * @param string
   */
  public function setInvalidRowCount($invalidRowCount)
  {
    $this->invalidRowCount = $invalidRowCount;
  }
  /**
   * @return string
   */
  public function getInvalidRowCount()
  {
    return $this->invalidRowCount;
  }
  /**
   * @param string[]
   */
  public function setSourceUris($sourceUris)
  {
    $this->sourceUris = $sourceUris;
  }
  /**
   * @return string[]
   */
  public function getSourceUris()
  {
    return $this->sourceUris;
  }
  /**
   * @param string
   */
  public function setTimestampOutsideRetentionRowsCount($timestampOutsideRetentionRowsCount)
  {
    $this->timestampOutsideRetentionRowsCount = $timestampOutsideRetentionRowsCount;
  }
  /**
   * @return string
   */
  public function getTimestampOutsideRetentionRowsCount()
  {
    return $this->timestampOutsideRetentionRowsCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ImportFeatureValuesOperationMetadata::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ImportFeatureValuesOperationMetadata');
