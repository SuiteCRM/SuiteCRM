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

namespace Google\Service\Contactcenterinsights;

class GoogleCloudContactcenterinsightsV1alpha1IngestConversationsRequestGcsSource extends \Google\Collection
{
  protected $collection_key = 'customMetadataKeys';
  /**
   * @var string
   */
  public $bucketObjectType;
  /**
   * @var string
   */
  public $bucketUri;
  /**
   * @var string[]
   */
  public $customMetadataKeys;
  /**
   * @var string
   */
  public $metadataBucketUri;

  /**
   * @param string
   */
  public function setBucketObjectType($bucketObjectType)
  {
    $this->bucketObjectType = $bucketObjectType;
  }
  /**
   * @return string
   */
  public function getBucketObjectType()
  {
    return $this->bucketObjectType;
  }
  /**
   * @param string
   */
  public function setBucketUri($bucketUri)
  {
    $this->bucketUri = $bucketUri;
  }
  /**
   * @return string
   */
  public function getBucketUri()
  {
    return $this->bucketUri;
  }
  /**
   * @param string[]
   */
  public function setCustomMetadataKeys($customMetadataKeys)
  {
    $this->customMetadataKeys = $customMetadataKeys;
  }
  /**
   * @return string[]
   */
  public function getCustomMetadataKeys()
  {
    return $this->customMetadataKeys;
  }
  /**
   * @param string
   */
  public function setMetadataBucketUri($metadataBucketUri)
  {
    $this->metadataBucketUri = $metadataBucketUri;
  }
  /**
   * @return string
   */
  public function getMetadataBucketUri()
  {
    return $this->metadataBucketUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1alpha1IngestConversationsRequestGcsSource::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1alpha1IngestConversationsRequestGcsSource');
