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

namespace Google\Service\CloudRedis;

class DatabaseResourceFeed extends \Google\Model
{
  /**
   * @var string
   */
  public $feedTimestamp;
  /**
   * @var string
   */
  public $feedType;
  protected $observabilityMetricDataType = ObservabilityMetricData::class;
  protected $observabilityMetricDataDataType = '';
  protected $recommendationSignalDataType = DatabaseResourceRecommendationSignalData::class;
  protected $recommendationSignalDataDataType = '';
  protected $resourceHealthSignalDataType = DatabaseResourceHealthSignalData::class;
  protected $resourceHealthSignalDataDataType = '';
  protected $resourceIdType = DatabaseResourceId::class;
  protected $resourceIdDataType = '';
  protected $resourceMetadataType = DatabaseResourceMetadata::class;
  protected $resourceMetadataDataType = '';

  /**
   * @param string
   */
  public function setFeedTimestamp($feedTimestamp)
  {
    $this->feedTimestamp = $feedTimestamp;
  }
  /**
   * @return string
   */
  public function getFeedTimestamp()
  {
    return $this->feedTimestamp;
  }
  /**
   * @param string
   */
  public function setFeedType($feedType)
  {
    $this->feedType = $feedType;
  }
  /**
   * @return string
   */
  public function getFeedType()
  {
    return $this->feedType;
  }
  /**
   * @param ObservabilityMetricData
   */
  public function setObservabilityMetricData(ObservabilityMetricData $observabilityMetricData)
  {
    $this->observabilityMetricData = $observabilityMetricData;
  }
  /**
   * @return ObservabilityMetricData
   */
  public function getObservabilityMetricData()
  {
    return $this->observabilityMetricData;
  }
  /**
   * @param DatabaseResourceRecommendationSignalData
   */
  public function setRecommendationSignalData(DatabaseResourceRecommendationSignalData $recommendationSignalData)
  {
    $this->recommendationSignalData = $recommendationSignalData;
  }
  /**
   * @return DatabaseResourceRecommendationSignalData
   */
  public function getRecommendationSignalData()
  {
    return $this->recommendationSignalData;
  }
  /**
   * @param DatabaseResourceHealthSignalData
   */
  public function setResourceHealthSignalData(DatabaseResourceHealthSignalData $resourceHealthSignalData)
  {
    $this->resourceHealthSignalData = $resourceHealthSignalData;
  }
  /**
   * @return DatabaseResourceHealthSignalData
   */
  public function getResourceHealthSignalData()
  {
    return $this->resourceHealthSignalData;
  }
  /**
   * @param DatabaseResourceId
   */
  public function setResourceId(DatabaseResourceId $resourceId)
  {
    $this->resourceId = $resourceId;
  }
  /**
   * @return DatabaseResourceId
   */
  public function getResourceId()
  {
    return $this->resourceId;
  }
  /**
   * @param DatabaseResourceMetadata
   */
  public function setResourceMetadata(DatabaseResourceMetadata $resourceMetadata)
  {
    $this->resourceMetadata = $resourceMetadata;
  }
  /**
   * @return DatabaseResourceMetadata
   */
  public function getResourceMetadata()
  {
    return $this->resourceMetadata;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DatabaseResourceFeed::class, 'Google_Service_CloudRedis_DatabaseResourceFeed');
