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

class DatabaseResourceRecommendationSignalData extends \Google\Model
{
  /**
   * @var array[]
   */
  public $additionalMetadata;
  /**
   * @var string
   */
  public $lastRefreshTime;
  /**
   * @var string
   */
  public $recommendationState;
  /**
   * @var string
   */
  public $recommender;
  /**
   * @var string
   */
  public $recommenderId;
  /**
   * @var string
   */
  public $recommenderSubtype;
  /**
   * @var string
   */
  public $resourceName;
  /**
   * @var string
   */
  public $signalType;

  /**
   * @param array[]
   */
  public function setAdditionalMetadata($additionalMetadata)
  {
    $this->additionalMetadata = $additionalMetadata;
  }
  /**
   * @return array[]
   */
  public function getAdditionalMetadata()
  {
    return $this->additionalMetadata;
  }
  /**
   * @param string
   */
  public function setLastRefreshTime($lastRefreshTime)
  {
    $this->lastRefreshTime = $lastRefreshTime;
  }
  /**
   * @return string
   */
  public function getLastRefreshTime()
  {
    return $this->lastRefreshTime;
  }
  /**
   * @param string
   */
  public function setRecommendationState($recommendationState)
  {
    $this->recommendationState = $recommendationState;
  }
  /**
   * @return string
   */
  public function getRecommendationState()
  {
    return $this->recommendationState;
  }
  /**
   * @param string
   */
  public function setRecommender($recommender)
  {
    $this->recommender = $recommender;
  }
  /**
   * @return string
   */
  public function getRecommender()
  {
    return $this->recommender;
  }
  /**
   * @param string
   */
  public function setRecommenderId($recommenderId)
  {
    $this->recommenderId = $recommenderId;
  }
  /**
   * @return string
   */
  public function getRecommenderId()
  {
    return $this->recommenderId;
  }
  /**
   * @param string
   */
  public function setRecommenderSubtype($recommenderSubtype)
  {
    $this->recommenderSubtype = $recommenderSubtype;
  }
  /**
   * @return string
   */
  public function getRecommenderSubtype()
  {
    return $this->recommenderSubtype;
  }
  /**
   * @param string
   */
  public function setResourceName($resourceName)
  {
    $this->resourceName = $resourceName;
  }
  /**
   * @return string
   */
  public function getResourceName()
  {
    return $this->resourceName;
  }
  /**
   * @param string
   */
  public function setSignalType($signalType)
  {
    $this->signalType = $signalType;
  }
  /**
   * @return string
   */
  public function getSignalType()
  {
    return $this->signalType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DatabaseResourceRecommendationSignalData::class, 'Google_Service_CloudRedis_DatabaseResourceRecommendationSignalData');
