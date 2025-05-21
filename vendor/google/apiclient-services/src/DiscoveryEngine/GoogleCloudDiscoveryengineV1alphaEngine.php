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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1alphaEngine extends \Google\Collection
{
  protected $collection_key = 'dataStoreIds';
  protected $chatEngineConfigType = GoogleCloudDiscoveryengineV1alphaEngineChatEngineConfig::class;
  protected $chatEngineConfigDataType = '';
  protected $chatEngineMetadataType = GoogleCloudDiscoveryengineV1alphaEngineChatEngineMetadata::class;
  protected $chatEngineMetadataDataType = '';
  protected $commonConfigType = GoogleCloudDiscoveryengineV1alphaEngineCommonConfig::class;
  protected $commonConfigDataType = '';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string[]
   */
  public $dataStoreIds;
  /**
   * @var bool
   */
  public $disableAnalytics;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $industryVertical;
  protected $mediaRecommendationEngineConfigType = GoogleCloudDiscoveryengineV1alphaEngineMediaRecommendationEngineConfig::class;
  protected $mediaRecommendationEngineConfigDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $recommendationMetadataType = GoogleCloudDiscoveryengineV1alphaEngineRecommendationMetadata::class;
  protected $recommendationMetadataDataType = '';
  protected $searchEngineConfigType = GoogleCloudDiscoveryengineV1alphaEngineSearchEngineConfig::class;
  protected $searchEngineConfigDataType = '';
  protected $similarDocumentsConfigType = GoogleCloudDiscoveryengineV1alphaEngineSimilarDocumentsEngineConfig::class;
  protected $similarDocumentsConfigDataType = '';
  /**
   * @var string
   */
  public $solutionType;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param GoogleCloudDiscoveryengineV1alphaEngineChatEngineConfig
   */
  public function setChatEngineConfig(GoogleCloudDiscoveryengineV1alphaEngineChatEngineConfig $chatEngineConfig)
  {
    $this->chatEngineConfig = $chatEngineConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaEngineChatEngineConfig
   */
  public function getChatEngineConfig()
  {
    return $this->chatEngineConfig;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaEngineChatEngineMetadata
   */
  public function setChatEngineMetadata(GoogleCloudDiscoveryengineV1alphaEngineChatEngineMetadata $chatEngineMetadata)
  {
    $this->chatEngineMetadata = $chatEngineMetadata;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaEngineChatEngineMetadata
   */
  public function getChatEngineMetadata()
  {
    return $this->chatEngineMetadata;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaEngineCommonConfig
   */
  public function setCommonConfig(GoogleCloudDiscoveryengineV1alphaEngineCommonConfig $commonConfig)
  {
    $this->commonConfig = $commonConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaEngineCommonConfig
   */
  public function getCommonConfig()
  {
    return $this->commonConfig;
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
   * @param string[]
   */
  public function setDataStoreIds($dataStoreIds)
  {
    $this->dataStoreIds = $dataStoreIds;
  }
  /**
   * @return string[]
   */
  public function getDataStoreIds()
  {
    return $this->dataStoreIds;
  }
  /**
   * @param bool
   */
  public function setDisableAnalytics($disableAnalytics)
  {
    $this->disableAnalytics = $disableAnalytics;
  }
  /**
   * @return bool
   */
  public function getDisableAnalytics()
  {
    return $this->disableAnalytics;
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
   * @param string
   */
  public function setIndustryVertical($industryVertical)
  {
    $this->industryVertical = $industryVertical;
  }
  /**
   * @return string
   */
  public function getIndustryVertical()
  {
    return $this->industryVertical;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaEngineMediaRecommendationEngineConfig
   */
  public function setMediaRecommendationEngineConfig(GoogleCloudDiscoveryengineV1alphaEngineMediaRecommendationEngineConfig $mediaRecommendationEngineConfig)
  {
    $this->mediaRecommendationEngineConfig = $mediaRecommendationEngineConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaEngineMediaRecommendationEngineConfig
   */
  public function getMediaRecommendationEngineConfig()
  {
    return $this->mediaRecommendationEngineConfig;
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
   * @param GoogleCloudDiscoveryengineV1alphaEngineRecommendationMetadata
   */
  public function setRecommendationMetadata(GoogleCloudDiscoveryengineV1alphaEngineRecommendationMetadata $recommendationMetadata)
  {
    $this->recommendationMetadata = $recommendationMetadata;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaEngineRecommendationMetadata
   */
  public function getRecommendationMetadata()
  {
    return $this->recommendationMetadata;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaEngineSearchEngineConfig
   */
  public function setSearchEngineConfig(GoogleCloudDiscoveryengineV1alphaEngineSearchEngineConfig $searchEngineConfig)
  {
    $this->searchEngineConfig = $searchEngineConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaEngineSearchEngineConfig
   */
  public function getSearchEngineConfig()
  {
    return $this->searchEngineConfig;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaEngineSimilarDocumentsEngineConfig
   */
  public function setSimilarDocumentsConfig(GoogleCloudDiscoveryengineV1alphaEngineSimilarDocumentsEngineConfig $similarDocumentsConfig)
  {
    $this->similarDocumentsConfig = $similarDocumentsConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaEngineSimilarDocumentsEngineConfig
   */
  public function getSimilarDocumentsConfig()
  {
    return $this->similarDocumentsConfig;
  }
  /**
   * @param string
   */
  public function setSolutionType($solutionType)
  {
    $this->solutionType = $solutionType;
  }
  /**
   * @return string
   */
  public function getSolutionType()
  {
    return $this->solutionType;
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
class_alias(GoogleCloudDiscoveryengineV1alphaEngine::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1alphaEngine');
