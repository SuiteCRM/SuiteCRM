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

class GoogleCloudDiscoveryengineV1betaEngine extends \Google\Collection
{
  protected $collection_key = 'dataStoreIds';
  protected $chatEngineConfigType = GoogleCloudDiscoveryengineV1betaEngineChatEngineConfig::class;
  protected $chatEngineConfigDataType = '';
  protected $chatEngineMetadataType = GoogleCloudDiscoveryengineV1betaEngineChatEngineMetadata::class;
  protected $chatEngineMetadataDataType = '';
  protected $commonConfigType = GoogleCloudDiscoveryengineV1betaEngineCommonConfig::class;
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
  /**
   * @var string
   */
  public $name;
  protected $searchEngineConfigType = GoogleCloudDiscoveryengineV1betaEngineSearchEngineConfig::class;
  protected $searchEngineConfigDataType = '';
  /**
   * @var string
   */
  public $solutionType;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param GoogleCloudDiscoveryengineV1betaEngineChatEngineConfig
   */
  public function setChatEngineConfig(GoogleCloudDiscoveryengineV1betaEngineChatEngineConfig $chatEngineConfig)
  {
    $this->chatEngineConfig = $chatEngineConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaEngineChatEngineConfig
   */
  public function getChatEngineConfig()
  {
    return $this->chatEngineConfig;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1betaEngineChatEngineMetadata
   */
  public function setChatEngineMetadata(GoogleCloudDiscoveryengineV1betaEngineChatEngineMetadata $chatEngineMetadata)
  {
    $this->chatEngineMetadata = $chatEngineMetadata;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaEngineChatEngineMetadata
   */
  public function getChatEngineMetadata()
  {
    return $this->chatEngineMetadata;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1betaEngineCommonConfig
   */
  public function setCommonConfig(GoogleCloudDiscoveryengineV1betaEngineCommonConfig $commonConfig)
  {
    $this->commonConfig = $commonConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaEngineCommonConfig
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
   * @param GoogleCloudDiscoveryengineV1betaEngineSearchEngineConfig
   */
  public function setSearchEngineConfig(GoogleCloudDiscoveryengineV1betaEngineSearchEngineConfig $searchEngineConfig)
  {
    $this->searchEngineConfig = $searchEngineConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaEngineSearchEngineConfig
   */
  public function getSearchEngineConfig()
  {
    return $this->searchEngineConfig;
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
class_alias(GoogleCloudDiscoveryengineV1betaEngine::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1betaEngine');
