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

class GoogleCloudDiscoveryengineV1UserEvent extends \Google\Collection
{
  protected $collection_key = 'tagIds';
  protected $attributesType = GoogleCloudDiscoveryengineV1CustomAttribute::class;
  protected $attributesDataType = 'map';
  /**
   * @var string
   */
  public $attributionToken;
  protected $completionInfoType = GoogleCloudDiscoveryengineV1CompletionInfo::class;
  protected $completionInfoDataType = '';
  /**
   * @var string
   */
  public $conversionType;
  /**
   * @var string
   */
  public $dataStore;
  /**
   * @var bool
   */
  public $directUserRequest;
  protected $documentsType = GoogleCloudDiscoveryengineV1DocumentInfo::class;
  protected $documentsDataType = 'array';
  /**
   * @var string
   */
  public $engine;
  /**
   * @var string
   */
  public $eventTime;
  /**
   * @var string
   */
  public $eventType;
  /**
   * @var string
   */
  public $filter;
  protected $mediaInfoType = GoogleCloudDiscoveryengineV1MediaInfo::class;
  protected $mediaInfoDataType = '';
  protected $pageInfoType = GoogleCloudDiscoveryengineV1PageInfo::class;
  protected $pageInfoDataType = '';
  protected $panelType = GoogleCloudDiscoveryengineV1PanelInfo::class;
  protected $panelDataType = '';
  protected $panelsType = GoogleCloudDiscoveryengineV1PanelInfo::class;
  protected $panelsDataType = 'array';
  /**
   * @var string[]
   */
  public $promotionIds;
  protected $searchInfoType = GoogleCloudDiscoveryengineV1SearchInfo::class;
  protected $searchInfoDataType = '';
  /**
   * @var string
   */
  public $sessionId;
  /**
   * @var string[]
   */
  public $tagIds;
  protected $transactionInfoType = GoogleCloudDiscoveryengineV1TransactionInfo::class;
  protected $transactionInfoDataType = '';
  protected $userInfoType = GoogleCloudDiscoveryengineV1UserInfo::class;
  protected $userInfoDataType = '';
  /**
   * @var string
   */
  public $userPseudoId;

  /**
   * @param GoogleCloudDiscoveryengineV1CustomAttribute[]
   */
  public function setAttributes($attributes)
  {
    $this->attributes = $attributes;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1CustomAttribute[]
   */
  public function getAttributes()
  {
    return $this->attributes;
  }
  /**
   * @param string
   */
  public function setAttributionToken($attributionToken)
  {
    $this->attributionToken = $attributionToken;
  }
  /**
   * @return string
   */
  public function getAttributionToken()
  {
    return $this->attributionToken;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1CompletionInfo
   */
  public function setCompletionInfo(GoogleCloudDiscoveryengineV1CompletionInfo $completionInfo)
  {
    $this->completionInfo = $completionInfo;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1CompletionInfo
   */
  public function getCompletionInfo()
  {
    return $this->completionInfo;
  }
  /**
   * @param string
   */
  public function setConversionType($conversionType)
  {
    $this->conversionType = $conversionType;
  }
  /**
   * @return string
   */
  public function getConversionType()
  {
    return $this->conversionType;
  }
  /**
   * @param string
   */
  public function setDataStore($dataStore)
  {
    $this->dataStore = $dataStore;
  }
  /**
   * @return string
   */
  public function getDataStore()
  {
    return $this->dataStore;
  }
  /**
   * @param bool
   */
  public function setDirectUserRequest($directUserRequest)
  {
    $this->directUserRequest = $directUserRequest;
  }
  /**
   * @return bool
   */
  public function getDirectUserRequest()
  {
    return $this->directUserRequest;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1DocumentInfo[]
   */
  public function setDocuments($documents)
  {
    $this->documents = $documents;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1DocumentInfo[]
   */
  public function getDocuments()
  {
    return $this->documents;
  }
  /**
   * @param string
   */
  public function setEngine($engine)
  {
    $this->engine = $engine;
  }
  /**
   * @return string
   */
  public function getEngine()
  {
    return $this->engine;
  }
  /**
   * @param string
   */
  public function setEventTime($eventTime)
  {
    $this->eventTime = $eventTime;
  }
  /**
   * @return string
   */
  public function getEventTime()
  {
    return $this->eventTime;
  }
  /**
   * @param string
   */
  public function setEventType($eventType)
  {
    $this->eventType = $eventType;
  }
  /**
   * @return string
   */
  public function getEventType()
  {
    return $this->eventType;
  }
  /**
   * @param string
   */
  public function setFilter($filter)
  {
    $this->filter = $filter;
  }
  /**
   * @return string
   */
  public function getFilter()
  {
    return $this->filter;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1MediaInfo
   */
  public function setMediaInfo(GoogleCloudDiscoveryengineV1MediaInfo $mediaInfo)
  {
    $this->mediaInfo = $mediaInfo;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1MediaInfo
   */
  public function getMediaInfo()
  {
    return $this->mediaInfo;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1PageInfo
   */
  public function setPageInfo(GoogleCloudDiscoveryengineV1PageInfo $pageInfo)
  {
    $this->pageInfo = $pageInfo;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1PageInfo
   */
  public function getPageInfo()
  {
    return $this->pageInfo;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1PanelInfo
   */
  public function setPanel(GoogleCloudDiscoveryengineV1PanelInfo $panel)
  {
    $this->panel = $panel;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1PanelInfo
   */
  public function getPanel()
  {
    return $this->panel;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1PanelInfo[]
   */
  public function setPanels($panels)
  {
    $this->panels = $panels;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1PanelInfo[]
   */
  public function getPanels()
  {
    return $this->panels;
  }
  /**
   * @param string[]
   */
  public function setPromotionIds($promotionIds)
  {
    $this->promotionIds = $promotionIds;
  }
  /**
   * @return string[]
   */
  public function getPromotionIds()
  {
    return $this->promotionIds;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SearchInfo
   */
  public function setSearchInfo(GoogleCloudDiscoveryengineV1SearchInfo $searchInfo)
  {
    $this->searchInfo = $searchInfo;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchInfo
   */
  public function getSearchInfo()
  {
    return $this->searchInfo;
  }
  /**
   * @param string
   */
  public function setSessionId($sessionId)
  {
    $this->sessionId = $sessionId;
  }
  /**
   * @return string
   */
  public function getSessionId()
  {
    return $this->sessionId;
  }
  /**
   * @param string[]
   */
  public function setTagIds($tagIds)
  {
    $this->tagIds = $tagIds;
  }
  /**
   * @return string[]
   */
  public function getTagIds()
  {
    return $this->tagIds;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1TransactionInfo
   */
  public function setTransactionInfo(GoogleCloudDiscoveryengineV1TransactionInfo $transactionInfo)
  {
    $this->transactionInfo = $transactionInfo;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1TransactionInfo
   */
  public function getTransactionInfo()
  {
    return $this->transactionInfo;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1UserInfo
   */
  public function setUserInfo(GoogleCloudDiscoveryengineV1UserInfo $userInfo)
  {
    $this->userInfo = $userInfo;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1UserInfo
   */
  public function getUserInfo()
  {
    return $this->userInfo;
  }
  /**
   * @param string
   */
  public function setUserPseudoId($userPseudoId)
  {
    $this->userPseudoId = $userPseudoId;
  }
  /**
   * @return string
   */
  public function getUserPseudoId()
  {
    return $this->userPseudoId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1UserEvent::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1UserEvent');
