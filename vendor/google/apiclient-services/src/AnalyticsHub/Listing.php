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

namespace Google\Service\AnalyticsHub;

class Listing extends \Google\Collection
{
  protected $collection_key = 'categories';
  protected $bigqueryDatasetType = BigQueryDatasetSource::class;
  protected $bigqueryDatasetDataType = '';
  /**
   * @var string[]
   */
  public $categories;
  protected $commercialInfoType = GoogleCloudBigqueryAnalyticshubV1ListingCommercialInfo::class;
  protected $commercialInfoDataType = '';
  protected $dataProviderType = DataProvider::class;
  protected $dataProviderDataType = '';
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $discoveryType;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $documentation;
  /**
   * @var string
   */
  public $icon;
  /**
   * @var bool
   */
  public $logLinkedDatasetQueryUserEmail;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $primaryContact;
  protected $publisherType = Publisher::class;
  protected $publisherDataType = '';
  protected $pubsubTopicType = PubSubTopicSource::class;
  protected $pubsubTopicDataType = '';
  /**
   * @var string
   */
  public $requestAccess;
  /**
   * @var string
   */
  public $resourceType;
  protected $restrictedExportConfigType = RestrictedExportConfig::class;
  protected $restrictedExportConfigDataType = '';
  /**
   * @var string
   */
  public $state;

  /**
   * @param BigQueryDatasetSource
   */
  public function setBigqueryDataset(BigQueryDatasetSource $bigqueryDataset)
  {
    $this->bigqueryDataset = $bigqueryDataset;
  }
  /**
   * @return BigQueryDatasetSource
   */
  public function getBigqueryDataset()
  {
    return $this->bigqueryDataset;
  }
  /**
   * @param string[]
   */
  public function setCategories($categories)
  {
    $this->categories = $categories;
  }
  /**
   * @return string[]
   */
  public function getCategories()
  {
    return $this->categories;
  }
  /**
   * @param GoogleCloudBigqueryAnalyticshubV1ListingCommercialInfo
   */
  public function setCommercialInfo(GoogleCloudBigqueryAnalyticshubV1ListingCommercialInfo $commercialInfo)
  {
    $this->commercialInfo = $commercialInfo;
  }
  /**
   * @return GoogleCloudBigqueryAnalyticshubV1ListingCommercialInfo
   */
  public function getCommercialInfo()
  {
    return $this->commercialInfo;
  }
  /**
   * @param DataProvider
   */
  public function setDataProvider(DataProvider $dataProvider)
  {
    $this->dataProvider = $dataProvider;
  }
  /**
   * @return DataProvider
   */
  public function getDataProvider()
  {
    return $this->dataProvider;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setDiscoveryType($discoveryType)
  {
    $this->discoveryType = $discoveryType;
  }
  /**
   * @return string
   */
  public function getDiscoveryType()
  {
    return $this->discoveryType;
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
  public function setDocumentation($documentation)
  {
    $this->documentation = $documentation;
  }
  /**
   * @return string
   */
  public function getDocumentation()
  {
    return $this->documentation;
  }
  /**
   * @param string
   */
  public function setIcon($icon)
  {
    $this->icon = $icon;
  }
  /**
   * @return string
   */
  public function getIcon()
  {
    return $this->icon;
  }
  /**
   * @param bool
   */
  public function setLogLinkedDatasetQueryUserEmail($logLinkedDatasetQueryUserEmail)
  {
    $this->logLinkedDatasetQueryUserEmail = $logLinkedDatasetQueryUserEmail;
  }
  /**
   * @return bool
   */
  public function getLogLinkedDatasetQueryUserEmail()
  {
    return $this->logLinkedDatasetQueryUserEmail;
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
   * @param string
   */
  public function setPrimaryContact($primaryContact)
  {
    $this->primaryContact = $primaryContact;
  }
  /**
   * @return string
   */
  public function getPrimaryContact()
  {
    return $this->primaryContact;
  }
  /**
   * @param Publisher
   */
  public function setPublisher(Publisher $publisher)
  {
    $this->publisher = $publisher;
  }
  /**
   * @return Publisher
   */
  public function getPublisher()
  {
    return $this->publisher;
  }
  /**
   * @param PubSubTopicSource
   */
  public function setPubsubTopic(PubSubTopicSource $pubsubTopic)
  {
    $this->pubsubTopic = $pubsubTopic;
  }
  /**
   * @return PubSubTopicSource
   */
  public function getPubsubTopic()
  {
    return $this->pubsubTopic;
  }
  /**
   * @param string
   */
  public function setRequestAccess($requestAccess)
  {
    $this->requestAccess = $requestAccess;
  }
  /**
   * @return string
   */
  public function getRequestAccess()
  {
    return $this->requestAccess;
  }
  /**
   * @param string
   */
  public function setResourceType($resourceType)
  {
    $this->resourceType = $resourceType;
  }
  /**
   * @return string
   */
  public function getResourceType()
  {
    return $this->resourceType;
  }
  /**
   * @param RestrictedExportConfig
   */
  public function setRestrictedExportConfig(RestrictedExportConfig $restrictedExportConfig)
  {
    $this->restrictedExportConfig = $restrictedExportConfig;
  }
  /**
   * @return RestrictedExportConfig
   */
  public function getRestrictedExportConfig()
  {
    return $this->restrictedExportConfig;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Listing::class, 'Google_Service_AnalyticsHub_Listing');
