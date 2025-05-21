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

class Subscription extends \Google\Collection
{
  protected $collection_key = 'linkedResources';
  protected $commercialInfoType = GoogleCloudBigqueryAnalyticshubV1SubscriptionCommercialInfo::class;
  protected $commercialInfoDataType = '';
  /**
   * @var string
   */
  public $creationTime;
  /**
   * @var string
   */
  public $dataExchange;
  /**
   * @var string
   */
  public $lastModifyTime;
  protected $linkedDatasetMapType = LinkedResource::class;
  protected $linkedDatasetMapDataType = 'map';
  protected $linkedResourcesType = LinkedResource::class;
  protected $linkedResourcesDataType = 'array';
  /**
   * @var string
   */
  public $listing;
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
  public $organizationDisplayName;
  /**
   * @var string
   */
  public $organizationId;
  /**
   * @var string
   */
  public $resourceType;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $subscriberContact;

  /**
   * @param GoogleCloudBigqueryAnalyticshubV1SubscriptionCommercialInfo
   */
  public function setCommercialInfo(GoogleCloudBigqueryAnalyticshubV1SubscriptionCommercialInfo $commercialInfo)
  {
    $this->commercialInfo = $commercialInfo;
  }
  /**
   * @return GoogleCloudBigqueryAnalyticshubV1SubscriptionCommercialInfo
   */
  public function getCommercialInfo()
  {
    return $this->commercialInfo;
  }
  /**
   * @param string
   */
  public function setCreationTime($creationTime)
  {
    $this->creationTime = $creationTime;
  }
  /**
   * @return string
   */
  public function getCreationTime()
  {
    return $this->creationTime;
  }
  /**
   * @param string
   */
  public function setDataExchange($dataExchange)
  {
    $this->dataExchange = $dataExchange;
  }
  /**
   * @return string
   */
  public function getDataExchange()
  {
    return $this->dataExchange;
  }
  /**
   * @param string
   */
  public function setLastModifyTime($lastModifyTime)
  {
    $this->lastModifyTime = $lastModifyTime;
  }
  /**
   * @return string
   */
  public function getLastModifyTime()
  {
    return $this->lastModifyTime;
  }
  /**
   * @param LinkedResource[]
   */
  public function setLinkedDatasetMap($linkedDatasetMap)
  {
    $this->linkedDatasetMap = $linkedDatasetMap;
  }
  /**
   * @return LinkedResource[]
   */
  public function getLinkedDatasetMap()
  {
    return $this->linkedDatasetMap;
  }
  /**
   * @param LinkedResource[]
   */
  public function setLinkedResources($linkedResources)
  {
    $this->linkedResources = $linkedResources;
  }
  /**
   * @return LinkedResource[]
   */
  public function getLinkedResources()
  {
    return $this->linkedResources;
  }
  /**
   * @param string
   */
  public function setListing($listing)
  {
    $this->listing = $listing;
  }
  /**
   * @return string
   */
  public function getListing()
  {
    return $this->listing;
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
  public function setOrganizationDisplayName($organizationDisplayName)
  {
    $this->organizationDisplayName = $organizationDisplayName;
  }
  /**
   * @return string
   */
  public function getOrganizationDisplayName()
  {
    return $this->organizationDisplayName;
  }
  /**
   * @param string
   */
  public function setOrganizationId($organizationId)
  {
    $this->organizationId = $organizationId;
  }
  /**
   * @return string
   */
  public function getOrganizationId()
  {
    return $this->organizationId;
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
  /**
   * @param string
   */
  public function setSubscriberContact($subscriberContact)
  {
    $this->subscriberContact = $subscriberContact;
  }
  /**
   * @return string
   */
  public function getSubscriberContact()
  {
    return $this->subscriberContact;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Subscription::class, 'Google_Service_AnalyticsHub_Subscription');
