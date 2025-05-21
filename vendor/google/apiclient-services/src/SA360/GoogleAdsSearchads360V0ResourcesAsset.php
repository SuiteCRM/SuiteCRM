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

namespace Google\Service\SA360;

class GoogleAdsSearchads360V0ResourcesAsset extends \Google\Collection
{
  protected $collection_key = 'finalUrls';
  protected $callAssetType = GoogleAdsSearchads360V0CommonUnifiedCallAsset::class;
  protected $callAssetDataType = '';
  protected $callToActionAssetType = GoogleAdsSearchads360V0CommonCallToActionAsset::class;
  protected $callToActionAssetDataType = '';
  protected $calloutAssetType = GoogleAdsSearchads360V0CommonUnifiedCalloutAsset::class;
  protected $calloutAssetDataType = '';
  /**
   * @var string
   */
  public $creationTime;
  /**
   * @var string
   */
  public $engineStatus;
  /**
   * @var string[]
   */
  public $finalUrls;
  /**
   * @var string
   */
  public $id;
  protected $imageAssetType = GoogleAdsSearchads360V0CommonImageAsset::class;
  protected $imageAssetDataType = '';
  /**
   * @var string
   */
  public $lastModifiedTime;
  protected $locationAssetType = GoogleAdsSearchads360V0CommonUnifiedLocationAsset::class;
  protected $locationAssetDataType = '';
  protected $mobileAppAssetType = GoogleAdsSearchads360V0CommonMobileAppAsset::class;
  protected $mobileAppAssetDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $pageFeedAssetType = GoogleAdsSearchads360V0CommonUnifiedPageFeedAsset::class;
  protected $pageFeedAssetDataType = '';
  /**
   * @var string
   */
  public $resourceName;
  protected $sitelinkAssetType = GoogleAdsSearchads360V0CommonUnifiedSitelinkAsset::class;
  protected $sitelinkAssetDataType = '';
  /**
   * @var string
   */
  public $status;
  protected $textAssetType = GoogleAdsSearchads360V0CommonTextAsset::class;
  protected $textAssetDataType = '';
  /**
   * @var string
   */
  public $trackingUrlTemplate;
  /**
   * @var string
   */
  public $type;
  protected $youtubeVideoAssetType = GoogleAdsSearchads360V0CommonYoutubeVideoAsset::class;
  protected $youtubeVideoAssetDataType = '';

  /**
   * @param GoogleAdsSearchads360V0CommonUnifiedCallAsset
   */
  public function setCallAsset(GoogleAdsSearchads360V0CommonUnifiedCallAsset $callAsset)
  {
    $this->callAsset = $callAsset;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonUnifiedCallAsset
   */
  public function getCallAsset()
  {
    return $this->callAsset;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonCallToActionAsset
   */
  public function setCallToActionAsset(GoogleAdsSearchads360V0CommonCallToActionAsset $callToActionAsset)
  {
    $this->callToActionAsset = $callToActionAsset;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonCallToActionAsset
   */
  public function getCallToActionAsset()
  {
    return $this->callToActionAsset;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonUnifiedCalloutAsset
   */
  public function setCalloutAsset(GoogleAdsSearchads360V0CommonUnifiedCalloutAsset $calloutAsset)
  {
    $this->calloutAsset = $calloutAsset;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonUnifiedCalloutAsset
   */
  public function getCalloutAsset()
  {
    return $this->calloutAsset;
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
  public function setEngineStatus($engineStatus)
  {
    $this->engineStatus = $engineStatus;
  }
  /**
   * @return string
   */
  public function getEngineStatus()
  {
    return $this->engineStatus;
  }
  /**
   * @param string[]
   */
  public function setFinalUrls($finalUrls)
  {
    $this->finalUrls = $finalUrls;
  }
  /**
   * @return string[]
   */
  public function getFinalUrls()
  {
    return $this->finalUrls;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonImageAsset
   */
  public function setImageAsset(GoogleAdsSearchads360V0CommonImageAsset $imageAsset)
  {
    $this->imageAsset = $imageAsset;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonImageAsset
   */
  public function getImageAsset()
  {
    return $this->imageAsset;
  }
  /**
   * @param string
   */
  public function setLastModifiedTime($lastModifiedTime)
  {
    $this->lastModifiedTime = $lastModifiedTime;
  }
  /**
   * @return string
   */
  public function getLastModifiedTime()
  {
    return $this->lastModifiedTime;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonUnifiedLocationAsset
   */
  public function setLocationAsset(GoogleAdsSearchads360V0CommonUnifiedLocationAsset $locationAsset)
  {
    $this->locationAsset = $locationAsset;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonUnifiedLocationAsset
   */
  public function getLocationAsset()
  {
    return $this->locationAsset;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonMobileAppAsset
   */
  public function setMobileAppAsset(GoogleAdsSearchads360V0CommonMobileAppAsset $mobileAppAsset)
  {
    $this->mobileAppAsset = $mobileAppAsset;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonMobileAppAsset
   */
  public function getMobileAppAsset()
  {
    return $this->mobileAppAsset;
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
   * @param GoogleAdsSearchads360V0CommonUnifiedPageFeedAsset
   */
  public function setPageFeedAsset(GoogleAdsSearchads360V0CommonUnifiedPageFeedAsset $pageFeedAsset)
  {
    $this->pageFeedAsset = $pageFeedAsset;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonUnifiedPageFeedAsset
   */
  public function getPageFeedAsset()
  {
    return $this->pageFeedAsset;
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
   * @param GoogleAdsSearchads360V0CommonUnifiedSitelinkAsset
   */
  public function setSitelinkAsset(GoogleAdsSearchads360V0CommonUnifiedSitelinkAsset $sitelinkAsset)
  {
    $this->sitelinkAsset = $sitelinkAsset;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonUnifiedSitelinkAsset
   */
  public function getSitelinkAsset()
  {
    return $this->sitelinkAsset;
  }
  /**
   * @param string
   */
  public function setStatus($status)
  {
    $this->status = $status;
  }
  /**
   * @return string
   */
  public function getStatus()
  {
    return $this->status;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonTextAsset
   */
  public function setTextAsset(GoogleAdsSearchads360V0CommonTextAsset $textAsset)
  {
    $this->textAsset = $textAsset;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonTextAsset
   */
  public function getTextAsset()
  {
    return $this->textAsset;
  }
  /**
   * @param string
   */
  public function setTrackingUrlTemplate($trackingUrlTemplate)
  {
    $this->trackingUrlTemplate = $trackingUrlTemplate;
  }
  /**
   * @return string
   */
  public function getTrackingUrlTemplate()
  {
    return $this->trackingUrlTemplate;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonYoutubeVideoAsset
   */
  public function setYoutubeVideoAsset(GoogleAdsSearchads360V0CommonYoutubeVideoAsset $youtubeVideoAsset)
  {
    $this->youtubeVideoAsset = $youtubeVideoAsset;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonYoutubeVideoAsset
   */
  public function getYoutubeVideoAsset()
  {
    return $this->youtubeVideoAsset;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ResourcesAsset::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ResourcesAsset');
