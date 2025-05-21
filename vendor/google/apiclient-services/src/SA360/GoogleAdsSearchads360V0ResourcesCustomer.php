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

class GoogleAdsSearchads360V0ResourcesCustomer extends \Google\Model
{
  /**
   * @var string
   */
  public $accountStatus;
  /**
   * @var string
   */
  public $accountType;
  /**
   * @var bool
   */
  public $autoTaggingEnabled;
  protected $conversionTrackingSettingType = GoogleAdsSearchads360V0ResourcesConversionTrackingSetting::class;
  protected $conversionTrackingSettingDataType = '';
  /**
   * @var string
   */
  public $creationTime;
  /**
   * @var string
   */
  public $currencyCode;
  /**
   * @var string
   */
  public $descriptiveName;
  protected $doubleClickCampaignManagerSettingType = GoogleAdsSearchads360V0ResourcesDoubleClickCampaignManagerSetting::class;
  protected $doubleClickCampaignManagerSettingDataType = '';
  /**
   * @var string
   */
  public $engineId;
  /**
   * @var string
   */
  public $finalUrlSuffix;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $lastModifiedTime;
  /**
   * @var bool
   */
  public $manager;
  /**
   * @var string
   */
  public $resourceName;
  /**
   * @var string
   */
  public $status;
  /**
   * @var string
   */
  public $timeZone;
  /**
   * @var string
   */
  public $trackingUrlTemplate;

  /**
   * @param string
   */
  public function setAccountStatus($accountStatus)
  {
    $this->accountStatus = $accountStatus;
  }
  /**
   * @return string
   */
  public function getAccountStatus()
  {
    return $this->accountStatus;
  }
  /**
   * @param string
   */
  public function setAccountType($accountType)
  {
    $this->accountType = $accountType;
  }
  /**
   * @return string
   */
  public function getAccountType()
  {
    return $this->accountType;
  }
  /**
   * @param bool
   */
  public function setAutoTaggingEnabled($autoTaggingEnabled)
  {
    $this->autoTaggingEnabled = $autoTaggingEnabled;
  }
  /**
   * @return bool
   */
  public function getAutoTaggingEnabled()
  {
    return $this->autoTaggingEnabled;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesConversionTrackingSetting
   */
  public function setConversionTrackingSetting(GoogleAdsSearchads360V0ResourcesConversionTrackingSetting $conversionTrackingSetting)
  {
    $this->conversionTrackingSetting = $conversionTrackingSetting;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesConversionTrackingSetting
   */
  public function getConversionTrackingSetting()
  {
    return $this->conversionTrackingSetting;
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
  public function setCurrencyCode($currencyCode)
  {
    $this->currencyCode = $currencyCode;
  }
  /**
   * @return string
   */
  public function getCurrencyCode()
  {
    return $this->currencyCode;
  }
  /**
   * @param string
   */
  public function setDescriptiveName($descriptiveName)
  {
    $this->descriptiveName = $descriptiveName;
  }
  /**
   * @return string
   */
  public function getDescriptiveName()
  {
    return $this->descriptiveName;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesDoubleClickCampaignManagerSetting
   */
  public function setDoubleClickCampaignManagerSetting(GoogleAdsSearchads360V0ResourcesDoubleClickCampaignManagerSetting $doubleClickCampaignManagerSetting)
  {
    $this->doubleClickCampaignManagerSetting = $doubleClickCampaignManagerSetting;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesDoubleClickCampaignManagerSetting
   */
  public function getDoubleClickCampaignManagerSetting()
  {
    return $this->doubleClickCampaignManagerSetting;
  }
  /**
   * @param string
   */
  public function setEngineId($engineId)
  {
    $this->engineId = $engineId;
  }
  /**
   * @return string
   */
  public function getEngineId()
  {
    return $this->engineId;
  }
  /**
   * @param string
   */
  public function setFinalUrlSuffix($finalUrlSuffix)
  {
    $this->finalUrlSuffix = $finalUrlSuffix;
  }
  /**
   * @return string
   */
  public function getFinalUrlSuffix()
  {
    return $this->finalUrlSuffix;
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
   * @param bool
   */
  public function setManager($manager)
  {
    $this->manager = $manager;
  }
  /**
   * @return bool
   */
  public function getManager()
  {
    return $this->manager;
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
   * @param string
   */
  public function setTimeZone($timeZone)
  {
    $this->timeZone = $timeZone;
  }
  /**
   * @return string
   */
  public function getTimeZone()
  {
    return $this->timeZone;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ResourcesCustomer::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ResourcesCustomer');
