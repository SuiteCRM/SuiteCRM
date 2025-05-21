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

class GoogleAdsSearchads360V0ResourcesCampaign extends \Google\Collection
{
  protected $collection_key = 'urlCustomParameters';
  /**
   * @var string
   */
  public $accessibleBiddingStrategy;
  /**
   * @var string
   */
  public $adServingOptimizationStatus;
  /**
   * @var string
   */
  public $advertisingChannelSubType;
  /**
   * @var string
   */
  public $advertisingChannelType;
  /**
   * @var string
   */
  public $biddingStrategy;
  /**
   * @var string
   */
  public $biddingStrategySystemStatus;
  /**
   * @var string
   */
  public $biddingStrategyType;
  /**
   * @var string
   */
  public $campaignBudget;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $creationTime;
  protected $dynamicSearchAdsSettingType = GoogleAdsSearchads360V0ResourcesCampaignDynamicSearchAdsSetting::class;
  protected $dynamicSearchAdsSettingDataType = '';
  /**
   * @var string[]
   */
  public $effectiveLabels;
  /**
   * @var string
   */
  public $endDate;
  /**
   * @var string
   */
  public $engineId;
  /**
   * @var string[]
   */
  public $excludedParentAssetFieldTypes;
  /**
   * @var string
   */
  public $finalUrlSuffix;
  protected $frequencyCapsType = GoogleAdsSearchads360V0CommonFrequencyCapEntry::class;
  protected $frequencyCapsDataType = 'array';
  protected $geoTargetTypeSettingType = GoogleAdsSearchads360V0ResourcesCampaignGeoTargetTypeSetting::class;
  protected $geoTargetTypeSettingDataType = '';
  /**
   * @var string
   */
  public $id;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $lastModifiedTime;
  protected $manualCpaType = GoogleAdsSearchads360V0CommonManualCpa::class;
  protected $manualCpaDataType = '';
  protected $manualCpcType = GoogleAdsSearchads360V0CommonManualCpc::class;
  protected $manualCpcDataType = '';
  protected $manualCpmType = GoogleAdsSearchads360V0CommonManualCpm::class;
  protected $manualCpmDataType = '';
  protected $maximizeConversionValueType = GoogleAdsSearchads360V0CommonMaximizeConversionValue::class;
  protected $maximizeConversionValueDataType = '';
  protected $maximizeConversionsType = GoogleAdsSearchads360V0CommonMaximizeConversions::class;
  protected $maximizeConversionsDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $networkSettingsType = GoogleAdsSearchads360V0ResourcesCampaignNetworkSettings::class;
  protected $networkSettingsDataType = '';
  protected $optimizationGoalSettingType = GoogleAdsSearchads360V0ResourcesCampaignOptimizationGoalSetting::class;
  protected $optimizationGoalSettingDataType = '';
  protected $percentCpcType = GoogleAdsSearchads360V0CommonPercentCpc::class;
  protected $percentCpcDataType = '';
  protected $realTimeBiddingSettingType = GoogleAdsSearchads360V0CommonRealTimeBiddingSetting::class;
  protected $realTimeBiddingSettingDataType = '';
  /**
   * @var string
   */
  public $resourceName;
  protected $selectiveOptimizationType = GoogleAdsSearchads360V0ResourcesCampaignSelectiveOptimization::class;
  protected $selectiveOptimizationDataType = '';
  /**
   * @var string
   */
  public $servingStatus;
  protected $shoppingSettingType = GoogleAdsSearchads360V0ResourcesCampaignShoppingSetting::class;
  protected $shoppingSettingDataType = '';
  /**
   * @var string
   */
  public $startDate;
  /**
   * @var string
   */
  public $status;
  protected $targetCpaType = GoogleAdsSearchads360V0CommonTargetCpa::class;
  protected $targetCpaDataType = '';
  protected $targetCpmType = GoogleAdsSearchads360V0CommonTargetCpm::class;
  protected $targetCpmDataType = '';
  protected $targetImpressionShareType = GoogleAdsSearchads360V0CommonTargetImpressionShare::class;
  protected $targetImpressionShareDataType = '';
  protected $targetRoasType = GoogleAdsSearchads360V0CommonTargetRoas::class;
  protected $targetRoasDataType = '';
  protected $targetSpendType = GoogleAdsSearchads360V0CommonTargetSpend::class;
  protected $targetSpendDataType = '';
  protected $trackingSettingType = GoogleAdsSearchads360V0ResourcesCampaignTrackingSetting::class;
  protected $trackingSettingDataType = '';
  /**
   * @var string
   */
  public $trackingUrlTemplate;
  protected $urlCustomParametersType = GoogleAdsSearchads360V0CommonCustomParameter::class;
  protected $urlCustomParametersDataType = 'array';
  /**
   * @var bool
   */
  public $urlExpansionOptOut;

  /**
   * @param string
   */
  public function setAccessibleBiddingStrategy($accessibleBiddingStrategy)
  {
    $this->accessibleBiddingStrategy = $accessibleBiddingStrategy;
  }
  /**
   * @return string
   */
  public function getAccessibleBiddingStrategy()
  {
    return $this->accessibleBiddingStrategy;
  }
  /**
   * @param string
   */
  public function setAdServingOptimizationStatus($adServingOptimizationStatus)
  {
    $this->adServingOptimizationStatus = $adServingOptimizationStatus;
  }
  /**
   * @return string
   */
  public function getAdServingOptimizationStatus()
  {
    return $this->adServingOptimizationStatus;
  }
  /**
   * @param string
   */
  public function setAdvertisingChannelSubType($advertisingChannelSubType)
  {
    $this->advertisingChannelSubType = $advertisingChannelSubType;
  }
  /**
   * @return string
   */
  public function getAdvertisingChannelSubType()
  {
    return $this->advertisingChannelSubType;
  }
  /**
   * @param string
   */
  public function setAdvertisingChannelType($advertisingChannelType)
  {
    $this->advertisingChannelType = $advertisingChannelType;
  }
  /**
   * @return string
   */
  public function getAdvertisingChannelType()
  {
    return $this->advertisingChannelType;
  }
  /**
   * @param string
   */
  public function setBiddingStrategy($biddingStrategy)
  {
    $this->biddingStrategy = $biddingStrategy;
  }
  /**
   * @return string
   */
  public function getBiddingStrategy()
  {
    return $this->biddingStrategy;
  }
  /**
   * @param string
   */
  public function setBiddingStrategySystemStatus($biddingStrategySystemStatus)
  {
    $this->biddingStrategySystemStatus = $biddingStrategySystemStatus;
  }
  /**
   * @return string
   */
  public function getBiddingStrategySystemStatus()
  {
    return $this->biddingStrategySystemStatus;
  }
  /**
   * @param string
   */
  public function setBiddingStrategyType($biddingStrategyType)
  {
    $this->biddingStrategyType = $biddingStrategyType;
  }
  /**
   * @return string
   */
  public function getBiddingStrategyType()
  {
    return $this->biddingStrategyType;
  }
  /**
   * @param string
   */
  public function setCampaignBudget($campaignBudget)
  {
    $this->campaignBudget = $campaignBudget;
  }
  /**
   * @return string
   */
  public function getCampaignBudget()
  {
    return $this->campaignBudget;
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
   * @param GoogleAdsSearchads360V0ResourcesCampaignDynamicSearchAdsSetting
   */
  public function setDynamicSearchAdsSetting(GoogleAdsSearchads360V0ResourcesCampaignDynamicSearchAdsSetting $dynamicSearchAdsSetting)
  {
    $this->dynamicSearchAdsSetting = $dynamicSearchAdsSetting;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCampaignDynamicSearchAdsSetting
   */
  public function getDynamicSearchAdsSetting()
  {
    return $this->dynamicSearchAdsSetting;
  }
  /**
   * @param string[]
   */
  public function setEffectiveLabels($effectiveLabels)
  {
    $this->effectiveLabels = $effectiveLabels;
  }
  /**
   * @return string[]
   */
  public function getEffectiveLabels()
  {
    return $this->effectiveLabels;
  }
  /**
   * @param string
   */
  public function setEndDate($endDate)
  {
    $this->endDate = $endDate;
  }
  /**
   * @return string
   */
  public function getEndDate()
  {
    return $this->endDate;
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
   * @param string[]
   */
  public function setExcludedParentAssetFieldTypes($excludedParentAssetFieldTypes)
  {
    $this->excludedParentAssetFieldTypes = $excludedParentAssetFieldTypes;
  }
  /**
   * @return string[]
   */
  public function getExcludedParentAssetFieldTypes()
  {
    return $this->excludedParentAssetFieldTypes;
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
   * @param GoogleAdsSearchads360V0CommonFrequencyCapEntry[]
   */
  public function setFrequencyCaps($frequencyCaps)
  {
    $this->frequencyCaps = $frequencyCaps;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonFrequencyCapEntry[]
   */
  public function getFrequencyCaps()
  {
    return $this->frequencyCaps;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesCampaignGeoTargetTypeSetting
   */
  public function setGeoTargetTypeSetting(GoogleAdsSearchads360V0ResourcesCampaignGeoTargetTypeSetting $geoTargetTypeSetting)
  {
    $this->geoTargetTypeSetting = $geoTargetTypeSetting;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCampaignGeoTargetTypeSetting
   */
  public function getGeoTargetTypeSetting()
  {
    return $this->geoTargetTypeSetting;
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
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
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
   * @param GoogleAdsSearchads360V0CommonManualCpa
   */
  public function setManualCpa(GoogleAdsSearchads360V0CommonManualCpa $manualCpa)
  {
    $this->manualCpa = $manualCpa;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonManualCpa
   */
  public function getManualCpa()
  {
    return $this->manualCpa;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonManualCpc
   */
  public function setManualCpc(GoogleAdsSearchads360V0CommonManualCpc $manualCpc)
  {
    $this->manualCpc = $manualCpc;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonManualCpc
   */
  public function getManualCpc()
  {
    return $this->manualCpc;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonManualCpm
   */
  public function setManualCpm(GoogleAdsSearchads360V0CommonManualCpm $manualCpm)
  {
    $this->manualCpm = $manualCpm;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonManualCpm
   */
  public function getManualCpm()
  {
    return $this->manualCpm;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonMaximizeConversionValue
   */
  public function setMaximizeConversionValue(GoogleAdsSearchads360V0CommonMaximizeConversionValue $maximizeConversionValue)
  {
    $this->maximizeConversionValue = $maximizeConversionValue;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonMaximizeConversionValue
   */
  public function getMaximizeConversionValue()
  {
    return $this->maximizeConversionValue;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonMaximizeConversions
   */
  public function setMaximizeConversions(GoogleAdsSearchads360V0CommonMaximizeConversions $maximizeConversions)
  {
    $this->maximizeConversions = $maximizeConversions;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonMaximizeConversions
   */
  public function getMaximizeConversions()
  {
    return $this->maximizeConversions;
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
   * @param GoogleAdsSearchads360V0ResourcesCampaignNetworkSettings
   */
  public function setNetworkSettings(GoogleAdsSearchads360V0ResourcesCampaignNetworkSettings $networkSettings)
  {
    $this->networkSettings = $networkSettings;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCampaignNetworkSettings
   */
  public function getNetworkSettings()
  {
    return $this->networkSettings;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesCampaignOptimizationGoalSetting
   */
  public function setOptimizationGoalSetting(GoogleAdsSearchads360V0ResourcesCampaignOptimizationGoalSetting $optimizationGoalSetting)
  {
    $this->optimizationGoalSetting = $optimizationGoalSetting;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCampaignOptimizationGoalSetting
   */
  public function getOptimizationGoalSetting()
  {
    return $this->optimizationGoalSetting;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonPercentCpc
   */
  public function setPercentCpc(GoogleAdsSearchads360V0CommonPercentCpc $percentCpc)
  {
    $this->percentCpc = $percentCpc;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonPercentCpc
   */
  public function getPercentCpc()
  {
    return $this->percentCpc;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonRealTimeBiddingSetting
   */
  public function setRealTimeBiddingSetting(GoogleAdsSearchads360V0CommonRealTimeBiddingSetting $realTimeBiddingSetting)
  {
    $this->realTimeBiddingSetting = $realTimeBiddingSetting;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonRealTimeBiddingSetting
   */
  public function getRealTimeBiddingSetting()
  {
    return $this->realTimeBiddingSetting;
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
   * @param GoogleAdsSearchads360V0ResourcesCampaignSelectiveOptimization
   */
  public function setSelectiveOptimization(GoogleAdsSearchads360V0ResourcesCampaignSelectiveOptimization $selectiveOptimization)
  {
    $this->selectiveOptimization = $selectiveOptimization;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCampaignSelectiveOptimization
   */
  public function getSelectiveOptimization()
  {
    return $this->selectiveOptimization;
  }
  /**
   * @param string
   */
  public function setServingStatus($servingStatus)
  {
    $this->servingStatus = $servingStatus;
  }
  /**
   * @return string
   */
  public function getServingStatus()
  {
    return $this->servingStatus;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesCampaignShoppingSetting
   */
  public function setShoppingSetting(GoogleAdsSearchads360V0ResourcesCampaignShoppingSetting $shoppingSetting)
  {
    $this->shoppingSetting = $shoppingSetting;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCampaignShoppingSetting
   */
  public function getShoppingSetting()
  {
    return $this->shoppingSetting;
  }
  /**
   * @param string
   */
  public function setStartDate($startDate)
  {
    $this->startDate = $startDate;
  }
  /**
   * @return string
   */
  public function getStartDate()
  {
    return $this->startDate;
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
   * @param GoogleAdsSearchads360V0CommonTargetCpa
   */
  public function setTargetCpa(GoogleAdsSearchads360V0CommonTargetCpa $targetCpa)
  {
    $this->targetCpa = $targetCpa;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonTargetCpa
   */
  public function getTargetCpa()
  {
    return $this->targetCpa;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonTargetCpm
   */
  public function setTargetCpm(GoogleAdsSearchads360V0CommonTargetCpm $targetCpm)
  {
    $this->targetCpm = $targetCpm;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonTargetCpm
   */
  public function getTargetCpm()
  {
    return $this->targetCpm;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonTargetImpressionShare
   */
  public function setTargetImpressionShare(GoogleAdsSearchads360V0CommonTargetImpressionShare $targetImpressionShare)
  {
    $this->targetImpressionShare = $targetImpressionShare;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonTargetImpressionShare
   */
  public function getTargetImpressionShare()
  {
    return $this->targetImpressionShare;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonTargetRoas
   */
  public function setTargetRoas(GoogleAdsSearchads360V0CommonTargetRoas $targetRoas)
  {
    $this->targetRoas = $targetRoas;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonTargetRoas
   */
  public function getTargetRoas()
  {
    return $this->targetRoas;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonTargetSpend
   */
  public function setTargetSpend(GoogleAdsSearchads360V0CommonTargetSpend $targetSpend)
  {
    $this->targetSpend = $targetSpend;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonTargetSpend
   */
  public function getTargetSpend()
  {
    return $this->targetSpend;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesCampaignTrackingSetting
   */
  public function setTrackingSetting(GoogleAdsSearchads360V0ResourcesCampaignTrackingSetting $trackingSetting)
  {
    $this->trackingSetting = $trackingSetting;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCampaignTrackingSetting
   */
  public function getTrackingSetting()
  {
    return $this->trackingSetting;
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
   * @param GoogleAdsSearchads360V0CommonCustomParameter[]
   */
  public function setUrlCustomParameters($urlCustomParameters)
  {
    $this->urlCustomParameters = $urlCustomParameters;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonCustomParameter[]
   */
  public function getUrlCustomParameters()
  {
    return $this->urlCustomParameters;
  }
  /**
   * @param bool
   */
  public function setUrlExpansionOptOut($urlExpansionOptOut)
  {
    $this->urlExpansionOptOut = $urlExpansionOptOut;
  }
  /**
   * @return bool
   */
  public function getUrlExpansionOptOut()
  {
    return $this->urlExpansionOptOut;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ResourcesCampaign::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ResourcesCampaign');
