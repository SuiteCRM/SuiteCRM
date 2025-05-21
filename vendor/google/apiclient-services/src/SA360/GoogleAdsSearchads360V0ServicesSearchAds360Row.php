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

class GoogleAdsSearchads360V0ServicesSearchAds360Row extends \Google\Collection
{
  protected $collection_key = 'customColumns';
  protected $accessibleBiddingStrategyType = GoogleAdsSearchads360V0ResourcesAccessibleBiddingStrategy::class;
  protected $accessibleBiddingStrategyDataType = '';
  protected $adGroupType = GoogleAdsSearchads360V0ResourcesAdGroup::class;
  protected $adGroupDataType = '';
  protected $adGroupAdType = GoogleAdsSearchads360V0ResourcesAdGroupAd::class;
  protected $adGroupAdDataType = '';
  protected $adGroupAdEffectiveLabelType = GoogleAdsSearchads360V0ResourcesAdGroupAdEffectiveLabel::class;
  protected $adGroupAdEffectiveLabelDataType = '';
  protected $adGroupAdLabelType = GoogleAdsSearchads360V0ResourcesAdGroupAdLabel::class;
  protected $adGroupAdLabelDataType = '';
  protected $adGroupAssetType = GoogleAdsSearchads360V0ResourcesAdGroupAsset::class;
  protected $adGroupAssetDataType = '';
  protected $adGroupAssetSetType = GoogleAdsSearchads360V0ResourcesAdGroupAssetSet::class;
  protected $adGroupAssetSetDataType = '';
  protected $adGroupAudienceViewType = GoogleAdsSearchads360V0ResourcesAdGroupAudienceView::class;
  protected $adGroupAudienceViewDataType = '';
  protected $adGroupBidModifierType = GoogleAdsSearchads360V0ResourcesAdGroupBidModifier::class;
  protected $adGroupBidModifierDataType = '';
  protected $adGroupCriterionType = GoogleAdsSearchads360V0ResourcesAdGroupCriterion::class;
  protected $adGroupCriterionDataType = '';
  protected $adGroupCriterionEffectiveLabelType = GoogleAdsSearchads360V0ResourcesAdGroupCriterionEffectiveLabel::class;
  protected $adGroupCriterionEffectiveLabelDataType = '';
  protected $adGroupCriterionLabelType = GoogleAdsSearchads360V0ResourcesAdGroupCriterionLabel::class;
  protected $adGroupCriterionLabelDataType = '';
  protected $adGroupEffectiveLabelType = GoogleAdsSearchads360V0ResourcesAdGroupEffectiveLabel::class;
  protected $adGroupEffectiveLabelDataType = '';
  protected $adGroupLabelType = GoogleAdsSearchads360V0ResourcesAdGroupLabel::class;
  protected $adGroupLabelDataType = '';
  protected $ageRangeViewType = GoogleAdsSearchads360V0ResourcesAgeRangeView::class;
  protected $ageRangeViewDataType = '';
  protected $assetType = GoogleAdsSearchads360V0ResourcesAsset::class;
  protected $assetDataType = '';
  protected $assetGroupType = GoogleAdsSearchads360V0ResourcesAssetGroup::class;
  protected $assetGroupDataType = '';
  protected $assetGroupAssetType = GoogleAdsSearchads360V0ResourcesAssetGroupAsset::class;
  protected $assetGroupAssetDataType = '';
  protected $assetGroupListingGroupFilterType = GoogleAdsSearchads360V0ResourcesAssetGroupListingGroupFilter::class;
  protected $assetGroupListingGroupFilterDataType = '';
  protected $assetGroupSignalType = GoogleAdsSearchads360V0ResourcesAssetGroupSignal::class;
  protected $assetGroupSignalDataType = '';
  protected $assetGroupTopCombinationViewType = GoogleAdsSearchads360V0ResourcesAssetGroupTopCombinationView::class;
  protected $assetGroupTopCombinationViewDataType = '';
  protected $assetSetType = GoogleAdsSearchads360V0ResourcesAssetSet::class;
  protected $assetSetDataType = '';
  protected $assetSetAssetType = GoogleAdsSearchads360V0ResourcesAssetSetAsset::class;
  protected $assetSetAssetDataType = '';
  protected $audienceType = GoogleAdsSearchads360V0ResourcesAudience::class;
  protected $audienceDataType = '';
  protected $biddingStrategyType = GoogleAdsSearchads360V0ResourcesBiddingStrategy::class;
  protected $biddingStrategyDataType = '';
  protected $campaignType = GoogleAdsSearchads360V0ResourcesCampaign::class;
  protected $campaignDataType = '';
  protected $campaignAssetType = GoogleAdsSearchads360V0ResourcesCampaignAsset::class;
  protected $campaignAssetDataType = '';
  protected $campaignAssetSetType = GoogleAdsSearchads360V0ResourcesCampaignAssetSet::class;
  protected $campaignAssetSetDataType = '';
  protected $campaignAudienceViewType = GoogleAdsSearchads360V0ResourcesCampaignAudienceView::class;
  protected $campaignAudienceViewDataType = '';
  protected $campaignBudgetType = GoogleAdsSearchads360V0ResourcesCampaignBudget::class;
  protected $campaignBudgetDataType = '';
  protected $campaignCriterionType = GoogleAdsSearchads360V0ResourcesCampaignCriterion::class;
  protected $campaignCriterionDataType = '';
  protected $campaignEffectiveLabelType = GoogleAdsSearchads360V0ResourcesCampaignEffectiveLabel::class;
  protected $campaignEffectiveLabelDataType = '';
  protected $campaignLabelType = GoogleAdsSearchads360V0ResourcesCampaignLabel::class;
  protected $campaignLabelDataType = '';
  protected $cartDataSalesViewType = GoogleAdsSearchads360V0ResourcesCartDataSalesView::class;
  protected $cartDataSalesViewDataType = '';
  protected $conversionType = GoogleAdsSearchads360V0ResourcesConversion::class;
  protected $conversionDataType = '';
  protected $conversionActionType = GoogleAdsSearchads360V0ResourcesConversionAction::class;
  protected $conversionActionDataType = '';
  protected $conversionCustomVariableType = GoogleAdsSearchads360V0ResourcesConversionCustomVariable::class;
  protected $conversionCustomVariableDataType = '';
  protected $customColumnsType = GoogleAdsSearchads360V0CommonValue::class;
  protected $customColumnsDataType = 'array';
  protected $customerType = GoogleAdsSearchads360V0ResourcesCustomer::class;
  protected $customerDataType = '';
  protected $customerAssetType = GoogleAdsSearchads360V0ResourcesCustomerAsset::class;
  protected $customerAssetDataType = '';
  protected $customerAssetSetType = GoogleAdsSearchads360V0ResourcesCustomerAssetSet::class;
  protected $customerAssetSetDataType = '';
  protected $customerClientType = GoogleAdsSearchads360V0ResourcesCustomerClient::class;
  protected $customerClientDataType = '';
  protected $customerManagerLinkType = GoogleAdsSearchads360V0ResourcesCustomerManagerLink::class;
  protected $customerManagerLinkDataType = '';
  protected $dynamicSearchAdsSearchTermViewType = GoogleAdsSearchads360V0ResourcesDynamicSearchAdsSearchTermView::class;
  protected $dynamicSearchAdsSearchTermViewDataType = '';
  protected $genderViewType = GoogleAdsSearchads360V0ResourcesGenderView::class;
  protected $genderViewDataType = '';
  protected $geoTargetConstantType = GoogleAdsSearchads360V0ResourcesGeoTargetConstant::class;
  protected $geoTargetConstantDataType = '';
  protected $keywordViewType = GoogleAdsSearchads360V0ResourcesKeywordView::class;
  protected $keywordViewDataType = '';
  protected $labelType = GoogleAdsSearchads360V0ResourcesLabel::class;
  protected $labelDataType = '';
  protected $languageConstantType = GoogleAdsSearchads360V0ResourcesLanguageConstant::class;
  protected $languageConstantDataType = '';
  protected $locationViewType = GoogleAdsSearchads360V0ResourcesLocationView::class;
  protected $locationViewDataType = '';
  protected $metricsType = GoogleAdsSearchads360V0CommonMetrics::class;
  protected $metricsDataType = '';
  protected $productBiddingCategoryConstantType = GoogleAdsSearchads360V0ResourcesProductBiddingCategoryConstant::class;
  protected $productBiddingCategoryConstantDataType = '';
  protected $productGroupViewType = GoogleAdsSearchads360V0ResourcesProductGroupView::class;
  protected $productGroupViewDataType = '';
  protected $segmentsType = GoogleAdsSearchads360V0CommonSegments::class;
  protected $segmentsDataType = '';
  protected $shoppingPerformanceViewType = GoogleAdsSearchads360V0ResourcesShoppingPerformanceView::class;
  protected $shoppingPerformanceViewDataType = '';
  protected $userListType = GoogleAdsSearchads360V0ResourcesUserList::class;
  protected $userListDataType = '';
  protected $userLocationViewType = GoogleAdsSearchads360V0ResourcesUserLocationView::class;
  protected $userLocationViewDataType = '';
  protected $visitType = GoogleAdsSearchads360V0ResourcesVisit::class;
  protected $visitDataType = '';
  protected $webpageViewType = GoogleAdsSearchads360V0ResourcesWebpageView::class;
  protected $webpageViewDataType = '';

  /**
   * @param GoogleAdsSearchads360V0ResourcesAccessibleBiddingStrategy
   */
  public function setAccessibleBiddingStrategy(GoogleAdsSearchads360V0ResourcesAccessibleBiddingStrategy $accessibleBiddingStrategy)
  {
    $this->accessibleBiddingStrategy = $accessibleBiddingStrategy;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAccessibleBiddingStrategy
   */
  public function getAccessibleBiddingStrategy()
  {
    return $this->accessibleBiddingStrategy;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAdGroup
   */
  public function setAdGroup(GoogleAdsSearchads360V0ResourcesAdGroup $adGroup)
  {
    $this->adGroup = $adGroup;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAdGroup
   */
  public function getAdGroup()
  {
    return $this->adGroup;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAdGroupAd
   */
  public function setAdGroupAd(GoogleAdsSearchads360V0ResourcesAdGroupAd $adGroupAd)
  {
    $this->adGroupAd = $adGroupAd;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAdGroupAd
   */
  public function getAdGroupAd()
  {
    return $this->adGroupAd;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAdGroupAdEffectiveLabel
   */
  public function setAdGroupAdEffectiveLabel(GoogleAdsSearchads360V0ResourcesAdGroupAdEffectiveLabel $adGroupAdEffectiveLabel)
  {
    $this->adGroupAdEffectiveLabel = $adGroupAdEffectiveLabel;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAdGroupAdEffectiveLabel
   */
  public function getAdGroupAdEffectiveLabel()
  {
    return $this->adGroupAdEffectiveLabel;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAdGroupAdLabel
   */
  public function setAdGroupAdLabel(GoogleAdsSearchads360V0ResourcesAdGroupAdLabel $adGroupAdLabel)
  {
    $this->adGroupAdLabel = $adGroupAdLabel;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAdGroupAdLabel
   */
  public function getAdGroupAdLabel()
  {
    return $this->adGroupAdLabel;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAdGroupAsset
   */
  public function setAdGroupAsset(GoogleAdsSearchads360V0ResourcesAdGroupAsset $adGroupAsset)
  {
    $this->adGroupAsset = $adGroupAsset;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAdGroupAsset
   */
  public function getAdGroupAsset()
  {
    return $this->adGroupAsset;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAdGroupAssetSet
   */
  public function setAdGroupAssetSet(GoogleAdsSearchads360V0ResourcesAdGroupAssetSet $adGroupAssetSet)
  {
    $this->adGroupAssetSet = $adGroupAssetSet;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAdGroupAssetSet
   */
  public function getAdGroupAssetSet()
  {
    return $this->adGroupAssetSet;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAdGroupAudienceView
   */
  public function setAdGroupAudienceView(GoogleAdsSearchads360V0ResourcesAdGroupAudienceView $adGroupAudienceView)
  {
    $this->adGroupAudienceView = $adGroupAudienceView;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAdGroupAudienceView
   */
  public function getAdGroupAudienceView()
  {
    return $this->adGroupAudienceView;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAdGroupBidModifier
   */
  public function setAdGroupBidModifier(GoogleAdsSearchads360V0ResourcesAdGroupBidModifier $adGroupBidModifier)
  {
    $this->adGroupBidModifier = $adGroupBidModifier;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAdGroupBidModifier
   */
  public function getAdGroupBidModifier()
  {
    return $this->adGroupBidModifier;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAdGroupCriterion
   */
  public function setAdGroupCriterion(GoogleAdsSearchads360V0ResourcesAdGroupCriterion $adGroupCriterion)
  {
    $this->adGroupCriterion = $adGroupCriterion;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAdGroupCriterion
   */
  public function getAdGroupCriterion()
  {
    return $this->adGroupCriterion;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAdGroupCriterionEffectiveLabel
   */
  public function setAdGroupCriterionEffectiveLabel(GoogleAdsSearchads360V0ResourcesAdGroupCriterionEffectiveLabel $adGroupCriterionEffectiveLabel)
  {
    $this->adGroupCriterionEffectiveLabel = $adGroupCriterionEffectiveLabel;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAdGroupCriterionEffectiveLabel
   */
  public function getAdGroupCriterionEffectiveLabel()
  {
    return $this->adGroupCriterionEffectiveLabel;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAdGroupCriterionLabel
   */
  public function setAdGroupCriterionLabel(GoogleAdsSearchads360V0ResourcesAdGroupCriterionLabel $adGroupCriterionLabel)
  {
    $this->adGroupCriterionLabel = $adGroupCriterionLabel;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAdGroupCriterionLabel
   */
  public function getAdGroupCriterionLabel()
  {
    return $this->adGroupCriterionLabel;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAdGroupEffectiveLabel
   */
  public function setAdGroupEffectiveLabel(GoogleAdsSearchads360V0ResourcesAdGroupEffectiveLabel $adGroupEffectiveLabel)
  {
    $this->adGroupEffectiveLabel = $adGroupEffectiveLabel;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAdGroupEffectiveLabel
   */
  public function getAdGroupEffectiveLabel()
  {
    return $this->adGroupEffectiveLabel;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAdGroupLabel
   */
  public function setAdGroupLabel(GoogleAdsSearchads360V0ResourcesAdGroupLabel $adGroupLabel)
  {
    $this->adGroupLabel = $adGroupLabel;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAdGroupLabel
   */
  public function getAdGroupLabel()
  {
    return $this->adGroupLabel;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAgeRangeView
   */
  public function setAgeRangeView(GoogleAdsSearchads360V0ResourcesAgeRangeView $ageRangeView)
  {
    $this->ageRangeView = $ageRangeView;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAgeRangeView
   */
  public function getAgeRangeView()
  {
    return $this->ageRangeView;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAsset
   */
  public function setAsset(GoogleAdsSearchads360V0ResourcesAsset $asset)
  {
    $this->asset = $asset;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAsset
   */
  public function getAsset()
  {
    return $this->asset;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAssetGroup
   */
  public function setAssetGroup(GoogleAdsSearchads360V0ResourcesAssetGroup $assetGroup)
  {
    $this->assetGroup = $assetGroup;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAssetGroup
   */
  public function getAssetGroup()
  {
    return $this->assetGroup;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAssetGroupAsset
   */
  public function setAssetGroupAsset(GoogleAdsSearchads360V0ResourcesAssetGroupAsset $assetGroupAsset)
  {
    $this->assetGroupAsset = $assetGroupAsset;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAssetGroupAsset
   */
  public function getAssetGroupAsset()
  {
    return $this->assetGroupAsset;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAssetGroupListingGroupFilter
   */
  public function setAssetGroupListingGroupFilter(GoogleAdsSearchads360V0ResourcesAssetGroupListingGroupFilter $assetGroupListingGroupFilter)
  {
    $this->assetGroupListingGroupFilter = $assetGroupListingGroupFilter;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAssetGroupListingGroupFilter
   */
  public function getAssetGroupListingGroupFilter()
  {
    return $this->assetGroupListingGroupFilter;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAssetGroupSignal
   */
  public function setAssetGroupSignal(GoogleAdsSearchads360V0ResourcesAssetGroupSignal $assetGroupSignal)
  {
    $this->assetGroupSignal = $assetGroupSignal;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAssetGroupSignal
   */
  public function getAssetGroupSignal()
  {
    return $this->assetGroupSignal;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAssetGroupTopCombinationView
   */
  public function setAssetGroupTopCombinationView(GoogleAdsSearchads360V0ResourcesAssetGroupTopCombinationView $assetGroupTopCombinationView)
  {
    $this->assetGroupTopCombinationView = $assetGroupTopCombinationView;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAssetGroupTopCombinationView
   */
  public function getAssetGroupTopCombinationView()
  {
    return $this->assetGroupTopCombinationView;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAssetSet
   */
  public function setAssetSet(GoogleAdsSearchads360V0ResourcesAssetSet $assetSet)
  {
    $this->assetSet = $assetSet;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAssetSet
   */
  public function getAssetSet()
  {
    return $this->assetSet;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAssetSetAsset
   */
  public function setAssetSetAsset(GoogleAdsSearchads360V0ResourcesAssetSetAsset $assetSetAsset)
  {
    $this->assetSetAsset = $assetSetAsset;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAssetSetAsset
   */
  public function getAssetSetAsset()
  {
    return $this->assetSetAsset;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesAudience
   */
  public function setAudience(GoogleAdsSearchads360V0ResourcesAudience $audience)
  {
    $this->audience = $audience;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesAudience
   */
  public function getAudience()
  {
    return $this->audience;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesBiddingStrategy
   */
  public function setBiddingStrategy(GoogleAdsSearchads360V0ResourcesBiddingStrategy $biddingStrategy)
  {
    $this->biddingStrategy = $biddingStrategy;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesBiddingStrategy
   */
  public function getBiddingStrategy()
  {
    return $this->biddingStrategy;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesCampaign
   */
  public function setCampaign(GoogleAdsSearchads360V0ResourcesCampaign $campaign)
  {
    $this->campaign = $campaign;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCampaign
   */
  public function getCampaign()
  {
    return $this->campaign;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesCampaignAsset
   */
  public function setCampaignAsset(GoogleAdsSearchads360V0ResourcesCampaignAsset $campaignAsset)
  {
    $this->campaignAsset = $campaignAsset;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCampaignAsset
   */
  public function getCampaignAsset()
  {
    return $this->campaignAsset;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesCampaignAssetSet
   */
  public function setCampaignAssetSet(GoogleAdsSearchads360V0ResourcesCampaignAssetSet $campaignAssetSet)
  {
    $this->campaignAssetSet = $campaignAssetSet;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCampaignAssetSet
   */
  public function getCampaignAssetSet()
  {
    return $this->campaignAssetSet;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesCampaignAudienceView
   */
  public function setCampaignAudienceView(GoogleAdsSearchads360V0ResourcesCampaignAudienceView $campaignAudienceView)
  {
    $this->campaignAudienceView = $campaignAudienceView;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCampaignAudienceView
   */
  public function getCampaignAudienceView()
  {
    return $this->campaignAudienceView;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesCampaignBudget
   */
  public function setCampaignBudget(GoogleAdsSearchads360V0ResourcesCampaignBudget $campaignBudget)
  {
    $this->campaignBudget = $campaignBudget;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCampaignBudget
   */
  public function getCampaignBudget()
  {
    return $this->campaignBudget;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesCampaignCriterion
   */
  public function setCampaignCriterion(GoogleAdsSearchads360V0ResourcesCampaignCriterion $campaignCriterion)
  {
    $this->campaignCriterion = $campaignCriterion;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCampaignCriterion
   */
  public function getCampaignCriterion()
  {
    return $this->campaignCriterion;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesCampaignEffectiveLabel
   */
  public function setCampaignEffectiveLabel(GoogleAdsSearchads360V0ResourcesCampaignEffectiveLabel $campaignEffectiveLabel)
  {
    $this->campaignEffectiveLabel = $campaignEffectiveLabel;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCampaignEffectiveLabel
   */
  public function getCampaignEffectiveLabel()
  {
    return $this->campaignEffectiveLabel;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesCampaignLabel
   */
  public function setCampaignLabel(GoogleAdsSearchads360V0ResourcesCampaignLabel $campaignLabel)
  {
    $this->campaignLabel = $campaignLabel;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCampaignLabel
   */
  public function getCampaignLabel()
  {
    return $this->campaignLabel;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesCartDataSalesView
   */
  public function setCartDataSalesView(GoogleAdsSearchads360V0ResourcesCartDataSalesView $cartDataSalesView)
  {
    $this->cartDataSalesView = $cartDataSalesView;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCartDataSalesView
   */
  public function getCartDataSalesView()
  {
    return $this->cartDataSalesView;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesConversion
   */
  public function setConversion(GoogleAdsSearchads360V0ResourcesConversion $conversion)
  {
    $this->conversion = $conversion;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesConversion
   */
  public function getConversion()
  {
    return $this->conversion;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesConversionAction
   */
  public function setConversionAction(GoogleAdsSearchads360V0ResourcesConversionAction $conversionAction)
  {
    $this->conversionAction = $conversionAction;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesConversionAction
   */
  public function getConversionAction()
  {
    return $this->conversionAction;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesConversionCustomVariable
   */
  public function setConversionCustomVariable(GoogleAdsSearchads360V0ResourcesConversionCustomVariable $conversionCustomVariable)
  {
    $this->conversionCustomVariable = $conversionCustomVariable;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesConversionCustomVariable
   */
  public function getConversionCustomVariable()
  {
    return $this->conversionCustomVariable;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonValue[]
   */
  public function setCustomColumns($customColumns)
  {
    $this->customColumns = $customColumns;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonValue[]
   */
  public function getCustomColumns()
  {
    return $this->customColumns;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesCustomer
   */
  public function setCustomer(GoogleAdsSearchads360V0ResourcesCustomer $customer)
  {
    $this->customer = $customer;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCustomer
   */
  public function getCustomer()
  {
    return $this->customer;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesCustomerAsset
   */
  public function setCustomerAsset(GoogleAdsSearchads360V0ResourcesCustomerAsset $customerAsset)
  {
    $this->customerAsset = $customerAsset;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCustomerAsset
   */
  public function getCustomerAsset()
  {
    return $this->customerAsset;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesCustomerAssetSet
   */
  public function setCustomerAssetSet(GoogleAdsSearchads360V0ResourcesCustomerAssetSet $customerAssetSet)
  {
    $this->customerAssetSet = $customerAssetSet;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCustomerAssetSet
   */
  public function getCustomerAssetSet()
  {
    return $this->customerAssetSet;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesCustomerClient
   */
  public function setCustomerClient(GoogleAdsSearchads360V0ResourcesCustomerClient $customerClient)
  {
    $this->customerClient = $customerClient;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCustomerClient
   */
  public function getCustomerClient()
  {
    return $this->customerClient;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesCustomerManagerLink
   */
  public function setCustomerManagerLink(GoogleAdsSearchads360V0ResourcesCustomerManagerLink $customerManagerLink)
  {
    $this->customerManagerLink = $customerManagerLink;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesCustomerManagerLink
   */
  public function getCustomerManagerLink()
  {
    return $this->customerManagerLink;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesDynamicSearchAdsSearchTermView
   */
  public function setDynamicSearchAdsSearchTermView(GoogleAdsSearchads360V0ResourcesDynamicSearchAdsSearchTermView $dynamicSearchAdsSearchTermView)
  {
    $this->dynamicSearchAdsSearchTermView = $dynamicSearchAdsSearchTermView;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesDynamicSearchAdsSearchTermView
   */
  public function getDynamicSearchAdsSearchTermView()
  {
    return $this->dynamicSearchAdsSearchTermView;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesGenderView
   */
  public function setGenderView(GoogleAdsSearchads360V0ResourcesGenderView $genderView)
  {
    $this->genderView = $genderView;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesGenderView
   */
  public function getGenderView()
  {
    return $this->genderView;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesGeoTargetConstant
   */
  public function setGeoTargetConstant(GoogleAdsSearchads360V0ResourcesGeoTargetConstant $geoTargetConstant)
  {
    $this->geoTargetConstant = $geoTargetConstant;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesGeoTargetConstant
   */
  public function getGeoTargetConstant()
  {
    return $this->geoTargetConstant;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesKeywordView
   */
  public function setKeywordView(GoogleAdsSearchads360V0ResourcesKeywordView $keywordView)
  {
    $this->keywordView = $keywordView;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesKeywordView
   */
  public function getKeywordView()
  {
    return $this->keywordView;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesLabel
   */
  public function setLabel(GoogleAdsSearchads360V0ResourcesLabel $label)
  {
    $this->label = $label;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesLabel
   */
  public function getLabel()
  {
    return $this->label;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesLanguageConstant
   */
  public function setLanguageConstant(GoogleAdsSearchads360V0ResourcesLanguageConstant $languageConstant)
  {
    $this->languageConstant = $languageConstant;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesLanguageConstant
   */
  public function getLanguageConstant()
  {
    return $this->languageConstant;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesLocationView
   */
  public function setLocationView(GoogleAdsSearchads360V0ResourcesLocationView $locationView)
  {
    $this->locationView = $locationView;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesLocationView
   */
  public function getLocationView()
  {
    return $this->locationView;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonMetrics
   */
  public function setMetrics(GoogleAdsSearchads360V0CommonMetrics $metrics)
  {
    $this->metrics = $metrics;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonMetrics
   */
  public function getMetrics()
  {
    return $this->metrics;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesProductBiddingCategoryConstant
   */
  public function setProductBiddingCategoryConstant(GoogleAdsSearchads360V0ResourcesProductBiddingCategoryConstant $productBiddingCategoryConstant)
  {
    $this->productBiddingCategoryConstant = $productBiddingCategoryConstant;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesProductBiddingCategoryConstant
   */
  public function getProductBiddingCategoryConstant()
  {
    return $this->productBiddingCategoryConstant;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesProductGroupView
   */
  public function setProductGroupView(GoogleAdsSearchads360V0ResourcesProductGroupView $productGroupView)
  {
    $this->productGroupView = $productGroupView;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesProductGroupView
   */
  public function getProductGroupView()
  {
    return $this->productGroupView;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonSegments
   */
  public function setSegments(GoogleAdsSearchads360V0CommonSegments $segments)
  {
    $this->segments = $segments;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonSegments
   */
  public function getSegments()
  {
    return $this->segments;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesShoppingPerformanceView
   */
  public function setShoppingPerformanceView(GoogleAdsSearchads360V0ResourcesShoppingPerformanceView $shoppingPerformanceView)
  {
    $this->shoppingPerformanceView = $shoppingPerformanceView;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesShoppingPerformanceView
   */
  public function getShoppingPerformanceView()
  {
    return $this->shoppingPerformanceView;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesUserList
   */
  public function setUserList(GoogleAdsSearchads360V0ResourcesUserList $userList)
  {
    $this->userList = $userList;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesUserList
   */
  public function getUserList()
  {
    return $this->userList;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesUserLocationView
   */
  public function setUserLocationView(GoogleAdsSearchads360V0ResourcesUserLocationView $userLocationView)
  {
    $this->userLocationView = $userLocationView;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesUserLocationView
   */
  public function getUserLocationView()
  {
    return $this->userLocationView;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesVisit
   */
  public function setVisit(GoogleAdsSearchads360V0ResourcesVisit $visit)
  {
    $this->visit = $visit;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesVisit
   */
  public function getVisit()
  {
    return $this->visit;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesWebpageView
   */
  public function setWebpageView(GoogleAdsSearchads360V0ResourcesWebpageView $webpageView)
  {
    $this->webpageView = $webpageView;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesWebpageView
   */
  public function getWebpageView()
  {
    return $this->webpageView;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ServicesSearchAds360Row::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ServicesSearchAds360Row');
