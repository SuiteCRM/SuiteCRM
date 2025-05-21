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

class GoogleAdsSearchads360V0CommonMetrics extends \Google\Collection
{
  protected $collection_key = 'rawEventConversionMetrics';
  public $absoluteTopImpressionPercentage;
  public $allConversions;
  public $allConversionsByConversionDate;
  public $allConversionsFromClickToCall;
  public $allConversionsFromDirections;
  public $allConversionsFromInteractionsRate;
  public $allConversionsFromInteractionsValuePerInteraction;
  public $allConversionsFromMenu;
  public $allConversionsFromOrder;
  public $allConversionsFromOtherEngagement;
  public $allConversionsFromStoreVisit;
  public $allConversionsFromStoreWebsite;
  public $allConversionsValue;
  public $allConversionsValueByConversionDate;
  public $allConversionsValuePerCost;
  public $averageCost;
  public $averageCpc;
  public $averageCpm;
  public $averageQualityScore;
  /**
   * @var string
   */
  public $clicks;
  public $clientAccountConversions;
  public $clientAccountConversionsValue;
  /**
   * @var string
   */
  public $clientAccountCrossSellCostOfGoodsSoldMicros;
  /**
   * @var string
   */
  public $clientAccountCrossSellGrossProfitMicros;
  /**
   * @var string
   */
  public $clientAccountCrossSellRevenueMicros;
  public $clientAccountCrossSellUnitsSold;
  /**
   * @var string
   */
  public $clientAccountLeadCostOfGoodsSoldMicros;
  /**
   * @var string
   */
  public $clientAccountLeadGrossProfitMicros;
  /**
   * @var string
   */
  public $clientAccountLeadRevenueMicros;
  public $clientAccountLeadUnitsSold;
  /**
   * @var string
   */
  public $clientAccountViewThroughConversions;
  public $contentBudgetLostImpressionShare;
  public $contentImpressionShare;
  public $contentRankLostImpressionShare;
  protected $conversionCustomMetricsType = GoogleAdsSearchads360V0CommonValue::class;
  protected $conversionCustomMetricsDataType = 'array';
  public $conversions;
  public $conversionsByConversionDate;
  public $conversionsFromInteractionsRate;
  public $conversionsFromInteractionsValuePerInteraction;
  public $conversionsValue;
  public $conversionsValueByConversionDate;
  public $conversionsValuePerCost;
  /**
   * @var string
   */
  public $costMicros;
  public $costPerAllConversions;
  public $costPerConversion;
  public $costPerCurrentModelAttributedConversion;
  public $crossDeviceConversions;
  public $crossDeviceConversionsByConversionDate;
  public $crossDeviceConversionsValue;
  public $crossDeviceConversionsValueByConversionDate;
  /**
   * @var string
   */
  public $crossSellCostOfGoodsSoldMicros;
  /**
   * @var string
   */
  public $crossSellGrossProfitMicros;
  /**
   * @var string
   */
  public $crossSellRevenueMicros;
  public $crossSellUnitsSold;
  public $ctr;
  public $generalInvalidClickRate;
  /**
   * @var string
   */
  public $generalInvalidClicks;
  /**
   * @var string
   */
  public $historicalCreativeQualityScore;
  /**
   * @var string
   */
  public $historicalLandingPageQualityScore;
  /**
   * @var string
   */
  public $historicalQualityScore;
  /**
   * @var string
   */
  public $historicalSearchPredictedCtr;
  /**
   * @var string
   */
  public $impressions;
  /**
   * @var string[]
   */
  public $interactionEventTypes;
  public $interactionRate;
  /**
   * @var string
   */
  public $interactions;
  public $invalidClickRate;
  /**
   * @var string
   */
  public $invalidClicks;
  /**
   * @var string
   */
  public $leadCostOfGoodsSoldMicros;
  /**
   * @var string
   */
  public $leadGrossProfitMicros;
  /**
   * @var string
   */
  public $leadRevenueMicros;
  public $leadUnitsSold;
  public $mobileFriendlyClicksPercentage;
  protected $rawEventConversionMetricsType = GoogleAdsSearchads360V0CommonValue::class;
  protected $rawEventConversionMetricsDataType = 'array';
  public $searchAbsoluteTopImpressionShare;
  public $searchBudgetLostAbsoluteTopImpressionShare;
  public $searchBudgetLostImpressionShare;
  public $searchBudgetLostTopImpressionShare;
  public $searchClickShare;
  public $searchExactMatchImpressionShare;
  public $searchImpressionShare;
  public $searchRankLostAbsoluteTopImpressionShare;
  public $searchRankLostImpressionShare;
  public $searchRankLostTopImpressionShare;
  public $searchTopImpressionShare;
  public $topImpressionPercentage;
  public $valuePerAllConversions;
  public $valuePerAllConversionsByConversionDate;
  public $valuePerConversion;
  public $valuePerConversionsByConversionDate;
  public $visits;

  public function setAbsoluteTopImpressionPercentage($absoluteTopImpressionPercentage)
  {
    $this->absoluteTopImpressionPercentage = $absoluteTopImpressionPercentage;
  }
  public function getAbsoluteTopImpressionPercentage()
  {
    return $this->absoluteTopImpressionPercentage;
  }
  public function setAllConversions($allConversions)
  {
    $this->allConversions = $allConversions;
  }
  public function getAllConversions()
  {
    return $this->allConversions;
  }
  public function setAllConversionsByConversionDate($allConversionsByConversionDate)
  {
    $this->allConversionsByConversionDate = $allConversionsByConversionDate;
  }
  public function getAllConversionsByConversionDate()
  {
    return $this->allConversionsByConversionDate;
  }
  public function setAllConversionsFromClickToCall($allConversionsFromClickToCall)
  {
    $this->allConversionsFromClickToCall = $allConversionsFromClickToCall;
  }
  public function getAllConversionsFromClickToCall()
  {
    return $this->allConversionsFromClickToCall;
  }
  public function setAllConversionsFromDirections($allConversionsFromDirections)
  {
    $this->allConversionsFromDirections = $allConversionsFromDirections;
  }
  public function getAllConversionsFromDirections()
  {
    return $this->allConversionsFromDirections;
  }
  public function setAllConversionsFromInteractionsRate($allConversionsFromInteractionsRate)
  {
    $this->allConversionsFromInteractionsRate = $allConversionsFromInteractionsRate;
  }
  public function getAllConversionsFromInteractionsRate()
  {
    return $this->allConversionsFromInteractionsRate;
  }
  public function setAllConversionsFromInteractionsValuePerInteraction($allConversionsFromInteractionsValuePerInteraction)
  {
    $this->allConversionsFromInteractionsValuePerInteraction = $allConversionsFromInteractionsValuePerInteraction;
  }
  public function getAllConversionsFromInteractionsValuePerInteraction()
  {
    return $this->allConversionsFromInteractionsValuePerInteraction;
  }
  public function setAllConversionsFromMenu($allConversionsFromMenu)
  {
    $this->allConversionsFromMenu = $allConversionsFromMenu;
  }
  public function getAllConversionsFromMenu()
  {
    return $this->allConversionsFromMenu;
  }
  public function setAllConversionsFromOrder($allConversionsFromOrder)
  {
    $this->allConversionsFromOrder = $allConversionsFromOrder;
  }
  public function getAllConversionsFromOrder()
  {
    return $this->allConversionsFromOrder;
  }
  public function setAllConversionsFromOtherEngagement($allConversionsFromOtherEngagement)
  {
    $this->allConversionsFromOtherEngagement = $allConversionsFromOtherEngagement;
  }
  public function getAllConversionsFromOtherEngagement()
  {
    return $this->allConversionsFromOtherEngagement;
  }
  public function setAllConversionsFromStoreVisit($allConversionsFromStoreVisit)
  {
    $this->allConversionsFromStoreVisit = $allConversionsFromStoreVisit;
  }
  public function getAllConversionsFromStoreVisit()
  {
    return $this->allConversionsFromStoreVisit;
  }
  public function setAllConversionsFromStoreWebsite($allConversionsFromStoreWebsite)
  {
    $this->allConversionsFromStoreWebsite = $allConversionsFromStoreWebsite;
  }
  public function getAllConversionsFromStoreWebsite()
  {
    return $this->allConversionsFromStoreWebsite;
  }
  public function setAllConversionsValue($allConversionsValue)
  {
    $this->allConversionsValue = $allConversionsValue;
  }
  public function getAllConversionsValue()
  {
    return $this->allConversionsValue;
  }
  public function setAllConversionsValueByConversionDate($allConversionsValueByConversionDate)
  {
    $this->allConversionsValueByConversionDate = $allConversionsValueByConversionDate;
  }
  public function getAllConversionsValueByConversionDate()
  {
    return $this->allConversionsValueByConversionDate;
  }
  public function setAllConversionsValuePerCost($allConversionsValuePerCost)
  {
    $this->allConversionsValuePerCost = $allConversionsValuePerCost;
  }
  public function getAllConversionsValuePerCost()
  {
    return $this->allConversionsValuePerCost;
  }
  public function setAverageCost($averageCost)
  {
    $this->averageCost = $averageCost;
  }
  public function getAverageCost()
  {
    return $this->averageCost;
  }
  public function setAverageCpc($averageCpc)
  {
    $this->averageCpc = $averageCpc;
  }
  public function getAverageCpc()
  {
    return $this->averageCpc;
  }
  public function setAverageCpm($averageCpm)
  {
    $this->averageCpm = $averageCpm;
  }
  public function getAverageCpm()
  {
    return $this->averageCpm;
  }
  public function setAverageQualityScore($averageQualityScore)
  {
    $this->averageQualityScore = $averageQualityScore;
  }
  public function getAverageQualityScore()
  {
    return $this->averageQualityScore;
  }
  /**
   * @param string
   */
  public function setClicks($clicks)
  {
    $this->clicks = $clicks;
  }
  /**
   * @return string
   */
  public function getClicks()
  {
    return $this->clicks;
  }
  public function setClientAccountConversions($clientAccountConversions)
  {
    $this->clientAccountConversions = $clientAccountConversions;
  }
  public function getClientAccountConversions()
  {
    return $this->clientAccountConversions;
  }
  public function setClientAccountConversionsValue($clientAccountConversionsValue)
  {
    $this->clientAccountConversionsValue = $clientAccountConversionsValue;
  }
  public function getClientAccountConversionsValue()
  {
    return $this->clientAccountConversionsValue;
  }
  /**
   * @param string
   */
  public function setClientAccountCrossSellCostOfGoodsSoldMicros($clientAccountCrossSellCostOfGoodsSoldMicros)
  {
    $this->clientAccountCrossSellCostOfGoodsSoldMicros = $clientAccountCrossSellCostOfGoodsSoldMicros;
  }
  /**
   * @return string
   */
  public function getClientAccountCrossSellCostOfGoodsSoldMicros()
  {
    return $this->clientAccountCrossSellCostOfGoodsSoldMicros;
  }
  /**
   * @param string
   */
  public function setClientAccountCrossSellGrossProfitMicros($clientAccountCrossSellGrossProfitMicros)
  {
    $this->clientAccountCrossSellGrossProfitMicros = $clientAccountCrossSellGrossProfitMicros;
  }
  /**
   * @return string
   */
  public function getClientAccountCrossSellGrossProfitMicros()
  {
    return $this->clientAccountCrossSellGrossProfitMicros;
  }
  /**
   * @param string
   */
  public function setClientAccountCrossSellRevenueMicros($clientAccountCrossSellRevenueMicros)
  {
    $this->clientAccountCrossSellRevenueMicros = $clientAccountCrossSellRevenueMicros;
  }
  /**
   * @return string
   */
  public function getClientAccountCrossSellRevenueMicros()
  {
    return $this->clientAccountCrossSellRevenueMicros;
  }
  public function setClientAccountCrossSellUnitsSold($clientAccountCrossSellUnitsSold)
  {
    $this->clientAccountCrossSellUnitsSold = $clientAccountCrossSellUnitsSold;
  }
  public function getClientAccountCrossSellUnitsSold()
  {
    return $this->clientAccountCrossSellUnitsSold;
  }
  /**
   * @param string
   */
  public function setClientAccountLeadCostOfGoodsSoldMicros($clientAccountLeadCostOfGoodsSoldMicros)
  {
    $this->clientAccountLeadCostOfGoodsSoldMicros = $clientAccountLeadCostOfGoodsSoldMicros;
  }
  /**
   * @return string
   */
  public function getClientAccountLeadCostOfGoodsSoldMicros()
  {
    return $this->clientAccountLeadCostOfGoodsSoldMicros;
  }
  /**
   * @param string
   */
  public function setClientAccountLeadGrossProfitMicros($clientAccountLeadGrossProfitMicros)
  {
    $this->clientAccountLeadGrossProfitMicros = $clientAccountLeadGrossProfitMicros;
  }
  /**
   * @return string
   */
  public function getClientAccountLeadGrossProfitMicros()
  {
    return $this->clientAccountLeadGrossProfitMicros;
  }
  /**
   * @param string
   */
  public function setClientAccountLeadRevenueMicros($clientAccountLeadRevenueMicros)
  {
    $this->clientAccountLeadRevenueMicros = $clientAccountLeadRevenueMicros;
  }
  /**
   * @return string
   */
  public function getClientAccountLeadRevenueMicros()
  {
    return $this->clientAccountLeadRevenueMicros;
  }
  public function setClientAccountLeadUnitsSold($clientAccountLeadUnitsSold)
  {
    $this->clientAccountLeadUnitsSold = $clientAccountLeadUnitsSold;
  }
  public function getClientAccountLeadUnitsSold()
  {
    return $this->clientAccountLeadUnitsSold;
  }
  /**
   * @param string
   */
  public function setClientAccountViewThroughConversions($clientAccountViewThroughConversions)
  {
    $this->clientAccountViewThroughConversions = $clientAccountViewThroughConversions;
  }
  /**
   * @return string
   */
  public function getClientAccountViewThroughConversions()
  {
    return $this->clientAccountViewThroughConversions;
  }
  public function setContentBudgetLostImpressionShare($contentBudgetLostImpressionShare)
  {
    $this->contentBudgetLostImpressionShare = $contentBudgetLostImpressionShare;
  }
  public function getContentBudgetLostImpressionShare()
  {
    return $this->contentBudgetLostImpressionShare;
  }
  public function setContentImpressionShare($contentImpressionShare)
  {
    $this->contentImpressionShare = $contentImpressionShare;
  }
  public function getContentImpressionShare()
  {
    return $this->contentImpressionShare;
  }
  public function setContentRankLostImpressionShare($contentRankLostImpressionShare)
  {
    $this->contentRankLostImpressionShare = $contentRankLostImpressionShare;
  }
  public function getContentRankLostImpressionShare()
  {
    return $this->contentRankLostImpressionShare;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonValue[]
   */
  public function setConversionCustomMetrics($conversionCustomMetrics)
  {
    $this->conversionCustomMetrics = $conversionCustomMetrics;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonValue[]
   */
  public function getConversionCustomMetrics()
  {
    return $this->conversionCustomMetrics;
  }
  public function setConversions($conversions)
  {
    $this->conversions = $conversions;
  }
  public function getConversions()
  {
    return $this->conversions;
  }
  public function setConversionsByConversionDate($conversionsByConversionDate)
  {
    $this->conversionsByConversionDate = $conversionsByConversionDate;
  }
  public function getConversionsByConversionDate()
  {
    return $this->conversionsByConversionDate;
  }
  public function setConversionsFromInteractionsRate($conversionsFromInteractionsRate)
  {
    $this->conversionsFromInteractionsRate = $conversionsFromInteractionsRate;
  }
  public function getConversionsFromInteractionsRate()
  {
    return $this->conversionsFromInteractionsRate;
  }
  public function setConversionsFromInteractionsValuePerInteraction($conversionsFromInteractionsValuePerInteraction)
  {
    $this->conversionsFromInteractionsValuePerInteraction = $conversionsFromInteractionsValuePerInteraction;
  }
  public function getConversionsFromInteractionsValuePerInteraction()
  {
    return $this->conversionsFromInteractionsValuePerInteraction;
  }
  public function setConversionsValue($conversionsValue)
  {
    $this->conversionsValue = $conversionsValue;
  }
  public function getConversionsValue()
  {
    return $this->conversionsValue;
  }
  public function setConversionsValueByConversionDate($conversionsValueByConversionDate)
  {
    $this->conversionsValueByConversionDate = $conversionsValueByConversionDate;
  }
  public function getConversionsValueByConversionDate()
  {
    return $this->conversionsValueByConversionDate;
  }
  public function setConversionsValuePerCost($conversionsValuePerCost)
  {
    $this->conversionsValuePerCost = $conversionsValuePerCost;
  }
  public function getConversionsValuePerCost()
  {
    return $this->conversionsValuePerCost;
  }
  /**
   * @param string
   */
  public function setCostMicros($costMicros)
  {
    $this->costMicros = $costMicros;
  }
  /**
   * @return string
   */
  public function getCostMicros()
  {
    return $this->costMicros;
  }
  public function setCostPerAllConversions($costPerAllConversions)
  {
    $this->costPerAllConversions = $costPerAllConversions;
  }
  public function getCostPerAllConversions()
  {
    return $this->costPerAllConversions;
  }
  public function setCostPerConversion($costPerConversion)
  {
    $this->costPerConversion = $costPerConversion;
  }
  public function getCostPerConversion()
  {
    return $this->costPerConversion;
  }
  public function setCostPerCurrentModelAttributedConversion($costPerCurrentModelAttributedConversion)
  {
    $this->costPerCurrentModelAttributedConversion = $costPerCurrentModelAttributedConversion;
  }
  public function getCostPerCurrentModelAttributedConversion()
  {
    return $this->costPerCurrentModelAttributedConversion;
  }
  public function setCrossDeviceConversions($crossDeviceConversions)
  {
    $this->crossDeviceConversions = $crossDeviceConversions;
  }
  public function getCrossDeviceConversions()
  {
    return $this->crossDeviceConversions;
  }
  public function setCrossDeviceConversionsByConversionDate($crossDeviceConversionsByConversionDate)
  {
    $this->crossDeviceConversionsByConversionDate = $crossDeviceConversionsByConversionDate;
  }
  public function getCrossDeviceConversionsByConversionDate()
  {
    return $this->crossDeviceConversionsByConversionDate;
  }
  public function setCrossDeviceConversionsValue($crossDeviceConversionsValue)
  {
    $this->crossDeviceConversionsValue = $crossDeviceConversionsValue;
  }
  public function getCrossDeviceConversionsValue()
  {
    return $this->crossDeviceConversionsValue;
  }
  public function setCrossDeviceConversionsValueByConversionDate($crossDeviceConversionsValueByConversionDate)
  {
    $this->crossDeviceConversionsValueByConversionDate = $crossDeviceConversionsValueByConversionDate;
  }
  public function getCrossDeviceConversionsValueByConversionDate()
  {
    return $this->crossDeviceConversionsValueByConversionDate;
  }
  /**
   * @param string
   */
  public function setCrossSellCostOfGoodsSoldMicros($crossSellCostOfGoodsSoldMicros)
  {
    $this->crossSellCostOfGoodsSoldMicros = $crossSellCostOfGoodsSoldMicros;
  }
  /**
   * @return string
   */
  public function getCrossSellCostOfGoodsSoldMicros()
  {
    return $this->crossSellCostOfGoodsSoldMicros;
  }
  /**
   * @param string
   */
  public function setCrossSellGrossProfitMicros($crossSellGrossProfitMicros)
  {
    $this->crossSellGrossProfitMicros = $crossSellGrossProfitMicros;
  }
  /**
   * @return string
   */
  public function getCrossSellGrossProfitMicros()
  {
    return $this->crossSellGrossProfitMicros;
  }
  /**
   * @param string
   */
  public function setCrossSellRevenueMicros($crossSellRevenueMicros)
  {
    $this->crossSellRevenueMicros = $crossSellRevenueMicros;
  }
  /**
   * @return string
   */
  public function getCrossSellRevenueMicros()
  {
    return $this->crossSellRevenueMicros;
  }
  public function setCrossSellUnitsSold($crossSellUnitsSold)
  {
    $this->crossSellUnitsSold = $crossSellUnitsSold;
  }
  public function getCrossSellUnitsSold()
  {
    return $this->crossSellUnitsSold;
  }
  public function setCtr($ctr)
  {
    $this->ctr = $ctr;
  }
  public function getCtr()
  {
    return $this->ctr;
  }
  public function setGeneralInvalidClickRate($generalInvalidClickRate)
  {
    $this->generalInvalidClickRate = $generalInvalidClickRate;
  }
  public function getGeneralInvalidClickRate()
  {
    return $this->generalInvalidClickRate;
  }
  /**
   * @param string
   */
  public function setGeneralInvalidClicks($generalInvalidClicks)
  {
    $this->generalInvalidClicks = $generalInvalidClicks;
  }
  /**
   * @return string
   */
  public function getGeneralInvalidClicks()
  {
    return $this->generalInvalidClicks;
  }
  /**
   * @param string
   */
  public function setHistoricalCreativeQualityScore($historicalCreativeQualityScore)
  {
    $this->historicalCreativeQualityScore = $historicalCreativeQualityScore;
  }
  /**
   * @return string
   */
  public function getHistoricalCreativeQualityScore()
  {
    return $this->historicalCreativeQualityScore;
  }
  /**
   * @param string
   */
  public function setHistoricalLandingPageQualityScore($historicalLandingPageQualityScore)
  {
    $this->historicalLandingPageQualityScore = $historicalLandingPageQualityScore;
  }
  /**
   * @return string
   */
  public function getHistoricalLandingPageQualityScore()
  {
    return $this->historicalLandingPageQualityScore;
  }
  /**
   * @param string
   */
  public function setHistoricalQualityScore($historicalQualityScore)
  {
    $this->historicalQualityScore = $historicalQualityScore;
  }
  /**
   * @return string
   */
  public function getHistoricalQualityScore()
  {
    return $this->historicalQualityScore;
  }
  /**
   * @param string
   */
  public function setHistoricalSearchPredictedCtr($historicalSearchPredictedCtr)
  {
    $this->historicalSearchPredictedCtr = $historicalSearchPredictedCtr;
  }
  /**
   * @return string
   */
  public function getHistoricalSearchPredictedCtr()
  {
    return $this->historicalSearchPredictedCtr;
  }
  /**
   * @param string
   */
  public function setImpressions($impressions)
  {
    $this->impressions = $impressions;
  }
  /**
   * @return string
   */
  public function getImpressions()
  {
    return $this->impressions;
  }
  /**
   * @param string[]
   */
  public function setInteractionEventTypes($interactionEventTypes)
  {
    $this->interactionEventTypes = $interactionEventTypes;
  }
  /**
   * @return string[]
   */
  public function getInteractionEventTypes()
  {
    return $this->interactionEventTypes;
  }
  public function setInteractionRate($interactionRate)
  {
    $this->interactionRate = $interactionRate;
  }
  public function getInteractionRate()
  {
    return $this->interactionRate;
  }
  /**
   * @param string
   */
  public function setInteractions($interactions)
  {
    $this->interactions = $interactions;
  }
  /**
   * @return string
   */
  public function getInteractions()
  {
    return $this->interactions;
  }
  public function setInvalidClickRate($invalidClickRate)
  {
    $this->invalidClickRate = $invalidClickRate;
  }
  public function getInvalidClickRate()
  {
    return $this->invalidClickRate;
  }
  /**
   * @param string
   */
  public function setInvalidClicks($invalidClicks)
  {
    $this->invalidClicks = $invalidClicks;
  }
  /**
   * @return string
   */
  public function getInvalidClicks()
  {
    return $this->invalidClicks;
  }
  /**
   * @param string
   */
  public function setLeadCostOfGoodsSoldMicros($leadCostOfGoodsSoldMicros)
  {
    $this->leadCostOfGoodsSoldMicros = $leadCostOfGoodsSoldMicros;
  }
  /**
   * @return string
   */
  public function getLeadCostOfGoodsSoldMicros()
  {
    return $this->leadCostOfGoodsSoldMicros;
  }
  /**
   * @param string
   */
  public function setLeadGrossProfitMicros($leadGrossProfitMicros)
  {
    $this->leadGrossProfitMicros = $leadGrossProfitMicros;
  }
  /**
   * @return string
   */
  public function getLeadGrossProfitMicros()
  {
    return $this->leadGrossProfitMicros;
  }
  /**
   * @param string
   */
  public function setLeadRevenueMicros($leadRevenueMicros)
  {
    $this->leadRevenueMicros = $leadRevenueMicros;
  }
  /**
   * @return string
   */
  public function getLeadRevenueMicros()
  {
    return $this->leadRevenueMicros;
  }
  public function setLeadUnitsSold($leadUnitsSold)
  {
    $this->leadUnitsSold = $leadUnitsSold;
  }
  public function getLeadUnitsSold()
  {
    return $this->leadUnitsSold;
  }
  public function setMobileFriendlyClicksPercentage($mobileFriendlyClicksPercentage)
  {
    $this->mobileFriendlyClicksPercentage = $mobileFriendlyClicksPercentage;
  }
  public function getMobileFriendlyClicksPercentage()
  {
    return $this->mobileFriendlyClicksPercentage;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonValue[]
   */
  public function setRawEventConversionMetrics($rawEventConversionMetrics)
  {
    $this->rawEventConversionMetrics = $rawEventConversionMetrics;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonValue[]
   */
  public function getRawEventConversionMetrics()
  {
    return $this->rawEventConversionMetrics;
  }
  public function setSearchAbsoluteTopImpressionShare($searchAbsoluteTopImpressionShare)
  {
    $this->searchAbsoluteTopImpressionShare = $searchAbsoluteTopImpressionShare;
  }
  public function getSearchAbsoluteTopImpressionShare()
  {
    return $this->searchAbsoluteTopImpressionShare;
  }
  public function setSearchBudgetLostAbsoluteTopImpressionShare($searchBudgetLostAbsoluteTopImpressionShare)
  {
    $this->searchBudgetLostAbsoluteTopImpressionShare = $searchBudgetLostAbsoluteTopImpressionShare;
  }
  public function getSearchBudgetLostAbsoluteTopImpressionShare()
  {
    return $this->searchBudgetLostAbsoluteTopImpressionShare;
  }
  public function setSearchBudgetLostImpressionShare($searchBudgetLostImpressionShare)
  {
    $this->searchBudgetLostImpressionShare = $searchBudgetLostImpressionShare;
  }
  public function getSearchBudgetLostImpressionShare()
  {
    return $this->searchBudgetLostImpressionShare;
  }
  public function setSearchBudgetLostTopImpressionShare($searchBudgetLostTopImpressionShare)
  {
    $this->searchBudgetLostTopImpressionShare = $searchBudgetLostTopImpressionShare;
  }
  public function getSearchBudgetLostTopImpressionShare()
  {
    return $this->searchBudgetLostTopImpressionShare;
  }
  public function setSearchClickShare($searchClickShare)
  {
    $this->searchClickShare = $searchClickShare;
  }
  public function getSearchClickShare()
  {
    return $this->searchClickShare;
  }
  public function setSearchExactMatchImpressionShare($searchExactMatchImpressionShare)
  {
    $this->searchExactMatchImpressionShare = $searchExactMatchImpressionShare;
  }
  public function getSearchExactMatchImpressionShare()
  {
    return $this->searchExactMatchImpressionShare;
  }
  public function setSearchImpressionShare($searchImpressionShare)
  {
    $this->searchImpressionShare = $searchImpressionShare;
  }
  public function getSearchImpressionShare()
  {
    return $this->searchImpressionShare;
  }
  public function setSearchRankLostAbsoluteTopImpressionShare($searchRankLostAbsoluteTopImpressionShare)
  {
    $this->searchRankLostAbsoluteTopImpressionShare = $searchRankLostAbsoluteTopImpressionShare;
  }
  public function getSearchRankLostAbsoluteTopImpressionShare()
  {
    return $this->searchRankLostAbsoluteTopImpressionShare;
  }
  public function setSearchRankLostImpressionShare($searchRankLostImpressionShare)
  {
    $this->searchRankLostImpressionShare = $searchRankLostImpressionShare;
  }
  public function getSearchRankLostImpressionShare()
  {
    return $this->searchRankLostImpressionShare;
  }
  public function setSearchRankLostTopImpressionShare($searchRankLostTopImpressionShare)
  {
    $this->searchRankLostTopImpressionShare = $searchRankLostTopImpressionShare;
  }
  public function getSearchRankLostTopImpressionShare()
  {
    return $this->searchRankLostTopImpressionShare;
  }
  public function setSearchTopImpressionShare($searchTopImpressionShare)
  {
    $this->searchTopImpressionShare = $searchTopImpressionShare;
  }
  public function getSearchTopImpressionShare()
  {
    return $this->searchTopImpressionShare;
  }
  public function setTopImpressionPercentage($topImpressionPercentage)
  {
    $this->topImpressionPercentage = $topImpressionPercentage;
  }
  public function getTopImpressionPercentage()
  {
    return $this->topImpressionPercentage;
  }
  public function setValuePerAllConversions($valuePerAllConversions)
  {
    $this->valuePerAllConversions = $valuePerAllConversions;
  }
  public function getValuePerAllConversions()
  {
    return $this->valuePerAllConversions;
  }
  public function setValuePerAllConversionsByConversionDate($valuePerAllConversionsByConversionDate)
  {
    $this->valuePerAllConversionsByConversionDate = $valuePerAllConversionsByConversionDate;
  }
  public function getValuePerAllConversionsByConversionDate()
  {
    return $this->valuePerAllConversionsByConversionDate;
  }
  public function setValuePerConversion($valuePerConversion)
  {
    $this->valuePerConversion = $valuePerConversion;
  }
  public function getValuePerConversion()
  {
    return $this->valuePerConversion;
  }
  public function setValuePerConversionsByConversionDate($valuePerConversionsByConversionDate)
  {
    $this->valuePerConversionsByConversionDate = $valuePerConversionsByConversionDate;
  }
  public function getValuePerConversionsByConversionDate()
  {
    return $this->valuePerConversionsByConversionDate;
  }
  public function setVisits($visits)
  {
    $this->visits = $visits;
  }
  public function getVisits()
  {
    return $this->visits;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0CommonMetrics::class, 'Google_Service_SA360_GoogleAdsSearchads360V0CommonMetrics');
