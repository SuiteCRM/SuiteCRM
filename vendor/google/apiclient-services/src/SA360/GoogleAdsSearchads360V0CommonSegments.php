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

class GoogleAdsSearchads360V0CommonSegments extends \Google\Collection
{
  protected $collection_key = 'rawEventConversionDimensions';
  /**
   * @var string
   */
  public $adNetworkType;
  protected $assetInteractionTargetType = GoogleAdsSearchads360V0CommonAssetInteractionTarget::class;
  protected $assetInteractionTargetDataType = '';
  /**
   * @var string
   */
  public $conversionAction;
  /**
   * @var string
   */
  public $conversionActionCategory;
  /**
   * @var string
   */
  public $conversionActionName;
  protected $conversionCustomDimensionsType = GoogleAdsSearchads360V0CommonValue::class;
  protected $conversionCustomDimensionsDataType = 'array';
  /**
   * @var string
   */
  public $date;
  /**
   * @var string
   */
  public $dayOfWeek;
  /**
   * @var string
   */
  public $device;
  /**
   * @var string
   */
  public $geoTargetCity;
  /**
   * @var string
   */
  public $geoTargetCountry;
  /**
   * @var string
   */
  public $geoTargetMetro;
  /**
   * @var string
   */
  public $geoTargetRegion;
  /**
   * @var int
   */
  public $hour;
  protected $keywordType = GoogleAdsSearchads360V0CommonKeyword::class;
  protected $keywordDataType = '';
  /**
   * @var string
   */
  public $month;
  /**
   * @var string
   */
  public $productBiddingCategoryLevel1;
  /**
   * @var string
   */
  public $productBiddingCategoryLevel2;
  /**
   * @var string
   */
  public $productBiddingCategoryLevel3;
  /**
   * @var string
   */
  public $productBiddingCategoryLevel4;
  /**
   * @var string
   */
  public $productBiddingCategoryLevel5;
  /**
   * @var string
   */
  public $productBrand;
  /**
   * @var string
   */
  public $productChannel;
  /**
   * @var string
   */
  public $productChannelExclusivity;
  /**
   * @var string
   */
  public $productCondition;
  /**
   * @var string
   */
  public $productCountry;
  /**
   * @var string
   */
  public $productCustomAttribute0;
  /**
   * @var string
   */
  public $productCustomAttribute1;
  /**
   * @var string
   */
  public $productCustomAttribute2;
  /**
   * @var string
   */
  public $productCustomAttribute3;
  /**
   * @var string
   */
  public $productCustomAttribute4;
  /**
   * @var string
   */
  public $productItemId;
  /**
   * @var string
   */
  public $productLanguage;
  /**
   * @var string
   */
  public $productSoldBiddingCategoryLevel1;
  /**
   * @var string
   */
  public $productSoldBiddingCategoryLevel2;
  /**
   * @var string
   */
  public $productSoldBiddingCategoryLevel3;
  /**
   * @var string
   */
  public $productSoldBiddingCategoryLevel4;
  /**
   * @var string
   */
  public $productSoldBiddingCategoryLevel5;
  /**
   * @var string
   */
  public $productSoldBrand;
  /**
   * @var string
   */
  public $productSoldCondition;
  /**
   * @var string
   */
  public $productSoldCustomAttribute0;
  /**
   * @var string
   */
  public $productSoldCustomAttribute1;
  /**
   * @var string
   */
  public $productSoldCustomAttribute2;
  /**
   * @var string
   */
  public $productSoldCustomAttribute3;
  /**
   * @var string
   */
  public $productSoldCustomAttribute4;
  /**
   * @var string
   */
  public $productSoldItemId;
  /**
   * @var string
   */
  public $productSoldTitle;
  /**
   * @var string
   */
  public $productSoldTypeL1;
  /**
   * @var string
   */
  public $productSoldTypeL2;
  /**
   * @var string
   */
  public $productSoldTypeL3;
  /**
   * @var string
   */
  public $productSoldTypeL4;
  /**
   * @var string
   */
  public $productSoldTypeL5;
  /**
   * @var string
   */
  public $productStoreId;
  /**
   * @var string
   */
  public $productTitle;
  /**
   * @var string
   */
  public $productTypeL1;
  /**
   * @var string
   */
  public $productTypeL2;
  /**
   * @var string
   */
  public $productTypeL3;
  /**
   * @var string
   */
  public $productTypeL4;
  /**
   * @var string
   */
  public $productTypeL5;
  /**
   * @var string
   */
  public $quarter;
  protected $rawEventConversionDimensionsType = GoogleAdsSearchads360V0CommonValue::class;
  protected $rawEventConversionDimensionsDataType = 'array';
  /**
   * @var string
   */
  public $week;
  /**
   * @var int
   */
  public $year;

  /**
   * @param string
   */
  public function setAdNetworkType($adNetworkType)
  {
    $this->adNetworkType = $adNetworkType;
  }
  /**
   * @return string
   */
  public function getAdNetworkType()
  {
    return $this->adNetworkType;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonAssetInteractionTarget
   */
  public function setAssetInteractionTarget(GoogleAdsSearchads360V0CommonAssetInteractionTarget $assetInteractionTarget)
  {
    $this->assetInteractionTarget = $assetInteractionTarget;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonAssetInteractionTarget
   */
  public function getAssetInteractionTarget()
  {
    return $this->assetInteractionTarget;
  }
  /**
   * @param string
   */
  public function setConversionAction($conversionAction)
  {
    $this->conversionAction = $conversionAction;
  }
  /**
   * @return string
   */
  public function getConversionAction()
  {
    return $this->conversionAction;
  }
  /**
   * @param string
   */
  public function setConversionActionCategory($conversionActionCategory)
  {
    $this->conversionActionCategory = $conversionActionCategory;
  }
  /**
   * @return string
   */
  public function getConversionActionCategory()
  {
    return $this->conversionActionCategory;
  }
  /**
   * @param string
   */
  public function setConversionActionName($conversionActionName)
  {
    $this->conversionActionName = $conversionActionName;
  }
  /**
   * @return string
   */
  public function getConversionActionName()
  {
    return $this->conversionActionName;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonValue[]
   */
  public function setConversionCustomDimensions($conversionCustomDimensions)
  {
    $this->conversionCustomDimensions = $conversionCustomDimensions;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonValue[]
   */
  public function getConversionCustomDimensions()
  {
    return $this->conversionCustomDimensions;
  }
  /**
   * @param string
   */
  public function setDate($date)
  {
    $this->date = $date;
  }
  /**
   * @return string
   */
  public function getDate()
  {
    return $this->date;
  }
  /**
   * @param string
   */
  public function setDayOfWeek($dayOfWeek)
  {
    $this->dayOfWeek = $dayOfWeek;
  }
  /**
   * @return string
   */
  public function getDayOfWeek()
  {
    return $this->dayOfWeek;
  }
  /**
   * @param string
   */
  public function setDevice($device)
  {
    $this->device = $device;
  }
  /**
   * @return string
   */
  public function getDevice()
  {
    return $this->device;
  }
  /**
   * @param string
   */
  public function setGeoTargetCity($geoTargetCity)
  {
    $this->geoTargetCity = $geoTargetCity;
  }
  /**
   * @return string
   */
  public function getGeoTargetCity()
  {
    return $this->geoTargetCity;
  }
  /**
   * @param string
   */
  public function setGeoTargetCountry($geoTargetCountry)
  {
    $this->geoTargetCountry = $geoTargetCountry;
  }
  /**
   * @return string
   */
  public function getGeoTargetCountry()
  {
    return $this->geoTargetCountry;
  }
  /**
   * @param string
   */
  public function setGeoTargetMetro($geoTargetMetro)
  {
    $this->geoTargetMetro = $geoTargetMetro;
  }
  /**
   * @return string
   */
  public function getGeoTargetMetro()
  {
    return $this->geoTargetMetro;
  }
  /**
   * @param string
   */
  public function setGeoTargetRegion($geoTargetRegion)
  {
    $this->geoTargetRegion = $geoTargetRegion;
  }
  /**
   * @return string
   */
  public function getGeoTargetRegion()
  {
    return $this->geoTargetRegion;
  }
  /**
   * @param int
   */
  public function setHour($hour)
  {
    $this->hour = $hour;
  }
  /**
   * @return int
   */
  public function getHour()
  {
    return $this->hour;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonKeyword
   */
  public function setKeyword(GoogleAdsSearchads360V0CommonKeyword $keyword)
  {
    $this->keyword = $keyword;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonKeyword
   */
  public function getKeyword()
  {
    return $this->keyword;
  }
  /**
   * @param string
   */
  public function setMonth($month)
  {
    $this->month = $month;
  }
  /**
   * @return string
   */
  public function getMonth()
  {
    return $this->month;
  }
  /**
   * @param string
   */
  public function setProductBiddingCategoryLevel1($productBiddingCategoryLevel1)
  {
    $this->productBiddingCategoryLevel1 = $productBiddingCategoryLevel1;
  }
  /**
   * @return string
   */
  public function getProductBiddingCategoryLevel1()
  {
    return $this->productBiddingCategoryLevel1;
  }
  /**
   * @param string
   */
  public function setProductBiddingCategoryLevel2($productBiddingCategoryLevel2)
  {
    $this->productBiddingCategoryLevel2 = $productBiddingCategoryLevel2;
  }
  /**
   * @return string
   */
  public function getProductBiddingCategoryLevel2()
  {
    return $this->productBiddingCategoryLevel2;
  }
  /**
   * @param string
   */
  public function setProductBiddingCategoryLevel3($productBiddingCategoryLevel3)
  {
    $this->productBiddingCategoryLevel3 = $productBiddingCategoryLevel3;
  }
  /**
   * @return string
   */
  public function getProductBiddingCategoryLevel3()
  {
    return $this->productBiddingCategoryLevel3;
  }
  /**
   * @param string
   */
  public function setProductBiddingCategoryLevel4($productBiddingCategoryLevel4)
  {
    $this->productBiddingCategoryLevel4 = $productBiddingCategoryLevel4;
  }
  /**
   * @return string
   */
  public function getProductBiddingCategoryLevel4()
  {
    return $this->productBiddingCategoryLevel4;
  }
  /**
   * @param string
   */
  public function setProductBiddingCategoryLevel5($productBiddingCategoryLevel5)
  {
    $this->productBiddingCategoryLevel5 = $productBiddingCategoryLevel5;
  }
  /**
   * @return string
   */
  public function getProductBiddingCategoryLevel5()
  {
    return $this->productBiddingCategoryLevel5;
  }
  /**
   * @param string
   */
  public function setProductBrand($productBrand)
  {
    $this->productBrand = $productBrand;
  }
  /**
   * @return string
   */
  public function getProductBrand()
  {
    return $this->productBrand;
  }
  /**
   * @param string
   */
  public function setProductChannel($productChannel)
  {
    $this->productChannel = $productChannel;
  }
  /**
   * @return string
   */
  public function getProductChannel()
  {
    return $this->productChannel;
  }
  /**
   * @param string
   */
  public function setProductChannelExclusivity($productChannelExclusivity)
  {
    $this->productChannelExclusivity = $productChannelExclusivity;
  }
  /**
   * @return string
   */
  public function getProductChannelExclusivity()
  {
    return $this->productChannelExclusivity;
  }
  /**
   * @param string
   */
  public function setProductCondition($productCondition)
  {
    $this->productCondition = $productCondition;
  }
  /**
   * @return string
   */
  public function getProductCondition()
  {
    return $this->productCondition;
  }
  /**
   * @param string
   */
  public function setProductCountry($productCountry)
  {
    $this->productCountry = $productCountry;
  }
  /**
   * @return string
   */
  public function getProductCountry()
  {
    return $this->productCountry;
  }
  /**
   * @param string
   */
  public function setProductCustomAttribute0($productCustomAttribute0)
  {
    $this->productCustomAttribute0 = $productCustomAttribute0;
  }
  /**
   * @return string
   */
  public function getProductCustomAttribute0()
  {
    return $this->productCustomAttribute0;
  }
  /**
   * @param string
   */
  public function setProductCustomAttribute1($productCustomAttribute1)
  {
    $this->productCustomAttribute1 = $productCustomAttribute1;
  }
  /**
   * @return string
   */
  public function getProductCustomAttribute1()
  {
    return $this->productCustomAttribute1;
  }
  /**
   * @param string
   */
  public function setProductCustomAttribute2($productCustomAttribute2)
  {
    $this->productCustomAttribute2 = $productCustomAttribute2;
  }
  /**
   * @return string
   */
  public function getProductCustomAttribute2()
  {
    return $this->productCustomAttribute2;
  }
  /**
   * @param string
   */
  public function setProductCustomAttribute3($productCustomAttribute3)
  {
    $this->productCustomAttribute3 = $productCustomAttribute3;
  }
  /**
   * @return string
   */
  public function getProductCustomAttribute3()
  {
    return $this->productCustomAttribute3;
  }
  /**
   * @param string
   */
  public function setProductCustomAttribute4($productCustomAttribute4)
  {
    $this->productCustomAttribute4 = $productCustomAttribute4;
  }
  /**
   * @return string
   */
  public function getProductCustomAttribute4()
  {
    return $this->productCustomAttribute4;
  }
  /**
   * @param string
   */
  public function setProductItemId($productItemId)
  {
    $this->productItemId = $productItemId;
  }
  /**
   * @return string
   */
  public function getProductItemId()
  {
    return $this->productItemId;
  }
  /**
   * @param string
   */
  public function setProductLanguage($productLanguage)
  {
    $this->productLanguage = $productLanguage;
  }
  /**
   * @return string
   */
  public function getProductLanguage()
  {
    return $this->productLanguage;
  }
  /**
   * @param string
   */
  public function setProductSoldBiddingCategoryLevel1($productSoldBiddingCategoryLevel1)
  {
    $this->productSoldBiddingCategoryLevel1 = $productSoldBiddingCategoryLevel1;
  }
  /**
   * @return string
   */
  public function getProductSoldBiddingCategoryLevel1()
  {
    return $this->productSoldBiddingCategoryLevel1;
  }
  /**
   * @param string
   */
  public function setProductSoldBiddingCategoryLevel2($productSoldBiddingCategoryLevel2)
  {
    $this->productSoldBiddingCategoryLevel2 = $productSoldBiddingCategoryLevel2;
  }
  /**
   * @return string
   */
  public function getProductSoldBiddingCategoryLevel2()
  {
    return $this->productSoldBiddingCategoryLevel2;
  }
  /**
   * @param string
   */
  public function setProductSoldBiddingCategoryLevel3($productSoldBiddingCategoryLevel3)
  {
    $this->productSoldBiddingCategoryLevel3 = $productSoldBiddingCategoryLevel3;
  }
  /**
   * @return string
   */
  public function getProductSoldBiddingCategoryLevel3()
  {
    return $this->productSoldBiddingCategoryLevel3;
  }
  /**
   * @param string
   */
  public function setProductSoldBiddingCategoryLevel4($productSoldBiddingCategoryLevel4)
  {
    $this->productSoldBiddingCategoryLevel4 = $productSoldBiddingCategoryLevel4;
  }
  /**
   * @return string
   */
  public function getProductSoldBiddingCategoryLevel4()
  {
    return $this->productSoldBiddingCategoryLevel4;
  }
  /**
   * @param string
   */
  public function setProductSoldBiddingCategoryLevel5($productSoldBiddingCategoryLevel5)
  {
    $this->productSoldBiddingCategoryLevel5 = $productSoldBiddingCategoryLevel5;
  }
  /**
   * @return string
   */
  public function getProductSoldBiddingCategoryLevel5()
  {
    return $this->productSoldBiddingCategoryLevel5;
  }
  /**
   * @param string
   */
  public function setProductSoldBrand($productSoldBrand)
  {
    $this->productSoldBrand = $productSoldBrand;
  }
  /**
   * @return string
   */
  public function getProductSoldBrand()
  {
    return $this->productSoldBrand;
  }
  /**
   * @param string
   */
  public function setProductSoldCondition($productSoldCondition)
  {
    $this->productSoldCondition = $productSoldCondition;
  }
  /**
   * @return string
   */
  public function getProductSoldCondition()
  {
    return $this->productSoldCondition;
  }
  /**
   * @param string
   */
  public function setProductSoldCustomAttribute0($productSoldCustomAttribute0)
  {
    $this->productSoldCustomAttribute0 = $productSoldCustomAttribute0;
  }
  /**
   * @return string
   */
  public function getProductSoldCustomAttribute0()
  {
    return $this->productSoldCustomAttribute0;
  }
  /**
   * @param string
   */
  public function setProductSoldCustomAttribute1($productSoldCustomAttribute1)
  {
    $this->productSoldCustomAttribute1 = $productSoldCustomAttribute1;
  }
  /**
   * @return string
   */
  public function getProductSoldCustomAttribute1()
  {
    return $this->productSoldCustomAttribute1;
  }
  /**
   * @param string
   */
  public function setProductSoldCustomAttribute2($productSoldCustomAttribute2)
  {
    $this->productSoldCustomAttribute2 = $productSoldCustomAttribute2;
  }
  /**
   * @return string
   */
  public function getProductSoldCustomAttribute2()
  {
    return $this->productSoldCustomAttribute2;
  }
  /**
   * @param string
   */
  public function setProductSoldCustomAttribute3($productSoldCustomAttribute3)
  {
    $this->productSoldCustomAttribute3 = $productSoldCustomAttribute3;
  }
  /**
   * @return string
   */
  public function getProductSoldCustomAttribute3()
  {
    return $this->productSoldCustomAttribute3;
  }
  /**
   * @param string
   */
  public function setProductSoldCustomAttribute4($productSoldCustomAttribute4)
  {
    $this->productSoldCustomAttribute4 = $productSoldCustomAttribute4;
  }
  /**
   * @return string
   */
  public function getProductSoldCustomAttribute4()
  {
    return $this->productSoldCustomAttribute4;
  }
  /**
   * @param string
   */
  public function setProductSoldItemId($productSoldItemId)
  {
    $this->productSoldItemId = $productSoldItemId;
  }
  /**
   * @return string
   */
  public function getProductSoldItemId()
  {
    return $this->productSoldItemId;
  }
  /**
   * @param string
   */
  public function setProductSoldTitle($productSoldTitle)
  {
    $this->productSoldTitle = $productSoldTitle;
  }
  /**
   * @return string
   */
  public function getProductSoldTitle()
  {
    return $this->productSoldTitle;
  }
  /**
   * @param string
   */
  public function setProductSoldTypeL1($productSoldTypeL1)
  {
    $this->productSoldTypeL1 = $productSoldTypeL1;
  }
  /**
   * @return string
   */
  public function getProductSoldTypeL1()
  {
    return $this->productSoldTypeL1;
  }
  /**
   * @param string
   */
  public function setProductSoldTypeL2($productSoldTypeL2)
  {
    $this->productSoldTypeL2 = $productSoldTypeL2;
  }
  /**
   * @return string
   */
  public function getProductSoldTypeL2()
  {
    return $this->productSoldTypeL2;
  }
  /**
   * @param string
   */
  public function setProductSoldTypeL3($productSoldTypeL3)
  {
    $this->productSoldTypeL3 = $productSoldTypeL3;
  }
  /**
   * @return string
   */
  public function getProductSoldTypeL3()
  {
    return $this->productSoldTypeL3;
  }
  /**
   * @param string
   */
  public function setProductSoldTypeL4($productSoldTypeL4)
  {
    $this->productSoldTypeL4 = $productSoldTypeL4;
  }
  /**
   * @return string
   */
  public function getProductSoldTypeL4()
  {
    return $this->productSoldTypeL4;
  }
  /**
   * @param string
   */
  public function setProductSoldTypeL5($productSoldTypeL5)
  {
    $this->productSoldTypeL5 = $productSoldTypeL5;
  }
  /**
   * @return string
   */
  public function getProductSoldTypeL5()
  {
    return $this->productSoldTypeL5;
  }
  /**
   * @param string
   */
  public function setProductStoreId($productStoreId)
  {
    $this->productStoreId = $productStoreId;
  }
  /**
   * @return string
   */
  public function getProductStoreId()
  {
    return $this->productStoreId;
  }
  /**
   * @param string
   */
  public function setProductTitle($productTitle)
  {
    $this->productTitle = $productTitle;
  }
  /**
   * @return string
   */
  public function getProductTitle()
  {
    return $this->productTitle;
  }
  /**
   * @param string
   */
  public function setProductTypeL1($productTypeL1)
  {
    $this->productTypeL1 = $productTypeL1;
  }
  /**
   * @return string
   */
  public function getProductTypeL1()
  {
    return $this->productTypeL1;
  }
  /**
   * @param string
   */
  public function setProductTypeL2($productTypeL2)
  {
    $this->productTypeL2 = $productTypeL2;
  }
  /**
   * @return string
   */
  public function getProductTypeL2()
  {
    return $this->productTypeL2;
  }
  /**
   * @param string
   */
  public function setProductTypeL3($productTypeL3)
  {
    $this->productTypeL3 = $productTypeL3;
  }
  /**
   * @return string
   */
  public function getProductTypeL3()
  {
    return $this->productTypeL3;
  }
  /**
   * @param string
   */
  public function setProductTypeL4($productTypeL4)
  {
    $this->productTypeL4 = $productTypeL4;
  }
  /**
   * @return string
   */
  public function getProductTypeL4()
  {
    return $this->productTypeL4;
  }
  /**
   * @param string
   */
  public function setProductTypeL5($productTypeL5)
  {
    $this->productTypeL5 = $productTypeL5;
  }
  /**
   * @return string
   */
  public function getProductTypeL5()
  {
    return $this->productTypeL5;
  }
  /**
   * @param string
   */
  public function setQuarter($quarter)
  {
    $this->quarter = $quarter;
  }
  /**
   * @return string
   */
  public function getQuarter()
  {
    return $this->quarter;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonValue[]
   */
  public function setRawEventConversionDimensions($rawEventConversionDimensions)
  {
    $this->rawEventConversionDimensions = $rawEventConversionDimensions;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonValue[]
   */
  public function getRawEventConversionDimensions()
  {
    return $this->rawEventConversionDimensions;
  }
  /**
   * @param string
   */
  public function setWeek($week)
  {
    $this->week = $week;
  }
  /**
   * @return string
   */
  public function getWeek()
  {
    return $this->week;
  }
  /**
   * @param int
   */
  public function setYear($year)
  {
    $this->year = $year;
  }
  /**
   * @return int
   */
  public function getYear()
  {
    return $this->year;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0CommonSegments::class, 'Google_Service_SA360_GoogleAdsSearchads360V0CommonSegments');
