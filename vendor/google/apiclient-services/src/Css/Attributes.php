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

namespace Google\Service\Css;

class Attributes extends \Google\Collection
{
  protected $collection_key = 'sizeTypes';
  /**
   * @var string[]
   */
  public $additionalImageLinks;
  /**
   * @var bool
   */
  public $adult;
  /**
   * @var string
   */
  public $ageGroup;
  /**
   * @var string
   */
  public $brand;
  protected $certificationsType = Certification::class;
  protected $certificationsDataType = 'array';
  /**
   * @var string
   */
  public $color;
  /**
   * @var string
   */
  public $cppAdsRedirect;
  /**
   * @var string
   */
  public $cppLink;
  /**
   * @var string
   */
  public $cppMobileLink;
  /**
   * @var string
   */
  public $customLabel0;
  /**
   * @var string
   */
  public $customLabel1;
  /**
   * @var string
   */
  public $customLabel2;
  /**
   * @var string
   */
  public $customLabel3;
  /**
   * @var string
   */
  public $customLabel4;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string[]
   */
  public $excludedDestinations;
  /**
   * @var string
   */
  public $expirationDate;
  /**
   * @var string
   */
  public $gender;
  /**
   * @var string
   */
  public $googleProductCategory;
  /**
   * @var string
   */
  public $gtin;
  /**
   * @var string
   */
  public $headlineOfferCondition;
  protected $headlineOfferInstallmentType = HeadlineOfferInstallment::class;
  protected $headlineOfferInstallmentDataType = '';
  /**
   * @var string
   */
  public $headlineOfferLink;
  /**
   * @var string
   */
  public $headlineOfferMobileLink;
  protected $headlineOfferPriceType = Price::class;
  protected $headlineOfferPriceDataType = '';
  protected $headlineOfferShippingPriceType = Price::class;
  protected $headlineOfferShippingPriceDataType = '';
  protected $headlineOfferSubscriptionCostType = HeadlineOfferSubscriptionCost::class;
  protected $headlineOfferSubscriptionCostDataType = '';
  protected $highPriceType = Price::class;
  protected $highPriceDataType = '';
  /**
   * @var string
   */
  public $imageLink;
  /**
   * @var string[]
   */
  public $includedDestinations;
  /**
   * @var bool
   */
  public $isBundle;
  /**
   * @var string
   */
  public $itemGroupId;
  protected $lowPriceType = Price::class;
  protected $lowPriceDataType = '';
  /**
   * @var string
   */
  public $material;
  /**
   * @var string
   */
  public $mpn;
  /**
   * @var string
   */
  public $multipack;
  /**
   * @var string
   */
  public $numberOfOffers;
  /**
   * @var string
   */
  public $pattern;
  /**
   * @var string
   */
  public $pause;
  protected $productDetailsType = ProductDetail::class;
  protected $productDetailsDataType = 'array';
  protected $productHeightType = ProductDimension::class;
  protected $productHeightDataType = '';
  /**
   * @var string[]
   */
  public $productHighlights;
  protected $productLengthType = ProductDimension::class;
  protected $productLengthDataType = '';
  /**
   * @var string[]
   */
  public $productTypes;
  protected $productWeightType = ProductWeight::class;
  protected $productWeightDataType = '';
  protected $productWidthType = ProductDimension::class;
  protected $productWidthDataType = '';
  /**
   * @var string
   */
  public $size;
  /**
   * @var string
   */
  public $sizeSystem;
  /**
   * @var string[]
   */
  public $sizeTypes;
  /**
   * @var string
   */
  public $title;

  /**
   * @param string[]
   */
  public function setAdditionalImageLinks($additionalImageLinks)
  {
    $this->additionalImageLinks = $additionalImageLinks;
  }
  /**
   * @return string[]
   */
  public function getAdditionalImageLinks()
  {
    return $this->additionalImageLinks;
  }
  /**
   * @param bool
   */
  public function setAdult($adult)
  {
    $this->adult = $adult;
  }
  /**
   * @return bool
   */
  public function getAdult()
  {
    return $this->adult;
  }
  /**
   * @param string
   */
  public function setAgeGroup($ageGroup)
  {
    $this->ageGroup = $ageGroup;
  }
  /**
   * @return string
   */
  public function getAgeGroup()
  {
    return $this->ageGroup;
  }
  /**
   * @param string
   */
  public function setBrand($brand)
  {
    $this->brand = $brand;
  }
  /**
   * @return string
   */
  public function getBrand()
  {
    return $this->brand;
  }
  /**
   * @param Certification[]
   */
  public function setCertifications($certifications)
  {
    $this->certifications = $certifications;
  }
  /**
   * @return Certification[]
   */
  public function getCertifications()
  {
    return $this->certifications;
  }
  /**
   * @param string
   */
  public function setColor($color)
  {
    $this->color = $color;
  }
  /**
   * @return string
   */
  public function getColor()
  {
    return $this->color;
  }
  /**
   * @param string
   */
  public function setCppAdsRedirect($cppAdsRedirect)
  {
    $this->cppAdsRedirect = $cppAdsRedirect;
  }
  /**
   * @return string
   */
  public function getCppAdsRedirect()
  {
    return $this->cppAdsRedirect;
  }
  /**
   * @param string
   */
  public function setCppLink($cppLink)
  {
    $this->cppLink = $cppLink;
  }
  /**
   * @return string
   */
  public function getCppLink()
  {
    return $this->cppLink;
  }
  /**
   * @param string
   */
  public function setCppMobileLink($cppMobileLink)
  {
    $this->cppMobileLink = $cppMobileLink;
  }
  /**
   * @return string
   */
  public function getCppMobileLink()
  {
    return $this->cppMobileLink;
  }
  /**
   * @param string
   */
  public function setCustomLabel0($customLabel0)
  {
    $this->customLabel0 = $customLabel0;
  }
  /**
   * @return string
   */
  public function getCustomLabel0()
  {
    return $this->customLabel0;
  }
  /**
   * @param string
   */
  public function setCustomLabel1($customLabel1)
  {
    $this->customLabel1 = $customLabel1;
  }
  /**
   * @return string
   */
  public function getCustomLabel1()
  {
    return $this->customLabel1;
  }
  /**
   * @param string
   */
  public function setCustomLabel2($customLabel2)
  {
    $this->customLabel2 = $customLabel2;
  }
  /**
   * @return string
   */
  public function getCustomLabel2()
  {
    return $this->customLabel2;
  }
  /**
   * @param string
   */
  public function setCustomLabel3($customLabel3)
  {
    $this->customLabel3 = $customLabel3;
  }
  /**
   * @return string
   */
  public function getCustomLabel3()
  {
    return $this->customLabel3;
  }
  /**
   * @param string
   */
  public function setCustomLabel4($customLabel4)
  {
    $this->customLabel4 = $customLabel4;
  }
  /**
   * @return string
   */
  public function getCustomLabel4()
  {
    return $this->customLabel4;
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
   * @param string[]
   */
  public function setExcludedDestinations($excludedDestinations)
  {
    $this->excludedDestinations = $excludedDestinations;
  }
  /**
   * @return string[]
   */
  public function getExcludedDestinations()
  {
    return $this->excludedDestinations;
  }
  /**
   * @param string
   */
  public function setExpirationDate($expirationDate)
  {
    $this->expirationDate = $expirationDate;
  }
  /**
   * @return string
   */
  public function getExpirationDate()
  {
    return $this->expirationDate;
  }
  /**
   * @param string
   */
  public function setGender($gender)
  {
    $this->gender = $gender;
  }
  /**
   * @return string
   */
  public function getGender()
  {
    return $this->gender;
  }
  /**
   * @param string
   */
  public function setGoogleProductCategory($googleProductCategory)
  {
    $this->googleProductCategory = $googleProductCategory;
  }
  /**
   * @return string
   */
  public function getGoogleProductCategory()
  {
    return $this->googleProductCategory;
  }
  /**
   * @param string
   */
  public function setGtin($gtin)
  {
    $this->gtin = $gtin;
  }
  /**
   * @return string
   */
  public function getGtin()
  {
    return $this->gtin;
  }
  /**
   * @param string
   */
  public function setHeadlineOfferCondition($headlineOfferCondition)
  {
    $this->headlineOfferCondition = $headlineOfferCondition;
  }
  /**
   * @return string
   */
  public function getHeadlineOfferCondition()
  {
    return $this->headlineOfferCondition;
  }
  /**
   * @param HeadlineOfferInstallment
   */
  public function setHeadlineOfferInstallment(HeadlineOfferInstallment $headlineOfferInstallment)
  {
    $this->headlineOfferInstallment = $headlineOfferInstallment;
  }
  /**
   * @return HeadlineOfferInstallment
   */
  public function getHeadlineOfferInstallment()
  {
    return $this->headlineOfferInstallment;
  }
  /**
   * @param string
   */
  public function setHeadlineOfferLink($headlineOfferLink)
  {
    $this->headlineOfferLink = $headlineOfferLink;
  }
  /**
   * @return string
   */
  public function getHeadlineOfferLink()
  {
    return $this->headlineOfferLink;
  }
  /**
   * @param string
   */
  public function setHeadlineOfferMobileLink($headlineOfferMobileLink)
  {
    $this->headlineOfferMobileLink = $headlineOfferMobileLink;
  }
  /**
   * @return string
   */
  public function getHeadlineOfferMobileLink()
  {
    return $this->headlineOfferMobileLink;
  }
  /**
   * @param Price
   */
  public function setHeadlineOfferPrice(Price $headlineOfferPrice)
  {
    $this->headlineOfferPrice = $headlineOfferPrice;
  }
  /**
   * @return Price
   */
  public function getHeadlineOfferPrice()
  {
    return $this->headlineOfferPrice;
  }
  /**
   * @param Price
   */
  public function setHeadlineOfferShippingPrice(Price $headlineOfferShippingPrice)
  {
    $this->headlineOfferShippingPrice = $headlineOfferShippingPrice;
  }
  /**
   * @return Price
   */
  public function getHeadlineOfferShippingPrice()
  {
    return $this->headlineOfferShippingPrice;
  }
  /**
   * @param HeadlineOfferSubscriptionCost
   */
  public function setHeadlineOfferSubscriptionCost(HeadlineOfferSubscriptionCost $headlineOfferSubscriptionCost)
  {
    $this->headlineOfferSubscriptionCost = $headlineOfferSubscriptionCost;
  }
  /**
   * @return HeadlineOfferSubscriptionCost
   */
  public function getHeadlineOfferSubscriptionCost()
  {
    return $this->headlineOfferSubscriptionCost;
  }
  /**
   * @param Price
   */
  public function setHighPrice(Price $highPrice)
  {
    $this->highPrice = $highPrice;
  }
  /**
   * @return Price
   */
  public function getHighPrice()
  {
    return $this->highPrice;
  }
  /**
   * @param string
   */
  public function setImageLink($imageLink)
  {
    $this->imageLink = $imageLink;
  }
  /**
   * @return string
   */
  public function getImageLink()
  {
    return $this->imageLink;
  }
  /**
   * @param string[]
   */
  public function setIncludedDestinations($includedDestinations)
  {
    $this->includedDestinations = $includedDestinations;
  }
  /**
   * @return string[]
   */
  public function getIncludedDestinations()
  {
    return $this->includedDestinations;
  }
  /**
   * @param bool
   */
  public function setIsBundle($isBundle)
  {
    $this->isBundle = $isBundle;
  }
  /**
   * @return bool
   */
  public function getIsBundle()
  {
    return $this->isBundle;
  }
  /**
   * @param string
   */
  public function setItemGroupId($itemGroupId)
  {
    $this->itemGroupId = $itemGroupId;
  }
  /**
   * @return string
   */
  public function getItemGroupId()
  {
    return $this->itemGroupId;
  }
  /**
   * @param Price
   */
  public function setLowPrice(Price $lowPrice)
  {
    $this->lowPrice = $lowPrice;
  }
  /**
   * @return Price
   */
  public function getLowPrice()
  {
    return $this->lowPrice;
  }
  /**
   * @param string
   */
  public function setMaterial($material)
  {
    $this->material = $material;
  }
  /**
   * @return string
   */
  public function getMaterial()
  {
    return $this->material;
  }
  /**
   * @param string
   */
  public function setMpn($mpn)
  {
    $this->mpn = $mpn;
  }
  /**
   * @return string
   */
  public function getMpn()
  {
    return $this->mpn;
  }
  /**
   * @param string
   */
  public function setMultipack($multipack)
  {
    $this->multipack = $multipack;
  }
  /**
   * @return string
   */
  public function getMultipack()
  {
    return $this->multipack;
  }
  /**
   * @param string
   */
  public function setNumberOfOffers($numberOfOffers)
  {
    $this->numberOfOffers = $numberOfOffers;
  }
  /**
   * @return string
   */
  public function getNumberOfOffers()
  {
    return $this->numberOfOffers;
  }
  /**
   * @param string
   */
  public function setPattern($pattern)
  {
    $this->pattern = $pattern;
  }
  /**
   * @return string
   */
  public function getPattern()
  {
    return $this->pattern;
  }
  /**
   * @param string
   */
  public function setPause($pause)
  {
    $this->pause = $pause;
  }
  /**
   * @return string
   */
  public function getPause()
  {
    return $this->pause;
  }
  /**
   * @param ProductDetail[]
   */
  public function setProductDetails($productDetails)
  {
    $this->productDetails = $productDetails;
  }
  /**
   * @return ProductDetail[]
   */
  public function getProductDetails()
  {
    return $this->productDetails;
  }
  /**
   * @param ProductDimension
   */
  public function setProductHeight(ProductDimension $productHeight)
  {
    $this->productHeight = $productHeight;
  }
  /**
   * @return ProductDimension
   */
  public function getProductHeight()
  {
    return $this->productHeight;
  }
  /**
   * @param string[]
   */
  public function setProductHighlights($productHighlights)
  {
    $this->productHighlights = $productHighlights;
  }
  /**
   * @return string[]
   */
  public function getProductHighlights()
  {
    return $this->productHighlights;
  }
  /**
   * @param ProductDimension
   */
  public function setProductLength(ProductDimension $productLength)
  {
    $this->productLength = $productLength;
  }
  /**
   * @return ProductDimension
   */
  public function getProductLength()
  {
    return $this->productLength;
  }
  /**
   * @param string[]
   */
  public function setProductTypes($productTypes)
  {
    $this->productTypes = $productTypes;
  }
  /**
   * @return string[]
   */
  public function getProductTypes()
  {
    return $this->productTypes;
  }
  /**
   * @param ProductWeight
   */
  public function setProductWeight(ProductWeight $productWeight)
  {
    $this->productWeight = $productWeight;
  }
  /**
   * @return ProductWeight
   */
  public function getProductWeight()
  {
    return $this->productWeight;
  }
  /**
   * @param ProductDimension
   */
  public function setProductWidth(ProductDimension $productWidth)
  {
    $this->productWidth = $productWidth;
  }
  /**
   * @return ProductDimension
   */
  public function getProductWidth()
  {
    return $this->productWidth;
  }
  /**
   * @param string
   */
  public function setSize($size)
  {
    $this->size = $size;
  }
  /**
   * @return string
   */
  public function getSize()
  {
    return $this->size;
  }
  /**
   * @param string
   */
  public function setSizeSystem($sizeSystem)
  {
    $this->sizeSystem = $sizeSystem;
  }
  /**
   * @return string
   */
  public function getSizeSystem()
  {
    return $this->sizeSystem;
  }
  /**
   * @param string[]
   */
  public function setSizeTypes($sizeTypes)
  {
    $this->sizeTypes = $sizeTypes;
  }
  /**
   * @return string[]
   */
  public function getSizeTypes()
  {
    return $this->sizeTypes;
  }
  /**
   * @param string
   */
  public function setTitle($title)
  {
    $this->title = $title;
  }
  /**
   * @return string
   */
  public function getTitle()
  {
    return $this->title;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Attributes::class, 'Google_Service_Css_Attributes');
