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

class GoogleAdsSearchads360V0ResourcesVisit extends \Google\Model
{
  /**
   * @var string
   */
  public $adId;
  /**
   * @var string
   */
  public $assetFieldType;
  /**
   * @var string
   */
  public $assetId;
  /**
   * @var string
   */
  public $clickId;
  /**
   * @var string
   */
  public $criterionId;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $merchantId;
  /**
   * @var string
   */
  public $productChannel;
  /**
   * @var string
   */
  public $productCountryCode;
  /**
   * @var string
   */
  public $productId;
  /**
   * @var string
   */
  public $productLanguageCode;
  /**
   * @var string
   */
  public $productStoreId;
  /**
   * @var string
   */
  public $resourceName;
  /**
   * @var string
   */
  public $visitDateTime;

  /**
   * @param string
   */
  public function setAdId($adId)
  {
    $this->adId = $adId;
  }
  /**
   * @return string
   */
  public function getAdId()
  {
    return $this->adId;
  }
  /**
   * @param string
   */
  public function setAssetFieldType($assetFieldType)
  {
    $this->assetFieldType = $assetFieldType;
  }
  /**
   * @return string
   */
  public function getAssetFieldType()
  {
    return $this->assetFieldType;
  }
  /**
   * @param string
   */
  public function setAssetId($assetId)
  {
    $this->assetId = $assetId;
  }
  /**
   * @return string
   */
  public function getAssetId()
  {
    return $this->assetId;
  }
  /**
   * @param string
   */
  public function setClickId($clickId)
  {
    $this->clickId = $clickId;
  }
  /**
   * @return string
   */
  public function getClickId()
  {
    return $this->clickId;
  }
  /**
   * @param string
   */
  public function setCriterionId($criterionId)
  {
    $this->criterionId = $criterionId;
  }
  /**
   * @return string
   */
  public function getCriterionId()
  {
    return $this->criterionId;
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
  public function setMerchantId($merchantId)
  {
    $this->merchantId = $merchantId;
  }
  /**
   * @return string
   */
  public function getMerchantId()
  {
    return $this->merchantId;
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
  public function setProductCountryCode($productCountryCode)
  {
    $this->productCountryCode = $productCountryCode;
  }
  /**
   * @return string
   */
  public function getProductCountryCode()
  {
    return $this->productCountryCode;
  }
  /**
   * @param string
   */
  public function setProductId($productId)
  {
    $this->productId = $productId;
  }
  /**
   * @return string
   */
  public function getProductId()
  {
    return $this->productId;
  }
  /**
   * @param string
   */
  public function setProductLanguageCode($productLanguageCode)
  {
    $this->productLanguageCode = $productLanguageCode;
  }
  /**
   * @return string
   */
  public function getProductLanguageCode()
  {
    return $this->productLanguageCode;
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
  public function setVisitDateTime($visitDateTime)
  {
    $this->visitDateTime = $visitDateTime;
  }
  /**
   * @return string
   */
  public function getVisitDateTime()
  {
    return $this->visitDateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ResourcesVisit::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ResourcesVisit');
