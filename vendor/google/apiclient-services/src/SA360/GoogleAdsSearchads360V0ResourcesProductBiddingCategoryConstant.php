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

class GoogleAdsSearchads360V0ResourcesProductBiddingCategoryConstant extends \Google\Model
{
  /**
   * @var string
   */
  public $countryCode;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $languageCode;
  /**
   * @var string
   */
  public $level;
  /**
   * @var string
   */
  public $localizedName;
  /**
   * @var string
   */
  public $productBiddingCategoryConstantParent;
  /**
   * @var string
   */
  public $resourceName;
  /**
   * @var string
   */
  public $status;

  /**
   * @param string
   */
  public function setCountryCode($countryCode)
  {
    $this->countryCode = $countryCode;
  }
  /**
   * @return string
   */
  public function getCountryCode()
  {
    return $this->countryCode;
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
  public function setLanguageCode($languageCode)
  {
    $this->languageCode = $languageCode;
  }
  /**
   * @return string
   */
  public function getLanguageCode()
  {
    return $this->languageCode;
  }
  /**
   * @param string
   */
  public function setLevel($level)
  {
    $this->level = $level;
  }
  /**
   * @return string
   */
  public function getLevel()
  {
    return $this->level;
  }
  /**
   * @param string
   */
  public function setLocalizedName($localizedName)
  {
    $this->localizedName = $localizedName;
  }
  /**
   * @return string
   */
  public function getLocalizedName()
  {
    return $this->localizedName;
  }
  /**
   * @param string
   */
  public function setProductBiddingCategoryConstantParent($productBiddingCategoryConstantParent)
  {
    $this->productBiddingCategoryConstantParent = $productBiddingCategoryConstantParent;
  }
  /**
   * @return string
   */
  public function getProductBiddingCategoryConstantParent()
  {
    return $this->productBiddingCategoryConstantParent;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ResourcesProductBiddingCategoryConstant::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ResourcesProductBiddingCategoryConstant');
