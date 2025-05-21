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

class GoogleAdsSearchads360V0ResourcesCampaignShoppingSetting extends \Google\Model
{
  /**
   * @var int
   */
  public $campaignPriority;
  /**
   * @var bool
   */
  public $enableLocal;
  /**
   * @var string
   */
  public $feedLabel;
  /**
   * @var string
   */
  public $merchantId;
  /**
   * @var string
   */
  public $salesCountry;
  /**
   * @var bool
   */
  public $useVehicleInventory;

  /**
   * @param int
   */
  public function setCampaignPriority($campaignPriority)
  {
    $this->campaignPriority = $campaignPriority;
  }
  /**
   * @return int
   */
  public function getCampaignPriority()
  {
    return $this->campaignPriority;
  }
  /**
   * @param bool
   */
  public function setEnableLocal($enableLocal)
  {
    $this->enableLocal = $enableLocal;
  }
  /**
   * @return bool
   */
  public function getEnableLocal()
  {
    return $this->enableLocal;
  }
  /**
   * @param string
   */
  public function setFeedLabel($feedLabel)
  {
    $this->feedLabel = $feedLabel;
  }
  /**
   * @return string
   */
  public function getFeedLabel()
  {
    return $this->feedLabel;
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
  public function setSalesCountry($salesCountry)
  {
    $this->salesCountry = $salesCountry;
  }
  /**
   * @return string
   */
  public function getSalesCountry()
  {
    return $this->salesCountry;
  }
  /**
   * @param bool
   */
  public function setUseVehicleInventory($useVehicleInventory)
  {
    $this->useVehicleInventory = $useVehicleInventory;
  }
  /**
   * @return bool
   */
  public function getUseVehicleInventory()
  {
    return $this->useVehicleInventory;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ResourcesCampaignShoppingSetting::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ResourcesCampaignShoppingSetting');
