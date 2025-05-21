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

class GoogleAdsSearchads360V0ResourcesBiddingStrategy extends \Google\Model
{
  /**
   * @var string
   */
  public $campaignCount;
  /**
   * @var string
   */
  public $currencyCode;
  /**
   * @var string
   */
  public $effectiveCurrencyCode;
  protected $enhancedCpcType = GoogleAdsSearchads360V0CommonEnhancedCpc::class;
  protected $enhancedCpcDataType = '';
  /**
   * @var string
   */
  public $id;
  protected $maximizeConversionValueType = GoogleAdsSearchads360V0CommonMaximizeConversionValue::class;
  protected $maximizeConversionValueDataType = '';
  protected $maximizeConversionsType = GoogleAdsSearchads360V0CommonMaximizeConversions::class;
  protected $maximizeConversionsDataType = '';
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $nonRemovedCampaignCount;
  /**
   * @var string
   */
  public $resourceName;
  /**
   * @var string
   */
  public $status;
  protected $targetCpaType = GoogleAdsSearchads360V0CommonTargetCpa::class;
  protected $targetCpaDataType = '';
  protected $targetImpressionShareType = GoogleAdsSearchads360V0CommonTargetImpressionShare::class;
  protected $targetImpressionShareDataType = '';
  protected $targetOutrankShareType = GoogleAdsSearchads360V0CommonTargetOutrankShare::class;
  protected $targetOutrankShareDataType = '';
  protected $targetRoasType = GoogleAdsSearchads360V0CommonTargetRoas::class;
  protected $targetRoasDataType = '';
  protected $targetSpendType = GoogleAdsSearchads360V0CommonTargetSpend::class;
  protected $targetSpendDataType = '';
  /**
   * @var string
   */
  public $type;

  /**
   * @param string
   */
  public function setCampaignCount($campaignCount)
  {
    $this->campaignCount = $campaignCount;
  }
  /**
   * @return string
   */
  public function getCampaignCount()
  {
    return $this->campaignCount;
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
  public function setEffectiveCurrencyCode($effectiveCurrencyCode)
  {
    $this->effectiveCurrencyCode = $effectiveCurrencyCode;
  }
  /**
   * @return string
   */
  public function getEffectiveCurrencyCode()
  {
    return $this->effectiveCurrencyCode;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonEnhancedCpc
   */
  public function setEnhancedCpc(GoogleAdsSearchads360V0CommonEnhancedCpc $enhancedCpc)
  {
    $this->enhancedCpc = $enhancedCpc;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonEnhancedCpc
   */
  public function getEnhancedCpc()
  {
    return $this->enhancedCpc;
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
   * @param string
   */
  public function setNonRemovedCampaignCount($nonRemovedCampaignCount)
  {
    $this->nonRemovedCampaignCount = $nonRemovedCampaignCount;
  }
  /**
   * @return string
   */
  public function getNonRemovedCampaignCount()
  {
    return $this->nonRemovedCampaignCount;
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
   * @param GoogleAdsSearchads360V0CommonTargetOutrankShare
   */
  public function setTargetOutrankShare(GoogleAdsSearchads360V0CommonTargetOutrankShare $targetOutrankShare)
  {
    $this->targetOutrankShare = $targetOutrankShare;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonTargetOutrankShare
   */
  public function getTargetOutrankShare()
  {
    return $this->targetOutrankShare;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ResourcesBiddingStrategy::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ResourcesBiddingStrategy');
