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

namespace Google\Service\AndroidPublisher;

class SubscriptionPurchaseLineItem extends \Google\Model
{
  protected $autoRenewingPlanType = AutoRenewingPlan::class;
  protected $autoRenewingPlanDataType = '';
  protected $deferredItemReplacementType = DeferredItemReplacement::class;
  protected $deferredItemReplacementDataType = '';
  /**
   * @var string
   */
  public $expiryTime;
  protected $offerDetailsType = OfferDetails::class;
  protected $offerDetailsDataType = '';
  protected $prepaidPlanType = PrepaidPlan::class;
  protected $prepaidPlanDataType = '';
  /**
   * @var string
   */
  public $productId;
  protected $signupPromotionType = SignupPromotion::class;
  protected $signupPromotionDataType = '';

  /**
   * @param AutoRenewingPlan
   */
  public function setAutoRenewingPlan(AutoRenewingPlan $autoRenewingPlan)
  {
    $this->autoRenewingPlan = $autoRenewingPlan;
  }
  /**
   * @return AutoRenewingPlan
   */
  public function getAutoRenewingPlan()
  {
    return $this->autoRenewingPlan;
  }
  /**
   * @param DeferredItemReplacement
   */
  public function setDeferredItemReplacement(DeferredItemReplacement $deferredItemReplacement)
  {
    $this->deferredItemReplacement = $deferredItemReplacement;
  }
  /**
   * @return DeferredItemReplacement
   */
  public function getDeferredItemReplacement()
  {
    return $this->deferredItemReplacement;
  }
  /**
   * @param string
   */
  public function setExpiryTime($expiryTime)
  {
    $this->expiryTime = $expiryTime;
  }
  /**
   * @return string
   */
  public function getExpiryTime()
  {
    return $this->expiryTime;
  }
  /**
   * @param OfferDetails
   */
  public function setOfferDetails(OfferDetails $offerDetails)
  {
    $this->offerDetails = $offerDetails;
  }
  /**
   * @return OfferDetails
   */
  public function getOfferDetails()
  {
    return $this->offerDetails;
  }
  /**
   * @param PrepaidPlan
   */
  public function setPrepaidPlan(PrepaidPlan $prepaidPlan)
  {
    $this->prepaidPlan = $prepaidPlan;
  }
  /**
   * @return PrepaidPlan
   */
  public function getPrepaidPlan()
  {
    return $this->prepaidPlan;
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
   * @param SignupPromotion
   */
  public function setSignupPromotion(SignupPromotion $signupPromotion)
  {
    $this->signupPromotion = $signupPromotion;
  }
  /**
   * @return SignupPromotion
   */
  public function getSignupPromotion()
  {
    return $this->signupPromotion;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SubscriptionPurchaseLineItem::class, 'Google_Service_AndroidPublisher_SubscriptionPurchaseLineItem');
