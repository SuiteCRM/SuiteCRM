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

namespace Google\Service\CloudCommercePartnerProcurementService;

class Entitlement extends \Google\Collection
{
  protected $collection_key = 'entitlementBenefitIds';
  /**
   * @var string
   */
  public $account;
  /**
   * @var string
   */
  public $cancellationReason;
  protected $consumersType = Consumer::class;
  protected $consumersDataType = 'array';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string[]
   */
  public $entitlementBenefitIds;
  /**
   * @var array[]
   */
  public $inputProperties;
  /**
   * @var string
   */
  public $messageToUser;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $newOfferEndTime;
  /**
   * @var string
   */
  public $newOfferStartTime;
  /**
   * @var string
   */
  public $newPendingOffer;
  /**
   * @var string
   */
  public $newPendingOfferDuration;
  /**
   * @var string
   */
  public $newPendingPlan;
  /**
   * @var string
   */
  public $offer;
  /**
   * @var string
   */
  public $offerDuration;
  /**
   * @var string
   */
  public $offerEndTime;
  /**
   * @var string
   */
  public $orderId;
  /**
   * @var string
   */
  public $plan;
  /**
   * @var string
   */
  public $product;
  /**
   * @var string
   */
  public $productExternalName;
  /**
   * @var string
   */
  public $provider;
  /**
   * @var string
   */
  public $quoteExternalName;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $subscriptionEndTime;
  /**
   * @var string
   */
  public $updateTime;
  /**
   * @var string
   */
  public $usageReportingId;

  /**
   * @param string
   */
  public function setAccount($account)
  {
    $this->account = $account;
  }
  /**
   * @return string
   */
  public function getAccount()
  {
    return $this->account;
  }
  /**
   * @param string
   */
  public function setCancellationReason($cancellationReason)
  {
    $this->cancellationReason = $cancellationReason;
  }
  /**
   * @return string
   */
  public function getCancellationReason()
  {
    return $this->cancellationReason;
  }
  /**
   * @param Consumer[]
   */
  public function setConsumers($consumers)
  {
    $this->consumers = $consumers;
  }
  /**
   * @return Consumer[]
   */
  public function getConsumers()
  {
    return $this->consumers;
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
   * @param string[]
   */
  public function setEntitlementBenefitIds($entitlementBenefitIds)
  {
    $this->entitlementBenefitIds = $entitlementBenefitIds;
  }
  /**
   * @return string[]
   */
  public function getEntitlementBenefitIds()
  {
    return $this->entitlementBenefitIds;
  }
  /**
   * @param array[]
   */
  public function setInputProperties($inputProperties)
  {
    $this->inputProperties = $inputProperties;
  }
  /**
   * @return array[]
   */
  public function getInputProperties()
  {
    return $this->inputProperties;
  }
  /**
   * @param string
   */
  public function setMessageToUser($messageToUser)
  {
    $this->messageToUser = $messageToUser;
  }
  /**
   * @return string
   */
  public function getMessageToUser()
  {
    return $this->messageToUser;
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
  public function setNewOfferEndTime($newOfferEndTime)
  {
    $this->newOfferEndTime = $newOfferEndTime;
  }
  /**
   * @return string
   */
  public function getNewOfferEndTime()
  {
    return $this->newOfferEndTime;
  }
  /**
   * @param string
   */
  public function setNewOfferStartTime($newOfferStartTime)
  {
    $this->newOfferStartTime = $newOfferStartTime;
  }
  /**
   * @return string
   */
  public function getNewOfferStartTime()
  {
    return $this->newOfferStartTime;
  }
  /**
   * @param string
   */
  public function setNewPendingOffer($newPendingOffer)
  {
    $this->newPendingOffer = $newPendingOffer;
  }
  /**
   * @return string
   */
  public function getNewPendingOffer()
  {
    return $this->newPendingOffer;
  }
  /**
   * @param string
   */
  public function setNewPendingOfferDuration($newPendingOfferDuration)
  {
    $this->newPendingOfferDuration = $newPendingOfferDuration;
  }
  /**
   * @return string
   */
  public function getNewPendingOfferDuration()
  {
    return $this->newPendingOfferDuration;
  }
  /**
   * @param string
   */
  public function setNewPendingPlan($newPendingPlan)
  {
    $this->newPendingPlan = $newPendingPlan;
  }
  /**
   * @return string
   */
  public function getNewPendingPlan()
  {
    return $this->newPendingPlan;
  }
  /**
   * @param string
   */
  public function setOffer($offer)
  {
    $this->offer = $offer;
  }
  /**
   * @return string
   */
  public function getOffer()
  {
    return $this->offer;
  }
  /**
   * @param string
   */
  public function setOfferDuration($offerDuration)
  {
    $this->offerDuration = $offerDuration;
  }
  /**
   * @return string
   */
  public function getOfferDuration()
  {
    return $this->offerDuration;
  }
  /**
   * @param string
   */
  public function setOfferEndTime($offerEndTime)
  {
    $this->offerEndTime = $offerEndTime;
  }
  /**
   * @return string
   */
  public function getOfferEndTime()
  {
    return $this->offerEndTime;
  }
  /**
   * @param string
   */
  public function setOrderId($orderId)
  {
    $this->orderId = $orderId;
  }
  /**
   * @return string
   */
  public function getOrderId()
  {
    return $this->orderId;
  }
  /**
   * @param string
   */
  public function setPlan($plan)
  {
    $this->plan = $plan;
  }
  /**
   * @return string
   */
  public function getPlan()
  {
    return $this->plan;
  }
  /**
   * @param string
   */
  public function setProduct($product)
  {
    $this->product = $product;
  }
  /**
   * @return string
   */
  public function getProduct()
  {
    return $this->product;
  }
  /**
   * @param string
   */
  public function setProductExternalName($productExternalName)
  {
    $this->productExternalName = $productExternalName;
  }
  /**
   * @return string
   */
  public function getProductExternalName()
  {
    return $this->productExternalName;
  }
  /**
   * @param string
   */
  public function setProvider($provider)
  {
    $this->provider = $provider;
  }
  /**
   * @return string
   */
  public function getProvider()
  {
    return $this->provider;
  }
  /**
   * @param string
   */
  public function setQuoteExternalName($quoteExternalName)
  {
    $this->quoteExternalName = $quoteExternalName;
  }
  /**
   * @return string
   */
  public function getQuoteExternalName()
  {
    return $this->quoteExternalName;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param string
   */
  public function setSubscriptionEndTime($subscriptionEndTime)
  {
    $this->subscriptionEndTime = $subscriptionEndTime;
  }
  /**
   * @return string
   */
  public function getSubscriptionEndTime()
  {
    return $this->subscriptionEndTime;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
  /**
   * @param string
   */
  public function setUsageReportingId($usageReportingId)
  {
    $this->usageReportingId = $usageReportingId;
  }
  /**
   * @return string
   */
  public function getUsageReportingId()
  {
    return $this->usageReportingId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Entitlement::class, 'Google_Service_CloudCommercePartnerProcurementService_Entitlement');
