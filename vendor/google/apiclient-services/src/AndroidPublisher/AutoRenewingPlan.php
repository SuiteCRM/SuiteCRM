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

class AutoRenewingPlan extends \Google\Model
{
  /**
   * @var bool
   */
  public $autoRenewEnabled;
  protected $installmentDetailsType = InstallmentPlan::class;
  protected $installmentDetailsDataType = '';
  protected $priceChangeDetailsType = SubscriptionItemPriceChangeDetails::class;
  protected $priceChangeDetailsDataType = '';
  protected $recurringPriceType = Money::class;
  protected $recurringPriceDataType = '';

  /**
   * @param bool
   */
  public function setAutoRenewEnabled($autoRenewEnabled)
  {
    $this->autoRenewEnabled = $autoRenewEnabled;
  }
  /**
   * @return bool
   */
  public function getAutoRenewEnabled()
  {
    return $this->autoRenewEnabled;
  }
  /**
   * @param InstallmentPlan
   */
  public function setInstallmentDetails(InstallmentPlan $installmentDetails)
  {
    $this->installmentDetails = $installmentDetails;
  }
  /**
   * @return InstallmentPlan
   */
  public function getInstallmentDetails()
  {
    return $this->installmentDetails;
  }
  /**
   * @param SubscriptionItemPriceChangeDetails
   */
  public function setPriceChangeDetails(SubscriptionItemPriceChangeDetails $priceChangeDetails)
  {
    $this->priceChangeDetails = $priceChangeDetails;
  }
  /**
   * @return SubscriptionItemPriceChangeDetails
   */
  public function getPriceChangeDetails()
  {
    return $this->priceChangeDetails;
  }
  /**
   * @param Money
   */
  public function setRecurringPrice(Money $recurringPrice)
  {
    $this->recurringPrice = $recurringPrice;
  }
  /**
   * @return Money
   */
  public function getRecurringPrice()
  {
    return $this->recurringPrice;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AutoRenewingPlan::class, 'Google_Service_AndroidPublisher_AutoRenewingPlan');
