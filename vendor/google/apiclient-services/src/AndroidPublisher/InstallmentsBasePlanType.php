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

class InstallmentsBasePlanType extends \Google\Model
{
  /**
   * @var string
   */
  public $accountHoldDuration;
  /**
   * @var string
   */
  public $billingPeriodDuration;
  /**
   * @var int
   */
  public $committedPaymentsCount;
  /**
   * @var string
   */
  public $gracePeriodDuration;
  /**
   * @var string
   */
  public $prorationMode;
  /**
   * @var string
   */
  public $renewalType;
  /**
   * @var string
   */
  public $resubscribeState;

  /**
   * @param string
   */
  public function setAccountHoldDuration($accountHoldDuration)
  {
    $this->accountHoldDuration = $accountHoldDuration;
  }
  /**
   * @return string
   */
  public function getAccountHoldDuration()
  {
    return $this->accountHoldDuration;
  }
  /**
   * @param string
   */
  public function setBillingPeriodDuration($billingPeriodDuration)
  {
    $this->billingPeriodDuration = $billingPeriodDuration;
  }
  /**
   * @return string
   */
  public function getBillingPeriodDuration()
  {
    return $this->billingPeriodDuration;
  }
  /**
   * @param int
   */
  public function setCommittedPaymentsCount($committedPaymentsCount)
  {
    $this->committedPaymentsCount = $committedPaymentsCount;
  }
  /**
   * @return int
   */
  public function getCommittedPaymentsCount()
  {
    return $this->committedPaymentsCount;
  }
  /**
   * @param string
   */
  public function setGracePeriodDuration($gracePeriodDuration)
  {
    $this->gracePeriodDuration = $gracePeriodDuration;
  }
  /**
   * @return string
   */
  public function getGracePeriodDuration()
  {
    return $this->gracePeriodDuration;
  }
  /**
   * @param string
   */
  public function setProrationMode($prorationMode)
  {
    $this->prorationMode = $prorationMode;
  }
  /**
   * @return string
   */
  public function getProrationMode()
  {
    return $this->prorationMode;
  }
  /**
   * @param string
   */
  public function setRenewalType($renewalType)
  {
    $this->renewalType = $renewalType;
  }
  /**
   * @return string
   */
  public function getRenewalType()
  {
    return $this->renewalType;
  }
  /**
   * @param string
   */
  public function setResubscribeState($resubscribeState)
  {
    $this->resubscribeState = $resubscribeState;
  }
  /**
   * @return string
   */
  public function getResubscribeState()
  {
    return $this->resubscribeState;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(InstallmentsBasePlanType::class, 'Google_Service_AndroidPublisher_InstallmentsBasePlanType');
