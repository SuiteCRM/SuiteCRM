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

namespace Google\Service\Solar;

class CashPurchaseSavings extends \Google\Model
{
  protected $outOfPocketCostType = Money::class;
  protected $outOfPocketCostDataType = '';
  /**
   * @var float
   */
  public $paybackYears;
  protected $rebateValueType = Money::class;
  protected $rebateValueDataType = '';
  protected $savingsType = SavingsOverTime::class;
  protected $savingsDataType = '';
  protected $upfrontCostType = Money::class;
  protected $upfrontCostDataType = '';

  /**
   * @param Money
   */
  public function setOutOfPocketCost(Money $outOfPocketCost)
  {
    $this->outOfPocketCost = $outOfPocketCost;
  }
  /**
   * @return Money
   */
  public function getOutOfPocketCost()
  {
    return $this->outOfPocketCost;
  }
  /**
   * @param float
   */
  public function setPaybackYears($paybackYears)
  {
    $this->paybackYears = $paybackYears;
  }
  /**
   * @return float
   */
  public function getPaybackYears()
  {
    return $this->paybackYears;
  }
  /**
   * @param Money
   */
  public function setRebateValue(Money $rebateValue)
  {
    $this->rebateValue = $rebateValue;
  }
  /**
   * @return Money
   */
  public function getRebateValue()
  {
    return $this->rebateValue;
  }
  /**
   * @param SavingsOverTime
   */
  public function setSavings(SavingsOverTime $savings)
  {
    $this->savings = $savings;
  }
  /**
   * @return SavingsOverTime
   */
  public function getSavings()
  {
    return $this->savings;
  }
  /**
   * @param Money
   */
  public function setUpfrontCost(Money $upfrontCost)
  {
    $this->upfrontCost = $upfrontCost;
  }
  /**
   * @return Money
   */
  public function getUpfrontCost()
  {
    return $this->upfrontCost;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CashPurchaseSavings::class, 'Google_Service_Solar_CashPurchaseSavings');
