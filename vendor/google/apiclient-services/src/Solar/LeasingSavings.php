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

class LeasingSavings extends \Google\Model
{
  protected $annualLeasingCostType = Money::class;
  protected $annualLeasingCostDataType = '';
  /**
   * @var bool
   */
  public $leasesAllowed;
  /**
   * @var bool
   */
  public $leasesSupported;
  protected $savingsType = SavingsOverTime::class;
  protected $savingsDataType = '';

  /**
   * @param Money
   */
  public function setAnnualLeasingCost(Money $annualLeasingCost)
  {
    $this->annualLeasingCost = $annualLeasingCost;
  }
  /**
   * @return Money
   */
  public function getAnnualLeasingCost()
  {
    return $this->annualLeasingCost;
  }
  /**
   * @param bool
   */
  public function setLeasesAllowed($leasesAllowed)
  {
    $this->leasesAllowed = $leasesAllowed;
  }
  /**
   * @return bool
   */
  public function getLeasesAllowed()
  {
    return $this->leasesAllowed;
  }
  /**
   * @param bool
   */
  public function setLeasesSupported($leasesSupported)
  {
    $this->leasesSupported = $leasesSupported;
  }
  /**
   * @return bool
   */
  public function getLeasesSupported()
  {
    return $this->leasesSupported;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(LeasingSavings::class, 'Google_Service_Solar_LeasingSavings');
