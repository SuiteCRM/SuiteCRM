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

class FinancialAnalysis extends \Google\Model
{
  /**
   * @var float
   */
  public $averageKwhPerMonth;
  protected $cashPurchaseSavingsType = CashPurchaseSavings::class;
  protected $cashPurchaseSavingsDataType = '';
  /**
   * @var bool
   */
  public $defaultBill;
  protected $financedPurchaseSavingsType = FinancedPurchaseSavings::class;
  protected $financedPurchaseSavingsDataType = '';
  protected $financialDetailsType = FinancialDetails::class;
  protected $financialDetailsDataType = '';
  protected $leasingSavingsType = LeasingSavings::class;
  protected $leasingSavingsDataType = '';
  protected $monthlyBillType = Money::class;
  protected $monthlyBillDataType = '';
  /**
   * @var int
   */
  public $panelConfigIndex;

  /**
   * @param float
   */
  public function setAverageKwhPerMonth($averageKwhPerMonth)
  {
    $this->averageKwhPerMonth = $averageKwhPerMonth;
  }
  /**
   * @return float
   */
  public function getAverageKwhPerMonth()
  {
    return $this->averageKwhPerMonth;
  }
  /**
   * @param CashPurchaseSavings
   */
  public function setCashPurchaseSavings(CashPurchaseSavings $cashPurchaseSavings)
  {
    $this->cashPurchaseSavings = $cashPurchaseSavings;
  }
  /**
   * @return CashPurchaseSavings
   */
  public function getCashPurchaseSavings()
  {
    return $this->cashPurchaseSavings;
  }
  /**
   * @param bool
   */
  public function setDefaultBill($defaultBill)
  {
    $this->defaultBill = $defaultBill;
  }
  /**
   * @return bool
   */
  public function getDefaultBill()
  {
    return $this->defaultBill;
  }
  /**
   * @param FinancedPurchaseSavings
   */
  public function setFinancedPurchaseSavings(FinancedPurchaseSavings $financedPurchaseSavings)
  {
    $this->financedPurchaseSavings = $financedPurchaseSavings;
  }
  /**
   * @return FinancedPurchaseSavings
   */
  public function getFinancedPurchaseSavings()
  {
    return $this->financedPurchaseSavings;
  }
  /**
   * @param FinancialDetails
   */
  public function setFinancialDetails(FinancialDetails $financialDetails)
  {
    $this->financialDetails = $financialDetails;
  }
  /**
   * @return FinancialDetails
   */
  public function getFinancialDetails()
  {
    return $this->financialDetails;
  }
  /**
   * @param LeasingSavings
   */
  public function setLeasingSavings(LeasingSavings $leasingSavings)
  {
    $this->leasingSavings = $leasingSavings;
  }
  /**
   * @return LeasingSavings
   */
  public function getLeasingSavings()
  {
    return $this->leasingSavings;
  }
  /**
   * @param Money
   */
  public function setMonthlyBill(Money $monthlyBill)
  {
    $this->monthlyBill = $monthlyBill;
  }
  /**
   * @return Money
   */
  public function getMonthlyBill()
  {
    return $this->monthlyBill;
  }
  /**
   * @param int
   */
  public function setPanelConfigIndex($panelConfigIndex)
  {
    $this->panelConfigIndex = $panelConfigIndex;
  }
  /**
   * @return int
   */
  public function getPanelConfigIndex()
  {
    return $this->panelConfigIndex;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(FinancialAnalysis::class, 'Google_Service_Solar_FinancialAnalysis');
