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

class FinancialDetails extends \Google\Model
{
  protected $costOfElectricityWithoutSolarType = Money::class;
  protected $costOfElectricityWithoutSolarDataType = '';
  protected $federalIncentiveType = Money::class;
  protected $federalIncentiveDataType = '';
  /**
   * @var float
   */
  public $initialAcKwhPerYear;
  protected $lifetimeSrecTotalType = Money::class;
  protected $lifetimeSrecTotalDataType = '';
  /**
   * @var bool
   */
  public $netMeteringAllowed;
  /**
   * @var float
   */
  public $percentageExportedToGrid;
  protected $remainingLifetimeUtilityBillType = Money::class;
  protected $remainingLifetimeUtilityBillDataType = '';
  /**
   * @var float
   */
  public $solarPercentage;
  protected $stateIncentiveType = Money::class;
  protected $stateIncentiveDataType = '';
  protected $utilityIncentiveType = Money::class;
  protected $utilityIncentiveDataType = '';

  /**
   * @param Money
   */
  public function setCostOfElectricityWithoutSolar(Money $costOfElectricityWithoutSolar)
  {
    $this->costOfElectricityWithoutSolar = $costOfElectricityWithoutSolar;
  }
  /**
   * @return Money
   */
  public function getCostOfElectricityWithoutSolar()
  {
    return $this->costOfElectricityWithoutSolar;
  }
  /**
   * @param Money
   */
  public function setFederalIncentive(Money $federalIncentive)
  {
    $this->federalIncentive = $federalIncentive;
  }
  /**
   * @return Money
   */
  public function getFederalIncentive()
  {
    return $this->federalIncentive;
  }
  /**
   * @param float
   */
  public function setInitialAcKwhPerYear($initialAcKwhPerYear)
  {
    $this->initialAcKwhPerYear = $initialAcKwhPerYear;
  }
  /**
   * @return float
   */
  public function getInitialAcKwhPerYear()
  {
    return $this->initialAcKwhPerYear;
  }
  /**
   * @param Money
   */
  public function setLifetimeSrecTotal(Money $lifetimeSrecTotal)
  {
    $this->lifetimeSrecTotal = $lifetimeSrecTotal;
  }
  /**
   * @return Money
   */
  public function getLifetimeSrecTotal()
  {
    return $this->lifetimeSrecTotal;
  }
  /**
   * @param bool
   */
  public function setNetMeteringAllowed($netMeteringAllowed)
  {
    $this->netMeteringAllowed = $netMeteringAllowed;
  }
  /**
   * @return bool
   */
  public function getNetMeteringAllowed()
  {
    return $this->netMeteringAllowed;
  }
  /**
   * @param float
   */
  public function setPercentageExportedToGrid($percentageExportedToGrid)
  {
    $this->percentageExportedToGrid = $percentageExportedToGrid;
  }
  /**
   * @return float
   */
  public function getPercentageExportedToGrid()
  {
    return $this->percentageExportedToGrid;
  }
  /**
   * @param Money
   */
  public function setRemainingLifetimeUtilityBill(Money $remainingLifetimeUtilityBill)
  {
    $this->remainingLifetimeUtilityBill = $remainingLifetimeUtilityBill;
  }
  /**
   * @return Money
   */
  public function getRemainingLifetimeUtilityBill()
  {
    return $this->remainingLifetimeUtilityBill;
  }
  /**
   * @param float
   */
  public function setSolarPercentage($solarPercentage)
  {
    $this->solarPercentage = $solarPercentage;
  }
  /**
   * @return float
   */
  public function getSolarPercentage()
  {
    return $this->solarPercentage;
  }
  /**
   * @param Money
   */
  public function setStateIncentive(Money $stateIncentive)
  {
    $this->stateIncentive = $stateIncentive;
  }
  /**
   * @return Money
   */
  public function getStateIncentive()
  {
    return $this->stateIncentive;
  }
  /**
   * @param Money
   */
  public function setUtilityIncentive(Money $utilityIncentive)
  {
    $this->utilityIncentive = $utilityIncentive;
  }
  /**
   * @return Money
   */
  public function getUtilityIncentive()
  {
    return $this->utilityIncentive;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(FinancialDetails::class, 'Google_Service_Solar_FinancialDetails');
