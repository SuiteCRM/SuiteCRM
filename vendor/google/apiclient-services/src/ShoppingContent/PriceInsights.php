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

namespace Google\Service\ShoppingContent;

class PriceInsights extends \Google\Model
{
  /**
   * @var string
   */
  public $effectiveness;
  public $predictedClicksChangeFraction;
  public $predictedConversionsChangeFraction;
  public $predictedGrossProfitChangeFraction;
  public $predictedImpressionsChangeFraction;
  /**
   * @var string
   */
  public $predictedMonthlyGrossProfitChangeCurrencyCode;
  /**
   * @var string
   */
  public $predictedMonthlyGrossProfitChangeMicros;
  /**
   * @var string
   */
  public $suggestedPriceCurrencyCode;
  /**
   * @var string
   */
  public $suggestedPriceMicros;

  /**
   * @param string
   */
  public function setEffectiveness($effectiveness)
  {
    $this->effectiveness = $effectiveness;
  }
  /**
   * @return string
   */
  public function getEffectiveness()
  {
    return $this->effectiveness;
  }
  public function setPredictedClicksChangeFraction($predictedClicksChangeFraction)
  {
    $this->predictedClicksChangeFraction = $predictedClicksChangeFraction;
  }
  public function getPredictedClicksChangeFraction()
  {
    return $this->predictedClicksChangeFraction;
  }
  public function setPredictedConversionsChangeFraction($predictedConversionsChangeFraction)
  {
    $this->predictedConversionsChangeFraction = $predictedConversionsChangeFraction;
  }
  public function getPredictedConversionsChangeFraction()
  {
    return $this->predictedConversionsChangeFraction;
  }
  public function setPredictedGrossProfitChangeFraction($predictedGrossProfitChangeFraction)
  {
    $this->predictedGrossProfitChangeFraction = $predictedGrossProfitChangeFraction;
  }
  public function getPredictedGrossProfitChangeFraction()
  {
    return $this->predictedGrossProfitChangeFraction;
  }
  public function setPredictedImpressionsChangeFraction($predictedImpressionsChangeFraction)
  {
    $this->predictedImpressionsChangeFraction = $predictedImpressionsChangeFraction;
  }
  public function getPredictedImpressionsChangeFraction()
  {
    return $this->predictedImpressionsChangeFraction;
  }
  /**
   * @param string
   */
  public function setPredictedMonthlyGrossProfitChangeCurrencyCode($predictedMonthlyGrossProfitChangeCurrencyCode)
  {
    $this->predictedMonthlyGrossProfitChangeCurrencyCode = $predictedMonthlyGrossProfitChangeCurrencyCode;
  }
  /**
   * @return string
   */
  public function getPredictedMonthlyGrossProfitChangeCurrencyCode()
  {
    return $this->predictedMonthlyGrossProfitChangeCurrencyCode;
  }
  /**
   * @param string
   */
  public function setPredictedMonthlyGrossProfitChangeMicros($predictedMonthlyGrossProfitChangeMicros)
  {
    $this->predictedMonthlyGrossProfitChangeMicros = $predictedMonthlyGrossProfitChangeMicros;
  }
  /**
   * @return string
   */
  public function getPredictedMonthlyGrossProfitChangeMicros()
  {
    return $this->predictedMonthlyGrossProfitChangeMicros;
  }
  /**
   * @param string
   */
  public function setSuggestedPriceCurrencyCode($suggestedPriceCurrencyCode)
  {
    $this->suggestedPriceCurrencyCode = $suggestedPriceCurrencyCode;
  }
  /**
   * @return string
   */
  public function getSuggestedPriceCurrencyCode()
  {
    return $this->suggestedPriceCurrencyCode;
  }
  /**
   * @param string
   */
  public function setSuggestedPriceMicros($suggestedPriceMicros)
  {
    $this->suggestedPriceMicros = $suggestedPriceMicros;
  }
  /**
   * @return string
   */
  public function getSuggestedPriceMicros()
  {
    return $this->suggestedPriceMicros;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PriceInsights::class, 'Google_Service_ShoppingContent_PriceInsights');
