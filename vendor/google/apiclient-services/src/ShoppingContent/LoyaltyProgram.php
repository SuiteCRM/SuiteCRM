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

class LoyaltyProgram extends \Google\Model
{
  protected $cashbackForFutureUseType = Price::class;
  protected $cashbackForFutureUseDataType = '';
  /**
   * @var string
   */
  public $loyaltyPoints;
  /**
   * @var string
   */
  public $memberPriceEffectiveDate;
  protected $priceType = Price::class;
  protected $priceDataType = '';
  /**
   * @var string
   */
  public $programLabel;
  /**
   * @var string
   */
  public $shippingLabel;
  /**
   * @var string
   */
  public $tierLabel;

  /**
   * @param Price
   */
  public function setCashbackForFutureUse(Price $cashbackForFutureUse)
  {
    $this->cashbackForFutureUse = $cashbackForFutureUse;
  }
  /**
   * @return Price
   */
  public function getCashbackForFutureUse()
  {
    return $this->cashbackForFutureUse;
  }
  /**
   * @param string
   */
  public function setLoyaltyPoints($loyaltyPoints)
  {
    $this->loyaltyPoints = $loyaltyPoints;
  }
  /**
   * @return string
   */
  public function getLoyaltyPoints()
  {
    return $this->loyaltyPoints;
  }
  /**
   * @param string
   */
  public function setMemberPriceEffectiveDate($memberPriceEffectiveDate)
  {
    $this->memberPriceEffectiveDate = $memberPriceEffectiveDate;
  }
  /**
   * @return string
   */
  public function getMemberPriceEffectiveDate()
  {
    return $this->memberPriceEffectiveDate;
  }
  /**
   * @param Price
   */
  public function setPrice(Price $price)
  {
    $this->price = $price;
  }
  /**
   * @return Price
   */
  public function getPrice()
  {
    return $this->price;
  }
  /**
   * @param string
   */
  public function setProgramLabel($programLabel)
  {
    $this->programLabel = $programLabel;
  }
  /**
   * @return string
   */
  public function getProgramLabel()
  {
    return $this->programLabel;
  }
  /**
   * @param string
   */
  public function setShippingLabel($shippingLabel)
  {
    $this->shippingLabel = $shippingLabel;
  }
  /**
   * @return string
   */
  public function getShippingLabel()
  {
    return $this->shippingLabel;
  }
  /**
   * @param string
   */
  public function setTierLabel($tierLabel)
  {
    $this->tierLabel = $tierLabel;
  }
  /**
   * @return string
   */
  public function getTierLabel()
  {
    return $this->tierLabel;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(LoyaltyProgram::class, 'Google_Service_ShoppingContent_LoyaltyProgram');
