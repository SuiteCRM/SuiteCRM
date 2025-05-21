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

class PartialRefund extends \Google\Model
{
  /**
   * @var string
   */
  public $refundId;
  protected $refundPreTaxAmountType = Price::class;
  protected $refundPreTaxAmountDataType = '';

  /**
   * @param string
   */
  public function setRefundId($refundId)
  {
    $this->refundId = $refundId;
  }
  /**
   * @return string
   */
  public function getRefundId()
  {
    return $this->refundId;
  }
  /**
   * @param Price
   */
  public function setRefundPreTaxAmount(Price $refundPreTaxAmount)
  {
    $this->refundPreTaxAmount = $refundPreTaxAmount;
  }
  /**
   * @return Price
   */
  public function getRefundPreTaxAmount()
  {
    return $this->refundPreTaxAmount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PartialRefund::class, 'Google_Service_AndroidPublisher_PartialRefund');
