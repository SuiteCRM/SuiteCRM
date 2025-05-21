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

namespace Google\Service\RecaptchaEnterprise;

class GoogleCloudRecaptchaenterpriseV1TransactionData extends \Google\Collection
{
  protected $collection_key = 'merchants';
  protected $billingAddressType = GoogleCloudRecaptchaenterpriseV1TransactionDataAddress::class;
  protected $billingAddressDataType = '';
  /**
   * @var string
   */
  public $cardBin;
  /**
   * @var string
   */
  public $cardLastFour;
  /**
   * @var string
   */
  public $currencyCode;
  protected $gatewayInfoType = GoogleCloudRecaptchaenterpriseV1TransactionDataGatewayInfo::class;
  protected $gatewayInfoDataType = '';
  protected $itemsType = GoogleCloudRecaptchaenterpriseV1TransactionDataItem::class;
  protected $itemsDataType = 'array';
  protected $merchantsType = GoogleCloudRecaptchaenterpriseV1TransactionDataUser::class;
  protected $merchantsDataType = 'array';
  /**
   * @var string
   */
  public $paymentMethod;
  protected $shippingAddressType = GoogleCloudRecaptchaenterpriseV1TransactionDataAddress::class;
  protected $shippingAddressDataType = '';
  public $shippingValue;
  /**
   * @var string
   */
  public $transactionId;
  protected $userType = GoogleCloudRecaptchaenterpriseV1TransactionDataUser::class;
  protected $userDataType = '';
  public $value;

  /**
   * @param GoogleCloudRecaptchaenterpriseV1TransactionDataAddress
   */
  public function setBillingAddress(GoogleCloudRecaptchaenterpriseV1TransactionDataAddress $billingAddress)
  {
    $this->billingAddress = $billingAddress;
  }
  /**
   * @return GoogleCloudRecaptchaenterpriseV1TransactionDataAddress
   */
  public function getBillingAddress()
  {
    return $this->billingAddress;
  }
  /**
   * @param string
   */
  public function setCardBin($cardBin)
  {
    $this->cardBin = $cardBin;
  }
  /**
   * @return string
   */
  public function getCardBin()
  {
    return $this->cardBin;
  }
  /**
   * @param string
   */
  public function setCardLastFour($cardLastFour)
  {
    $this->cardLastFour = $cardLastFour;
  }
  /**
   * @return string
   */
  public function getCardLastFour()
  {
    return $this->cardLastFour;
  }
  /**
   * @param string
   */
  public function setCurrencyCode($currencyCode)
  {
    $this->currencyCode = $currencyCode;
  }
  /**
   * @return string
   */
  public function getCurrencyCode()
  {
    return $this->currencyCode;
  }
  /**
   * @param GoogleCloudRecaptchaenterpriseV1TransactionDataGatewayInfo
   */
  public function setGatewayInfo(GoogleCloudRecaptchaenterpriseV1TransactionDataGatewayInfo $gatewayInfo)
  {
    $this->gatewayInfo = $gatewayInfo;
  }
  /**
   * @return GoogleCloudRecaptchaenterpriseV1TransactionDataGatewayInfo
   */
  public function getGatewayInfo()
  {
    return $this->gatewayInfo;
  }
  /**
   * @param GoogleCloudRecaptchaenterpriseV1TransactionDataItem[]
   */
  public function setItems($items)
  {
    $this->items = $items;
  }
  /**
   * @return GoogleCloudRecaptchaenterpriseV1TransactionDataItem[]
   */
  public function getItems()
  {
    return $this->items;
  }
  /**
   * @param GoogleCloudRecaptchaenterpriseV1TransactionDataUser[]
   */
  public function setMerchants($merchants)
  {
    $this->merchants = $merchants;
  }
  /**
   * @return GoogleCloudRecaptchaenterpriseV1TransactionDataUser[]
   */
  public function getMerchants()
  {
    return $this->merchants;
  }
  /**
   * @param string
   */
  public function setPaymentMethod($paymentMethod)
  {
    $this->paymentMethod = $paymentMethod;
  }
  /**
   * @return string
   */
  public function getPaymentMethod()
  {
    return $this->paymentMethod;
  }
  /**
   * @param GoogleCloudRecaptchaenterpriseV1TransactionDataAddress
   */
  public function setShippingAddress(GoogleCloudRecaptchaenterpriseV1TransactionDataAddress $shippingAddress)
  {
    $this->shippingAddress = $shippingAddress;
  }
  /**
   * @return GoogleCloudRecaptchaenterpriseV1TransactionDataAddress
   */
  public function getShippingAddress()
  {
    return $this->shippingAddress;
  }
  public function setShippingValue($shippingValue)
  {
    $this->shippingValue = $shippingValue;
  }
  public function getShippingValue()
  {
    return $this->shippingValue;
  }
  /**
   * @param string
   */
  public function setTransactionId($transactionId)
  {
    $this->transactionId = $transactionId;
  }
  /**
   * @return string
   */
  public function getTransactionId()
  {
    return $this->transactionId;
  }
  /**
   * @param GoogleCloudRecaptchaenterpriseV1TransactionDataUser
   */
  public function setUser(GoogleCloudRecaptchaenterpriseV1TransactionDataUser $user)
  {
    $this->user = $user;
  }
  /**
   * @return GoogleCloudRecaptchaenterpriseV1TransactionDataUser
   */
  public function getUser()
  {
    return $this->user;
  }
  public function setValue($value)
  {
    $this->value = $value;
  }
  public function getValue()
  {
    return $this->value;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRecaptchaenterpriseV1TransactionData::class, 'Google_Service_RecaptchaEnterprise_GoogleCloudRecaptchaenterpriseV1TransactionData');
