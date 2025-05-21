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

class ExternalTransaction extends \Google\Model
{
  /**
   * @var string
   */
  public $createTime;
  protected $currentPreTaxAmountType = Price::class;
  protected $currentPreTaxAmountDataType = '';
  protected $currentTaxAmountType = Price::class;
  protected $currentTaxAmountDataType = '';
  /**
   * @var string
   */
  public $externalTransactionId;
  protected $oneTimeTransactionType = OneTimeExternalTransaction::class;
  protected $oneTimeTransactionDataType = '';
  protected $originalPreTaxAmountType = Price::class;
  protected $originalPreTaxAmountDataType = '';
  protected $originalTaxAmountType = Price::class;
  protected $originalTaxAmountDataType = '';
  /**
   * @var string
   */
  public $packageName;
  protected $recurringTransactionType = RecurringExternalTransaction::class;
  protected $recurringTransactionDataType = '';
  protected $testPurchaseType = ExternalTransactionTestPurchase::class;
  protected $testPurchaseDataType = '';
  /**
   * @var int
   */
  public $transactionProgramCode;
  /**
   * @var string
   */
  public $transactionState;
  /**
   * @var string
   */
  public $transactionTime;
  protected $userTaxAddressType = ExternalTransactionAddress::class;
  protected $userTaxAddressDataType = '';

  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param Price
   */
  public function setCurrentPreTaxAmount(Price $currentPreTaxAmount)
  {
    $this->currentPreTaxAmount = $currentPreTaxAmount;
  }
  /**
   * @return Price
   */
  public function getCurrentPreTaxAmount()
  {
    return $this->currentPreTaxAmount;
  }
  /**
   * @param Price
   */
  public function setCurrentTaxAmount(Price $currentTaxAmount)
  {
    $this->currentTaxAmount = $currentTaxAmount;
  }
  /**
   * @return Price
   */
  public function getCurrentTaxAmount()
  {
    return $this->currentTaxAmount;
  }
  /**
   * @param string
   */
  public function setExternalTransactionId($externalTransactionId)
  {
    $this->externalTransactionId = $externalTransactionId;
  }
  /**
   * @return string
   */
  public function getExternalTransactionId()
  {
    return $this->externalTransactionId;
  }
  /**
   * @param OneTimeExternalTransaction
   */
  public function setOneTimeTransaction(OneTimeExternalTransaction $oneTimeTransaction)
  {
    $this->oneTimeTransaction = $oneTimeTransaction;
  }
  /**
   * @return OneTimeExternalTransaction
   */
  public function getOneTimeTransaction()
  {
    return $this->oneTimeTransaction;
  }
  /**
   * @param Price
   */
  public function setOriginalPreTaxAmount(Price $originalPreTaxAmount)
  {
    $this->originalPreTaxAmount = $originalPreTaxAmount;
  }
  /**
   * @return Price
   */
  public function getOriginalPreTaxAmount()
  {
    return $this->originalPreTaxAmount;
  }
  /**
   * @param Price
   */
  public function setOriginalTaxAmount(Price $originalTaxAmount)
  {
    $this->originalTaxAmount = $originalTaxAmount;
  }
  /**
   * @return Price
   */
  public function getOriginalTaxAmount()
  {
    return $this->originalTaxAmount;
  }
  /**
   * @param string
   */
  public function setPackageName($packageName)
  {
    $this->packageName = $packageName;
  }
  /**
   * @return string
   */
  public function getPackageName()
  {
    return $this->packageName;
  }
  /**
   * @param RecurringExternalTransaction
   */
  public function setRecurringTransaction(RecurringExternalTransaction $recurringTransaction)
  {
    $this->recurringTransaction = $recurringTransaction;
  }
  /**
   * @return RecurringExternalTransaction
   */
  public function getRecurringTransaction()
  {
    return $this->recurringTransaction;
  }
  /**
   * @param ExternalTransactionTestPurchase
   */
  public function setTestPurchase(ExternalTransactionTestPurchase $testPurchase)
  {
    $this->testPurchase = $testPurchase;
  }
  /**
   * @return ExternalTransactionTestPurchase
   */
  public function getTestPurchase()
  {
    return $this->testPurchase;
  }
  /**
   * @param int
   */
  public function setTransactionProgramCode($transactionProgramCode)
  {
    $this->transactionProgramCode = $transactionProgramCode;
  }
  /**
   * @return int
   */
  public function getTransactionProgramCode()
  {
    return $this->transactionProgramCode;
  }
  /**
   * @param string
   */
  public function setTransactionState($transactionState)
  {
    $this->transactionState = $transactionState;
  }
  /**
   * @return string
   */
  public function getTransactionState()
  {
    return $this->transactionState;
  }
  /**
   * @param string
   */
  public function setTransactionTime($transactionTime)
  {
    $this->transactionTime = $transactionTime;
  }
  /**
   * @return string
   */
  public function getTransactionTime()
  {
    return $this->transactionTime;
  }
  /**
   * @param ExternalTransactionAddress
   */
  public function setUserTaxAddress(ExternalTransactionAddress $userTaxAddress)
  {
    $this->userTaxAddress = $userTaxAddress;
  }
  /**
   * @return ExternalTransactionAddress
   */
  public function getUserTaxAddress()
  {
    return $this->userTaxAddress;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ExternalTransaction::class, 'Google_Service_AndroidPublisher_ExternalTransaction');
