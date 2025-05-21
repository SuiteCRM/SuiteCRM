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

namespace Google\Service\SA360;

class GoogleAdsSearchads360V0ResourcesCustomerClient extends \Google\Collection
{
  protected $collection_key = 'appliedLabels';
  /**
   * @var string[]
   */
  public $appliedLabels;
  /**
   * @var string
   */
  public $clientCustomer;
  /**
   * @var string
   */
  public $currencyCode;
  /**
   * @var string
   */
  public $descriptiveName;
  /**
   * @var bool
   */
  public $hidden;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $level;
  /**
   * @var bool
   */
  public $manager;
  /**
   * @var string
   */
  public $resourceName;
  /**
   * @var string
   */
  public $status;
  /**
   * @var bool
   */
  public $testAccount;
  /**
   * @var string
   */
  public $timeZone;

  /**
   * @param string[]
   */
  public function setAppliedLabels($appliedLabels)
  {
    $this->appliedLabels = $appliedLabels;
  }
  /**
   * @return string[]
   */
  public function getAppliedLabels()
  {
    return $this->appliedLabels;
  }
  /**
   * @param string
   */
  public function setClientCustomer($clientCustomer)
  {
    $this->clientCustomer = $clientCustomer;
  }
  /**
   * @return string
   */
  public function getClientCustomer()
  {
    return $this->clientCustomer;
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
   * @param string
   */
  public function setDescriptiveName($descriptiveName)
  {
    $this->descriptiveName = $descriptiveName;
  }
  /**
   * @return string
   */
  public function getDescriptiveName()
  {
    return $this->descriptiveName;
  }
  /**
   * @param bool
   */
  public function setHidden($hidden)
  {
    $this->hidden = $hidden;
  }
  /**
   * @return bool
   */
  public function getHidden()
  {
    return $this->hidden;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
  }
  /**
   * @param string
   */
  public function setLevel($level)
  {
    $this->level = $level;
  }
  /**
   * @return string
   */
  public function getLevel()
  {
    return $this->level;
  }
  /**
   * @param bool
   */
  public function setManager($manager)
  {
    $this->manager = $manager;
  }
  /**
   * @return bool
   */
  public function getManager()
  {
    return $this->manager;
  }
  /**
   * @param string
   */
  public function setResourceName($resourceName)
  {
    $this->resourceName = $resourceName;
  }
  /**
   * @return string
   */
  public function getResourceName()
  {
    return $this->resourceName;
  }
  /**
   * @param string
   */
  public function setStatus($status)
  {
    $this->status = $status;
  }
  /**
   * @return string
   */
  public function getStatus()
  {
    return $this->status;
  }
  /**
   * @param bool
   */
  public function setTestAccount($testAccount)
  {
    $this->testAccount = $testAccount;
  }
  /**
   * @return bool
   */
  public function getTestAccount()
  {
    return $this->testAccount;
  }
  /**
   * @param string
   */
  public function setTimeZone($timeZone)
  {
    $this->timeZone = $timeZone;
  }
  /**
   * @return string
   */
  public function getTimeZone()
  {
    return $this->timeZone;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ResourcesCustomerClient::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ResourcesCustomerClient');
