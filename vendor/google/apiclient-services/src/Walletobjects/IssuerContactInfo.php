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

namespace Google\Service\Walletobjects;

class IssuerContactInfo extends \Google\Collection
{
  protected $collection_key = 'alertsEmails';
  /**
   * @var string[]
   */
  public $alertsEmails;
  /**
   * @var string
   */
  public $email;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $phone;

  /**
   * @param string[]
   */
  public function setAlertsEmails($alertsEmails)
  {
    $this->alertsEmails = $alertsEmails;
  }
  /**
   * @return string[]
   */
  public function getAlertsEmails()
  {
    return $this->alertsEmails;
  }
  /**
   * @param string
   */
  public function setEmail($email)
  {
    $this->email = $email;
  }
  /**
   * @return string
   */
  public function getEmail()
  {
    return $this->email;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setPhone($phone)
  {
    $this->phone = $phone;
  }
  /**
   * @return string
   */
  public function getPhone()
  {
    return $this->phone;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(IssuerContactInfo::class, 'Google_Service_Walletobjects_IssuerContactInfo');
