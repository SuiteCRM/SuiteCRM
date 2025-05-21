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

class GoogleCloudRecaptchaenterpriseV1TransactionDataUser extends \Google\Model
{
  /**
   * @var string
   */
  public $accountId;
  /**
   * @var string
   */
  public $creationMs;
  /**
   * @var string
   */
  public $email;
  /**
   * @var bool
   */
  public $emailVerified;
  /**
   * @var string
   */
  public $phoneNumber;
  /**
   * @var bool
   */
  public $phoneVerified;

  /**
   * @param string
   */
  public function setAccountId($accountId)
  {
    $this->accountId = $accountId;
  }
  /**
   * @return string
   */
  public function getAccountId()
  {
    return $this->accountId;
  }
  /**
   * @param string
   */
  public function setCreationMs($creationMs)
  {
    $this->creationMs = $creationMs;
  }
  /**
   * @return string
   */
  public function getCreationMs()
  {
    return $this->creationMs;
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
   * @param bool
   */
  public function setEmailVerified($emailVerified)
  {
    $this->emailVerified = $emailVerified;
  }
  /**
   * @return bool
   */
  public function getEmailVerified()
  {
    return $this->emailVerified;
  }
  /**
   * @param string
   */
  public function setPhoneNumber($phoneNumber)
  {
    $this->phoneNumber = $phoneNumber;
  }
  /**
   * @return string
   */
  public function getPhoneNumber()
  {
    return $this->phoneNumber;
  }
  /**
   * @param bool
   */
  public function setPhoneVerified($phoneVerified)
  {
    $this->phoneVerified = $phoneVerified;
  }
  /**
   * @return bool
   */
  public function getPhoneVerified()
  {
    return $this->phoneVerified;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRecaptchaenterpriseV1TransactionDataUser::class, 'Google_Service_RecaptchaEnterprise_GoogleCloudRecaptchaenterpriseV1TransactionDataUser');
