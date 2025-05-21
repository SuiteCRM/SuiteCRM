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

class GoogleCloudRecaptchaenterpriseV1EndpointVerificationInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $emailAddress;
  /**
   * @var string
   */
  public $lastVerificationTime;
  /**
   * @var string
   */
  public $phoneNumber;
  /**
   * @var string
   */
  public $requestToken;

  /**
   * @param string
   */
  public function setEmailAddress($emailAddress)
  {
    $this->emailAddress = $emailAddress;
  }
  /**
   * @return string
   */
  public function getEmailAddress()
  {
    return $this->emailAddress;
  }
  /**
   * @param string
   */
  public function setLastVerificationTime($lastVerificationTime)
  {
    $this->lastVerificationTime = $lastVerificationTime;
  }
  /**
   * @return string
   */
  public function getLastVerificationTime()
  {
    return $this->lastVerificationTime;
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
   * @param string
   */
  public function setRequestToken($requestToken)
  {
    $this->requestToken = $requestToken;
  }
  /**
   * @return string
   */
  public function getRequestToken()
  {
    return $this->requestToken;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRecaptchaenterpriseV1EndpointVerificationInfo::class, 'Google_Service_RecaptchaEnterprise_GoogleCloudRecaptchaenterpriseV1EndpointVerificationInfo');
