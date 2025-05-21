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

class GoogleCloudRecaptchaenterpriseV1Event extends \Google\Collection
{
  protected $collection_key = 'headers';
  /**
   * @var string
   */
  public $expectedAction;
  /**
   * @var bool
   */
  public $express;
  /**
   * @var bool
   */
  public $firewallPolicyEvaluation;
  /**
   * @var string
   */
  public $fraudPrevention;
  /**
   * @var string
   */
  public $hashedAccountId;
  /**
   * @var string[]
   */
  public $headers;
  /**
   * @var string
   */
  public $ja3;
  /**
   * @var string
   */
  public $ja4;
  /**
   * @var string
   */
  public $requestedUri;
  /**
   * @var string
   */
  public $siteKey;
  /**
   * @var string
   */
  public $token;
  protected $transactionDataType = GoogleCloudRecaptchaenterpriseV1TransactionData::class;
  protected $transactionDataDataType = '';
  /**
   * @var string
   */
  public $userAgent;
  protected $userInfoType = GoogleCloudRecaptchaenterpriseV1UserInfo::class;
  protected $userInfoDataType = '';
  /**
   * @var string
   */
  public $userIpAddress;
  /**
   * @var bool
   */
  public $wafTokenAssessment;

  /**
   * @param string
   */
  public function setExpectedAction($expectedAction)
  {
    $this->expectedAction = $expectedAction;
  }
  /**
   * @return string
   */
  public function getExpectedAction()
  {
    return $this->expectedAction;
  }
  /**
   * @param bool
   */
  public function setExpress($express)
  {
    $this->express = $express;
  }
  /**
   * @return bool
   */
  public function getExpress()
  {
    return $this->express;
  }
  /**
   * @param bool
   */
  public function setFirewallPolicyEvaluation($firewallPolicyEvaluation)
  {
    $this->firewallPolicyEvaluation = $firewallPolicyEvaluation;
  }
  /**
   * @return bool
   */
  public function getFirewallPolicyEvaluation()
  {
    return $this->firewallPolicyEvaluation;
  }
  /**
   * @param string
   */
  public function setFraudPrevention($fraudPrevention)
  {
    $this->fraudPrevention = $fraudPrevention;
  }
  /**
   * @return string
   */
  public function getFraudPrevention()
  {
    return $this->fraudPrevention;
  }
  /**
   * @param string
   */
  public function setHashedAccountId($hashedAccountId)
  {
    $this->hashedAccountId = $hashedAccountId;
  }
  /**
   * @return string
   */
  public function getHashedAccountId()
  {
    return $this->hashedAccountId;
  }
  /**
   * @param string[]
   */
  public function setHeaders($headers)
  {
    $this->headers = $headers;
  }
  /**
   * @return string[]
   */
  public function getHeaders()
  {
    return $this->headers;
  }
  /**
   * @param string
   */
  public function setJa3($ja3)
  {
    $this->ja3 = $ja3;
  }
  /**
   * @return string
   */
  public function getJa3()
  {
    return $this->ja3;
  }
  /**
   * @param string
   */
  public function setJa4($ja4)
  {
    $this->ja4 = $ja4;
  }
  /**
   * @return string
   */
  public function getJa4()
  {
    return $this->ja4;
  }
  /**
   * @param string
   */
  public function setRequestedUri($requestedUri)
  {
    $this->requestedUri = $requestedUri;
  }
  /**
   * @return string
   */
  public function getRequestedUri()
  {
    return $this->requestedUri;
  }
  /**
   * @param string
   */
  public function setSiteKey($siteKey)
  {
    $this->siteKey = $siteKey;
  }
  /**
   * @return string
   */
  public function getSiteKey()
  {
    return $this->siteKey;
  }
  /**
   * @param string
   */
  public function setToken($token)
  {
    $this->token = $token;
  }
  /**
   * @return string
   */
  public function getToken()
  {
    return $this->token;
  }
  /**
   * @param GoogleCloudRecaptchaenterpriseV1TransactionData
   */
  public function setTransactionData(GoogleCloudRecaptchaenterpriseV1TransactionData $transactionData)
  {
    $this->transactionData = $transactionData;
  }
  /**
   * @return GoogleCloudRecaptchaenterpriseV1TransactionData
   */
  public function getTransactionData()
  {
    return $this->transactionData;
  }
  /**
   * @param string
   */
  public function setUserAgent($userAgent)
  {
    $this->userAgent = $userAgent;
  }
  /**
   * @return string
   */
  public function getUserAgent()
  {
    return $this->userAgent;
  }
  /**
   * @param GoogleCloudRecaptchaenterpriseV1UserInfo
   */
  public function setUserInfo(GoogleCloudRecaptchaenterpriseV1UserInfo $userInfo)
  {
    $this->userInfo = $userInfo;
  }
  /**
   * @return GoogleCloudRecaptchaenterpriseV1UserInfo
   */
  public function getUserInfo()
  {
    return $this->userInfo;
  }
  /**
   * @param string
   */
  public function setUserIpAddress($userIpAddress)
  {
    $this->userIpAddress = $userIpAddress;
  }
  /**
   * @return string
   */
  public function getUserIpAddress()
  {
    return $this->userIpAddress;
  }
  /**
   * @param bool
   */
  public function setWafTokenAssessment($wafTokenAssessment)
  {
    $this->wafTokenAssessment = $wafTokenAssessment;
  }
  /**
   * @return bool
   */
  public function getWafTokenAssessment()
  {
    return $this->wafTokenAssessment;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRecaptchaenterpriseV1Event::class, 'Google_Service_RecaptchaEnterprise_GoogleCloudRecaptchaenterpriseV1Event');
