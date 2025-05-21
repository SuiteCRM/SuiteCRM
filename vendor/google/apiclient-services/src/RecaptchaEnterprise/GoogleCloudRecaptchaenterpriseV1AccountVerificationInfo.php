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

class GoogleCloudRecaptchaenterpriseV1AccountVerificationInfo extends \Google\Collection
{
  protected $collection_key = 'endpoints';
  protected $endpointsType = GoogleCloudRecaptchaenterpriseV1EndpointVerificationInfo::class;
  protected $endpointsDataType = 'array';
  /**
   * @var string
   */
  public $languageCode;
  /**
   * @var string
   */
  public $latestVerificationResult;
  /**
   * @var string
   */
  public $username;

  /**
   * @param GoogleCloudRecaptchaenterpriseV1EndpointVerificationInfo[]
   */
  public function setEndpoints($endpoints)
  {
    $this->endpoints = $endpoints;
  }
  /**
   * @return GoogleCloudRecaptchaenterpriseV1EndpointVerificationInfo[]
   */
  public function getEndpoints()
  {
    return $this->endpoints;
  }
  /**
   * @param string
   */
  public function setLanguageCode($languageCode)
  {
    $this->languageCode = $languageCode;
  }
  /**
   * @return string
   */
  public function getLanguageCode()
  {
    return $this->languageCode;
  }
  /**
   * @param string
   */
  public function setLatestVerificationResult($latestVerificationResult)
  {
    $this->latestVerificationResult = $latestVerificationResult;
  }
  /**
   * @return string
   */
  public function getLatestVerificationResult()
  {
    return $this->latestVerificationResult;
  }
  /**
   * @param string
   */
  public function setUsername($username)
  {
    $this->username = $username;
  }
  /**
   * @return string
   */
  public function getUsername()
  {
    return $this->username;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRecaptchaenterpriseV1AccountVerificationInfo::class, 'Google_Service_RecaptchaEnterprise_GoogleCloudRecaptchaenterpriseV1AccountVerificationInfo');
