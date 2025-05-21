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

namespace Google\Service\Apigee;

class GoogleCloudApigeeV1SecurityActionConditionConfig extends \Google\Collection
{
  protected $collection_key = 'userAgents';
  /**
   * @var string[]
   */
  public $accessTokens;
  /**
   * @var string[]
   */
  public $apiKeys;
  /**
   * @var string[]
   */
  public $apiProducts;
  /**
   * @var string[]
   */
  public $asns;
  /**
   * @var string[]
   */
  public $botReasons;
  /**
   * @var string[]
   */
  public $developerApps;
  /**
   * @var string[]
   */
  public $developers;
  /**
   * @var string[]
   */
  public $httpMethods;
  /**
   * @var string[]
   */
  public $ipAddressRanges;
  /**
   * @var string[]
   */
  public $regionCodes;
  /**
   * @var string[]
   */
  public $userAgents;

  /**
   * @param string[]
   */
  public function setAccessTokens($accessTokens)
  {
    $this->accessTokens = $accessTokens;
  }
  /**
   * @return string[]
   */
  public function getAccessTokens()
  {
    return $this->accessTokens;
  }
  /**
   * @param string[]
   */
  public function setApiKeys($apiKeys)
  {
    $this->apiKeys = $apiKeys;
  }
  /**
   * @return string[]
   */
  public function getApiKeys()
  {
    return $this->apiKeys;
  }
  /**
   * @param string[]
   */
  public function setApiProducts($apiProducts)
  {
    $this->apiProducts = $apiProducts;
  }
  /**
   * @return string[]
   */
  public function getApiProducts()
  {
    return $this->apiProducts;
  }
  /**
   * @param string[]
   */
  public function setAsns($asns)
  {
    $this->asns = $asns;
  }
  /**
   * @return string[]
   */
  public function getAsns()
  {
    return $this->asns;
  }
  /**
   * @param string[]
   */
  public function setBotReasons($botReasons)
  {
    $this->botReasons = $botReasons;
  }
  /**
   * @return string[]
   */
  public function getBotReasons()
  {
    return $this->botReasons;
  }
  /**
   * @param string[]
   */
  public function setDeveloperApps($developerApps)
  {
    $this->developerApps = $developerApps;
  }
  /**
   * @return string[]
   */
  public function getDeveloperApps()
  {
    return $this->developerApps;
  }
  /**
   * @param string[]
   */
  public function setDevelopers($developers)
  {
    $this->developers = $developers;
  }
  /**
   * @return string[]
   */
  public function getDevelopers()
  {
    return $this->developers;
  }
  /**
   * @param string[]
   */
  public function setHttpMethods($httpMethods)
  {
    $this->httpMethods = $httpMethods;
  }
  /**
   * @return string[]
   */
  public function getHttpMethods()
  {
    return $this->httpMethods;
  }
  /**
   * @param string[]
   */
  public function setIpAddressRanges($ipAddressRanges)
  {
    $this->ipAddressRanges = $ipAddressRanges;
  }
  /**
   * @return string[]
   */
  public function getIpAddressRanges()
  {
    return $this->ipAddressRanges;
  }
  /**
   * @param string[]
   */
  public function setRegionCodes($regionCodes)
  {
    $this->regionCodes = $regionCodes;
  }
  /**
   * @return string[]
   */
  public function getRegionCodes()
  {
    return $this->regionCodes;
  }
  /**
   * @param string[]
   */
  public function setUserAgents($userAgents)
  {
    $this->userAgents = $userAgents;
  }
  /**
   * @return string[]
   */
  public function getUserAgents()
  {
    return $this->userAgents;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudApigeeV1SecurityActionConditionConfig::class, 'Google_Service_Apigee_GoogleCloudApigeeV1SecurityActionConditionConfig');
