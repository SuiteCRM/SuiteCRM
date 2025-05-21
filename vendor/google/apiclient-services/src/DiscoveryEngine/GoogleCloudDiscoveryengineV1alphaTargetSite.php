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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1alphaTargetSite extends \Google\Model
{
  /**
   * @var bool
   */
  public $exactMatch;
  protected $failureReasonType = GoogleCloudDiscoveryengineV1alphaTargetSiteFailureReason::class;
  protected $failureReasonDataType = '';
  /**
   * @var string
   */
  public $generatedUriPattern;
  /**
   * @var string
   */
  public $indexingStatus;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $providedUriPattern;
  /**
   * @var string
   */
  public $rootDomainUri;
  protected $siteVerificationInfoType = GoogleCloudDiscoveryengineV1alphaSiteVerificationInfo::class;
  protected $siteVerificationInfoDataType = '';
  /**
   * @var string
   */
  public $type;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param bool
   */
  public function setExactMatch($exactMatch)
  {
    $this->exactMatch = $exactMatch;
  }
  /**
   * @return bool
   */
  public function getExactMatch()
  {
    return $this->exactMatch;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaTargetSiteFailureReason
   */
  public function setFailureReason(GoogleCloudDiscoveryengineV1alphaTargetSiteFailureReason $failureReason)
  {
    $this->failureReason = $failureReason;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaTargetSiteFailureReason
   */
  public function getFailureReason()
  {
    return $this->failureReason;
  }
  /**
   * @param string
   */
  public function setGeneratedUriPattern($generatedUriPattern)
  {
    $this->generatedUriPattern = $generatedUriPattern;
  }
  /**
   * @return string
   */
  public function getGeneratedUriPattern()
  {
    return $this->generatedUriPattern;
  }
  /**
   * @param string
   */
  public function setIndexingStatus($indexingStatus)
  {
    $this->indexingStatus = $indexingStatus;
  }
  /**
   * @return string
   */
  public function getIndexingStatus()
  {
    return $this->indexingStatus;
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
  public function setProvidedUriPattern($providedUriPattern)
  {
    $this->providedUriPattern = $providedUriPattern;
  }
  /**
   * @return string
   */
  public function getProvidedUriPattern()
  {
    return $this->providedUriPattern;
  }
  /**
   * @param string
   */
  public function setRootDomainUri($rootDomainUri)
  {
    $this->rootDomainUri = $rootDomainUri;
  }
  /**
   * @return string
   */
  public function getRootDomainUri()
  {
    return $this->rootDomainUri;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaSiteVerificationInfo
   */
  public function setSiteVerificationInfo(GoogleCloudDiscoveryengineV1alphaSiteVerificationInfo $siteVerificationInfo)
  {
    $this->siteVerificationInfo = $siteVerificationInfo;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaSiteVerificationInfo
   */
  public function getSiteVerificationInfo()
  {
    return $this->siteVerificationInfo;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1alphaTargetSite::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1alphaTargetSite');
