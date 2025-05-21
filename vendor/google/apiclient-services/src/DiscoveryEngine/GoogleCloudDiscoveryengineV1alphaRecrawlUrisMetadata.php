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

class GoogleCloudDiscoveryengineV1alphaRecrawlUrisMetadata extends \Google\Collection
{
  protected $collection_key = 'urisNotMatchingTargetSites';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string[]
   */
  public $invalidUris;
  /**
   * @var int
   */
  public $invalidUrisCount;
  /**
   * @var string[]
   */
  public $noindexUris;
  /**
   * @var int
   */
  public $noindexUrisCount;
  /**
   * @var int
   */
  public $pendingCount;
  /**
   * @var int
   */
  public $quotaExceededCount;
  /**
   * @var int
   */
  public $successCount;
  /**
   * @var string
   */
  public $updateTime;
  /**
   * @var string[]
   */
  public $urisNotMatchingTargetSites;
  /**
   * @var int
   */
  public $urisNotMatchingTargetSitesCount;
  /**
   * @var int
   */
  public $validUrisCount;

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
   * @param string[]
   */
  public function setInvalidUris($invalidUris)
  {
    $this->invalidUris = $invalidUris;
  }
  /**
   * @return string[]
   */
  public function getInvalidUris()
  {
    return $this->invalidUris;
  }
  /**
   * @param int
   */
  public function setInvalidUrisCount($invalidUrisCount)
  {
    $this->invalidUrisCount = $invalidUrisCount;
  }
  /**
   * @return int
   */
  public function getInvalidUrisCount()
  {
    return $this->invalidUrisCount;
  }
  /**
   * @param string[]
   */
  public function setNoindexUris($noindexUris)
  {
    $this->noindexUris = $noindexUris;
  }
  /**
   * @return string[]
   */
  public function getNoindexUris()
  {
    return $this->noindexUris;
  }
  /**
   * @param int
   */
  public function setNoindexUrisCount($noindexUrisCount)
  {
    $this->noindexUrisCount = $noindexUrisCount;
  }
  /**
   * @return int
   */
  public function getNoindexUrisCount()
  {
    return $this->noindexUrisCount;
  }
  /**
   * @param int
   */
  public function setPendingCount($pendingCount)
  {
    $this->pendingCount = $pendingCount;
  }
  /**
   * @return int
   */
  public function getPendingCount()
  {
    return $this->pendingCount;
  }
  /**
   * @param int
   */
  public function setQuotaExceededCount($quotaExceededCount)
  {
    $this->quotaExceededCount = $quotaExceededCount;
  }
  /**
   * @return int
   */
  public function getQuotaExceededCount()
  {
    return $this->quotaExceededCount;
  }
  /**
   * @param int
   */
  public function setSuccessCount($successCount)
  {
    $this->successCount = $successCount;
  }
  /**
   * @return int
   */
  public function getSuccessCount()
  {
    return $this->successCount;
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
  /**
   * @param string[]
   */
  public function setUrisNotMatchingTargetSites($urisNotMatchingTargetSites)
  {
    $this->urisNotMatchingTargetSites = $urisNotMatchingTargetSites;
  }
  /**
   * @return string[]
   */
  public function getUrisNotMatchingTargetSites()
  {
    return $this->urisNotMatchingTargetSites;
  }
  /**
   * @param int
   */
  public function setUrisNotMatchingTargetSitesCount($urisNotMatchingTargetSitesCount)
  {
    $this->urisNotMatchingTargetSitesCount = $urisNotMatchingTargetSitesCount;
  }
  /**
   * @return int
   */
  public function getUrisNotMatchingTargetSitesCount()
  {
    return $this->urisNotMatchingTargetSitesCount;
  }
  /**
   * @param int
   */
  public function setValidUrisCount($validUrisCount)
  {
    $this->validUrisCount = $validUrisCount;
  }
  /**
   * @return int
   */
  public function getValidUrisCount()
  {
    return $this->validUrisCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1alphaRecrawlUrisMetadata::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1alphaRecrawlUrisMetadata');
