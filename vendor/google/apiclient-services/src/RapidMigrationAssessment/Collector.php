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

namespace Google\Service\RapidMigrationAssessment;

class Collector extends \Google\Model
{
  /**
   * @var string
   */
  public $bucket;
  /**
   * @var string
   */
  public $clientVersion;
  /**
   * @var int
   */
  public $collectionDays;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $eulaUri;
  /**
   * @var string
   */
  public $expectedAssetCount;
  protected $guestOsScanType = GuestOsScan::class;
  protected $guestOsScanDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $serviceAccount;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $updateTime;
  protected $vsphereScanType = VSphereScan::class;
  protected $vsphereScanDataType = '';

  /**
   * @param string
   */
  public function setBucket($bucket)
  {
    $this->bucket = $bucket;
  }
  /**
   * @return string
   */
  public function getBucket()
  {
    return $this->bucket;
  }
  /**
   * @param string
   */
  public function setClientVersion($clientVersion)
  {
    $this->clientVersion = $clientVersion;
  }
  /**
   * @return string
   */
  public function getClientVersion()
  {
    return $this->clientVersion;
  }
  /**
   * @param int
   */
  public function setCollectionDays($collectionDays)
  {
    $this->collectionDays = $collectionDays;
  }
  /**
   * @return int
   */
  public function getCollectionDays()
  {
    return $this->collectionDays;
  }
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
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param string
   */
  public function setEulaUri($eulaUri)
  {
    $this->eulaUri = $eulaUri;
  }
  /**
   * @return string
   */
  public function getEulaUri()
  {
    return $this->eulaUri;
  }
  /**
   * @param string
   */
  public function setExpectedAssetCount($expectedAssetCount)
  {
    $this->expectedAssetCount = $expectedAssetCount;
  }
  /**
   * @return string
   */
  public function getExpectedAssetCount()
  {
    return $this->expectedAssetCount;
  }
  /**
   * @param GuestOsScan
   */
  public function setGuestOsScan(GuestOsScan $guestOsScan)
  {
    $this->guestOsScan = $guestOsScan;
  }
  /**
   * @return GuestOsScan
   */
  public function getGuestOsScan()
  {
    return $this->guestOsScan;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
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
  public function setServiceAccount($serviceAccount)
  {
    $this->serviceAccount = $serviceAccount;
  }
  /**
   * @return string
   */
  public function getServiceAccount()
  {
    return $this->serviceAccount;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
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
   * @param VSphereScan
   */
  public function setVsphereScan(VSphereScan $vsphereScan)
  {
    $this->vsphereScan = $vsphereScan;
  }
  /**
   * @return VSphereScan
   */
  public function getVsphereScan()
  {
    return $this->vsphereScan;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Collector::class, 'Google_Service_RapidMigrationAssessment_Collector');
