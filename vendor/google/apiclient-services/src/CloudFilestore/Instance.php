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

namespace Google\Service\CloudFilestore;

class Instance extends \Google\Collection
{
  protected $collection_key = 'suspensionReasons';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var bool
   */
  public $customPerformanceSupported;
  /**
   * @var bool
   */
  public $deletionProtectionEnabled;
  /**
   * @var string
   */
  public $deletionProtectionReason;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $etag;
  protected $fileSharesType = FileShareConfig::class;
  protected $fileSharesDataType = 'array';
  /**
   * @var string
   */
  public $kmsKeyName;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  protected $networksType = NetworkConfig::class;
  protected $networksDataType = 'array';
  protected $performanceConfigType = PerformanceConfig::class;
  protected $performanceConfigDataType = '';
  protected $performanceLimitsType = PerformanceLimits::class;
  protected $performanceLimitsDataType = '';
  /**
   * @var string
   */
  public $protocol;
  protected $replicationType = Replication::class;
  protected $replicationDataType = '';
  /**
   * @var bool
   */
  public $satisfiesPzi;
  /**
   * @var bool
   */
  public $satisfiesPzs;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $statusMessage;
  /**
   * @var string[]
   */
  public $suspensionReasons;
  /**
   * @var string[]
   */
  public $tags;
  /**
   * @var string
   */
  public $tier;

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
   * @param bool
   */
  public function setCustomPerformanceSupported($customPerformanceSupported)
  {
    $this->customPerformanceSupported = $customPerformanceSupported;
  }
  /**
   * @return bool
   */
  public function getCustomPerformanceSupported()
  {
    return $this->customPerformanceSupported;
  }
  /**
   * @param bool
   */
  public function setDeletionProtectionEnabled($deletionProtectionEnabled)
  {
    $this->deletionProtectionEnabled = $deletionProtectionEnabled;
  }
  /**
   * @return bool
   */
  public function getDeletionProtectionEnabled()
  {
    return $this->deletionProtectionEnabled;
  }
  /**
   * @param string
   */
  public function setDeletionProtectionReason($deletionProtectionReason)
  {
    $this->deletionProtectionReason = $deletionProtectionReason;
  }
  /**
   * @return string
   */
  public function getDeletionProtectionReason()
  {
    return $this->deletionProtectionReason;
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
  public function setEtag($etag)
  {
    $this->etag = $etag;
  }
  /**
   * @return string
   */
  public function getEtag()
  {
    return $this->etag;
  }
  /**
   * @param FileShareConfig[]
   */
  public function setFileShares($fileShares)
  {
    $this->fileShares = $fileShares;
  }
  /**
   * @return FileShareConfig[]
   */
  public function getFileShares()
  {
    return $this->fileShares;
  }
  /**
   * @param string
   */
  public function setKmsKeyName($kmsKeyName)
  {
    $this->kmsKeyName = $kmsKeyName;
  }
  /**
   * @return string
   */
  public function getKmsKeyName()
  {
    return $this->kmsKeyName;
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
   * @param NetworkConfig[]
   */
  public function setNetworks($networks)
  {
    $this->networks = $networks;
  }
  /**
   * @return NetworkConfig[]
   */
  public function getNetworks()
  {
    return $this->networks;
  }
  /**
   * @param PerformanceConfig
   */
  public function setPerformanceConfig(PerformanceConfig $performanceConfig)
  {
    $this->performanceConfig = $performanceConfig;
  }
  /**
   * @return PerformanceConfig
   */
  public function getPerformanceConfig()
  {
    return $this->performanceConfig;
  }
  /**
   * @param PerformanceLimits
   */
  public function setPerformanceLimits(PerformanceLimits $performanceLimits)
  {
    $this->performanceLimits = $performanceLimits;
  }
  /**
   * @return PerformanceLimits
   */
  public function getPerformanceLimits()
  {
    return $this->performanceLimits;
  }
  /**
   * @param string
   */
  public function setProtocol($protocol)
  {
    $this->protocol = $protocol;
  }
  /**
   * @return string
   */
  public function getProtocol()
  {
    return $this->protocol;
  }
  /**
   * @param Replication
   */
  public function setReplication(Replication $replication)
  {
    $this->replication = $replication;
  }
  /**
   * @return Replication
   */
  public function getReplication()
  {
    return $this->replication;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzi($satisfiesPzi)
  {
    $this->satisfiesPzi = $satisfiesPzi;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzi()
  {
    return $this->satisfiesPzi;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzs($satisfiesPzs)
  {
    $this->satisfiesPzs = $satisfiesPzs;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzs()
  {
    return $this->satisfiesPzs;
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
  public function setStatusMessage($statusMessage)
  {
    $this->statusMessage = $statusMessage;
  }
  /**
   * @return string
   */
  public function getStatusMessage()
  {
    return $this->statusMessage;
  }
  /**
   * @param string[]
   */
  public function setSuspensionReasons($suspensionReasons)
  {
    $this->suspensionReasons = $suspensionReasons;
  }
  /**
   * @return string[]
   */
  public function getSuspensionReasons()
  {
    return $this->suspensionReasons;
  }
  /**
   * @param string[]
   */
  public function setTags($tags)
  {
    $this->tags = $tags;
  }
  /**
   * @return string[]
   */
  public function getTags()
  {
    return $this->tags;
  }
  /**
   * @param string
   */
  public function setTier($tier)
  {
    $this->tier = $tier;
  }
  /**
   * @return string
   */
  public function getTier()
  {
    return $this->tier;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Instance::class, 'Google_Service_CloudFilestore_Instance');
