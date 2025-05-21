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

class GoogleCloudDiscoveryengineV1betaCmekConfig extends \Google\Collection
{
  protected $collection_key = 'singleRegionKeys';
  /**
   * @var bool
   */
  public $isDefault;
  /**
   * @var string
   */
  public $kmsKey;
  /**
   * @var string
   */
  public $kmsKeyVersion;
  /**
   * @var string
   */
  public $lastRotationTimestampMicros;
  /**
   * @var string
   */
  public $name;
  protected $singleRegionKeysType = GoogleCloudDiscoveryengineV1betaSingleRegionKey::class;
  protected $singleRegionKeysDataType = 'array';
  /**
   * @var string
   */
  public $state;

  /**
   * @param bool
   */
  public function setIsDefault($isDefault)
  {
    $this->isDefault = $isDefault;
  }
  /**
   * @return bool
   */
  public function getIsDefault()
  {
    return $this->isDefault;
  }
  /**
   * @param string
   */
  public function setKmsKey($kmsKey)
  {
    $this->kmsKey = $kmsKey;
  }
  /**
   * @return string
   */
  public function getKmsKey()
  {
    return $this->kmsKey;
  }
  /**
   * @param string
   */
  public function setKmsKeyVersion($kmsKeyVersion)
  {
    $this->kmsKeyVersion = $kmsKeyVersion;
  }
  /**
   * @return string
   */
  public function getKmsKeyVersion()
  {
    return $this->kmsKeyVersion;
  }
  /**
   * @param string
   */
  public function setLastRotationTimestampMicros($lastRotationTimestampMicros)
  {
    $this->lastRotationTimestampMicros = $lastRotationTimestampMicros;
  }
  /**
   * @return string
   */
  public function getLastRotationTimestampMicros()
  {
    return $this->lastRotationTimestampMicros;
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
   * @param GoogleCloudDiscoveryengineV1betaSingleRegionKey[]
   */
  public function setSingleRegionKeys($singleRegionKeys)
  {
    $this->singleRegionKeys = $singleRegionKeys;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaSingleRegionKey[]
   */
  public function getSingleRegionKeys()
  {
    return $this->singleRegionKeys;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1betaCmekConfig::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1betaCmekConfig');
