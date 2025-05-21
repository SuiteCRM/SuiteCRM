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

namespace Google\Service\NetworkSecurity;

class TlsInspectionPolicy extends \Google\Collection
{
  protected $collection_key = 'customTlsFeatures';
  /**
   * @var string
   */
  public $caPool;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string[]
   */
  public $customTlsFeatures;
  /**
   * @var string
   */
  public $description;
  /**
   * @var bool
   */
  public $excludePublicCaSet;
  /**
   * @var string
   */
  public $minTlsVersion;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $tlsFeatureProfile;
  /**
   * @var string
   */
  public $trustConfig;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string
   */
  public function setCaPool($caPool)
  {
    $this->caPool = $caPool;
  }
  /**
   * @return string
   */
  public function getCaPool()
  {
    return $this->caPool;
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
   * @param string[]
   */
  public function setCustomTlsFeatures($customTlsFeatures)
  {
    $this->customTlsFeatures = $customTlsFeatures;
  }
  /**
   * @return string[]
   */
  public function getCustomTlsFeatures()
  {
    return $this->customTlsFeatures;
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
   * @param bool
   */
  public function setExcludePublicCaSet($excludePublicCaSet)
  {
    $this->excludePublicCaSet = $excludePublicCaSet;
  }
  /**
   * @return bool
   */
  public function getExcludePublicCaSet()
  {
    return $this->excludePublicCaSet;
  }
  /**
   * @param string
   */
  public function setMinTlsVersion($minTlsVersion)
  {
    $this->minTlsVersion = $minTlsVersion;
  }
  /**
   * @return string
   */
  public function getMinTlsVersion()
  {
    return $this->minTlsVersion;
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
  public function setTlsFeatureProfile($tlsFeatureProfile)
  {
    $this->tlsFeatureProfile = $tlsFeatureProfile;
  }
  /**
   * @return string
   */
  public function getTlsFeatureProfile()
  {
    return $this->tlsFeatureProfile;
  }
  /**
   * @param string
   */
  public function setTrustConfig($trustConfig)
  {
    $this->trustConfig = $trustConfig;
  }
  /**
   * @return string
   */
  public function getTrustConfig()
  {
    return $this->trustConfig;
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
class_alias(TlsInspectionPolicy::class, 'Google_Service_NetworkSecurity_TlsInspectionPolicy');
