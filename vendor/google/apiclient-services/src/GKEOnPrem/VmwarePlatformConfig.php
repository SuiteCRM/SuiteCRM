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

namespace Google\Service\GKEOnPrem;

class VmwarePlatformConfig extends \Google\Collection
{
  protected $collection_key = 'bundles';
  protected $bundlesType = VmwareBundleConfig::class;
  protected $bundlesDataType = 'array';
  /**
   * @var string
   */
  public $platformVersion;
  /**
   * @var string
   */
  public $requiredPlatformVersion;
  protected $statusType = ResourceStatus::class;
  protected $statusDataType = '';

  /**
   * @param VmwareBundleConfig[]
   */
  public function setBundles($bundles)
  {
    $this->bundles = $bundles;
  }
  /**
   * @return VmwareBundleConfig[]
   */
  public function getBundles()
  {
    return $this->bundles;
  }
  /**
   * @param string
   */
  public function setPlatformVersion($platformVersion)
  {
    $this->platformVersion = $platformVersion;
  }
  /**
   * @return string
   */
  public function getPlatformVersion()
  {
    return $this->platformVersion;
  }
  /**
   * @param string
   */
  public function setRequiredPlatformVersion($requiredPlatformVersion)
  {
    $this->requiredPlatformVersion = $requiredPlatformVersion;
  }
  /**
   * @return string
   */
  public function getRequiredPlatformVersion()
  {
    return $this->requiredPlatformVersion;
  }
  /**
   * @param ResourceStatus
   */
  public function setStatus(ResourceStatus $status)
  {
    $this->status = $status;
  }
  /**
   * @return ResourceStatus
   */
  public function getStatus()
  {
    return $this->status;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(VmwarePlatformConfig::class, 'Google_Service_GKEOnPrem_VmwarePlatformConfig');
