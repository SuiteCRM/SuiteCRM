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

namespace Google\Service\Contentwarehouse;

class CloudAiPlatformTenantresourceInfraSpannerConfig extends \Google\Model
{
  protected $createDatabaseOptionsType = CloudAiPlatformTenantresourceInfraSpannerConfigCreateDatabaseOptions::class;
  protected $createDatabaseOptionsDataType = '';
  /**
   * @var string
   */
  public $kmsKeyReference;
  /**
   * @var string
   */
  public $sdlBundlePath;
  /**
   * @var string
   */
  public $spannerBorgServiceAccount;
  /**
   * @var string
   */
  public $spannerLocalNamePrefix;
  /**
   * @var string
   */
  public $spannerNamespace;
  /**
   * @var string
   */
  public $spannerUniverse;

  /**
   * @param CloudAiPlatformTenantresourceInfraSpannerConfigCreateDatabaseOptions
   */
  public function setCreateDatabaseOptions(CloudAiPlatformTenantresourceInfraSpannerConfigCreateDatabaseOptions $createDatabaseOptions)
  {
    $this->createDatabaseOptions = $createDatabaseOptions;
  }
  /**
   * @return CloudAiPlatformTenantresourceInfraSpannerConfigCreateDatabaseOptions
   */
  public function getCreateDatabaseOptions()
  {
    return $this->createDatabaseOptions;
  }
  /**
   * @param string
   */
  public function setKmsKeyReference($kmsKeyReference)
  {
    $this->kmsKeyReference = $kmsKeyReference;
  }
  /**
   * @return string
   */
  public function getKmsKeyReference()
  {
    return $this->kmsKeyReference;
  }
  /**
   * @param string
   */
  public function setSdlBundlePath($sdlBundlePath)
  {
    $this->sdlBundlePath = $sdlBundlePath;
  }
  /**
   * @return string
   */
  public function getSdlBundlePath()
  {
    return $this->sdlBundlePath;
  }
  /**
   * @param string
   */
  public function setSpannerBorgServiceAccount($spannerBorgServiceAccount)
  {
    $this->spannerBorgServiceAccount = $spannerBorgServiceAccount;
  }
  /**
   * @return string
   */
  public function getSpannerBorgServiceAccount()
  {
    return $this->spannerBorgServiceAccount;
  }
  /**
   * @param string
   */
  public function setSpannerLocalNamePrefix($spannerLocalNamePrefix)
  {
    $this->spannerLocalNamePrefix = $spannerLocalNamePrefix;
  }
  /**
   * @return string
   */
  public function getSpannerLocalNamePrefix()
  {
    return $this->spannerLocalNamePrefix;
  }
  /**
   * @param string
   */
  public function setSpannerNamespace($spannerNamespace)
  {
    $this->spannerNamespace = $spannerNamespace;
  }
  /**
   * @return string
   */
  public function getSpannerNamespace()
  {
    return $this->spannerNamespace;
  }
  /**
   * @param string
   */
  public function setSpannerUniverse($spannerUniverse)
  {
    $this->spannerUniverse = $spannerUniverse;
  }
  /**
   * @return string
   */
  public function getSpannerUniverse()
  {
    return $this->spannerUniverse;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CloudAiPlatformTenantresourceInfraSpannerConfig::class, 'Google_Service_Contentwarehouse_CloudAiPlatformTenantresourceInfraSpannerConfig');
