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

namespace Google\Service\CloudHealthcare;

class ConsentConfig extends \Google\Collection
{
  protected $collection_key = 'enforcedAdminConsents';
  protected $accessDeterminationLogConfigType = AccessDeterminationLogConfig::class;
  protected $accessDeterminationLogConfigDataType = '';
  /**
   * @var bool
   */
  public $accessEnforced;
  protected $consentHeaderHandlingType = ConsentHeaderHandling::class;
  protected $consentHeaderHandlingDataType = '';
  /**
   * @var string[]
   */
  public $enforcedAdminConsents;
  /**
   * @var string
   */
  public $version;

  /**
   * @param AccessDeterminationLogConfig
   */
  public function setAccessDeterminationLogConfig(AccessDeterminationLogConfig $accessDeterminationLogConfig)
  {
    $this->accessDeterminationLogConfig = $accessDeterminationLogConfig;
  }
  /**
   * @return AccessDeterminationLogConfig
   */
  public function getAccessDeterminationLogConfig()
  {
    return $this->accessDeterminationLogConfig;
  }
  /**
   * @param bool
   */
  public function setAccessEnforced($accessEnforced)
  {
    $this->accessEnforced = $accessEnforced;
  }
  /**
   * @return bool
   */
  public function getAccessEnforced()
  {
    return $this->accessEnforced;
  }
  /**
   * @param ConsentHeaderHandling
   */
  public function setConsentHeaderHandling(ConsentHeaderHandling $consentHeaderHandling)
  {
    $this->consentHeaderHandling = $consentHeaderHandling;
  }
  /**
   * @return ConsentHeaderHandling
   */
  public function getConsentHeaderHandling()
  {
    return $this->consentHeaderHandling;
  }
  /**
   * @param string[]
   */
  public function setEnforcedAdminConsents($enforcedAdminConsents)
  {
    $this->enforcedAdminConsents = $enforcedAdminConsents;
  }
  /**
   * @return string[]
   */
  public function getEnforcedAdminConsents()
  {
    return $this->enforcedAdminConsents;
  }
  /**
   * @param string
   */
  public function setVersion($version)
  {
    $this->version = $version;
  }
  /**
   * @return string
   */
  public function getVersion()
  {
    return $this->version;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ConsentConfig::class, 'Google_Service_CloudHealthcare_ConsentConfig');
