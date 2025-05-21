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

class GatewaySecurityPolicyRule extends \Google\Model
{
  /**
   * @var string
   */
  public $applicationMatcher;
  /**
   * @var string
   */
  public $basicProfile;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var bool
   */
  public $enabled;
  /**
   * @var string
   */
  public $name;
  /**
   * @var int
   */
  public $priority;
  /**
   * @var string
   */
  public $sessionMatcher;
  /**
   * @var bool
   */
  public $tlsInspectionEnabled;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string
   */
  public function setApplicationMatcher($applicationMatcher)
  {
    $this->applicationMatcher = $applicationMatcher;
  }
  /**
   * @return string
   */
  public function getApplicationMatcher()
  {
    return $this->applicationMatcher;
  }
  /**
   * @param string
   */
  public function setBasicProfile($basicProfile)
  {
    $this->basicProfile = $basicProfile;
  }
  /**
   * @return string
   */
  public function getBasicProfile()
  {
    return $this->basicProfile;
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
   * @param bool
   */
  public function setEnabled($enabled)
  {
    $this->enabled = $enabled;
  }
  /**
   * @return bool
   */
  public function getEnabled()
  {
    return $this->enabled;
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
   * @param int
   */
  public function setPriority($priority)
  {
    $this->priority = $priority;
  }
  /**
   * @return int
   */
  public function getPriority()
  {
    return $this->priority;
  }
  /**
   * @param string
   */
  public function setSessionMatcher($sessionMatcher)
  {
    $this->sessionMatcher = $sessionMatcher;
  }
  /**
   * @return string
   */
  public function getSessionMatcher()
  {
    return $this->sessionMatcher;
  }
  /**
   * @param bool
   */
  public function setTlsInspectionEnabled($tlsInspectionEnabled)
  {
    $this->tlsInspectionEnabled = $tlsInspectionEnabled;
  }
  /**
   * @return bool
   */
  public function getTlsInspectionEnabled()
  {
    return $this->tlsInspectionEnabled;
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
class_alias(GatewaySecurityPolicyRule::class, 'Google_Service_NetworkSecurity_GatewaySecurityPolicyRule');
