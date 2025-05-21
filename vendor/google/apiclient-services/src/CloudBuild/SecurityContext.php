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

namespace Google\Service\CloudBuild;

class SecurityContext extends \Google\Model
{
  /**
   * @var bool
   */
  public $allowPrivilegeEscalation;
  protected $capabilitiesType = Capabilities::class;
  protected $capabilitiesDataType = '';
  /**
   * @var bool
   */
  public $privileged;
  /**
   * @var string
   */
  public $runAsGroup;
  /**
   * @var bool
   */
  public $runAsNonRoot;
  /**
   * @var string
   */
  public $runAsUser;

  /**
   * @param bool
   */
  public function setAllowPrivilegeEscalation($allowPrivilegeEscalation)
  {
    $this->allowPrivilegeEscalation = $allowPrivilegeEscalation;
  }
  /**
   * @return bool
   */
  public function getAllowPrivilegeEscalation()
  {
    return $this->allowPrivilegeEscalation;
  }
  /**
   * @param Capabilities
   */
  public function setCapabilities(Capabilities $capabilities)
  {
    $this->capabilities = $capabilities;
  }
  /**
   * @return Capabilities
   */
  public function getCapabilities()
  {
    return $this->capabilities;
  }
  /**
   * @param bool
   */
  public function setPrivileged($privileged)
  {
    $this->privileged = $privileged;
  }
  /**
   * @return bool
   */
  public function getPrivileged()
  {
    return $this->privileged;
  }
  /**
   * @param string
   */
  public function setRunAsGroup($runAsGroup)
  {
    $this->runAsGroup = $runAsGroup;
  }
  /**
   * @return string
   */
  public function getRunAsGroup()
  {
    return $this->runAsGroup;
  }
  /**
   * @param bool
   */
  public function setRunAsNonRoot($runAsNonRoot)
  {
    $this->runAsNonRoot = $runAsNonRoot;
  }
  /**
   * @return bool
   */
  public function getRunAsNonRoot()
  {
    return $this->runAsNonRoot;
  }
  /**
   * @param string
   */
  public function setRunAsUser($runAsUser)
  {
    $this->runAsUser = $runAsUser;
  }
  /**
   * @return string
   */
  public function getRunAsUser()
  {
    return $this->runAsUser;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SecurityContext::class, 'Google_Service_CloudBuild_SecurityContext');
