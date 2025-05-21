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

namespace Google\Service\Dataform;

class GitRemoteSettings extends \Google\Model
{
  /**
   * @var string
   */
  public $authenticationTokenSecretVersion;
  /**
   * @var string
   */
  public $defaultBranch;
  protected $sshAuthenticationConfigType = SshAuthenticationConfig::class;
  protected $sshAuthenticationConfigDataType = '';
  /**
   * @var string
   */
  public $tokenStatus;
  /**
   * @var string
   */
  public $url;

  /**
   * @param string
   */
  public function setAuthenticationTokenSecretVersion($authenticationTokenSecretVersion)
  {
    $this->authenticationTokenSecretVersion = $authenticationTokenSecretVersion;
  }
  /**
   * @return string
   */
  public function getAuthenticationTokenSecretVersion()
  {
    return $this->authenticationTokenSecretVersion;
  }
  /**
   * @param string
   */
  public function setDefaultBranch($defaultBranch)
  {
    $this->defaultBranch = $defaultBranch;
  }
  /**
   * @return string
   */
  public function getDefaultBranch()
  {
    return $this->defaultBranch;
  }
  /**
   * @param SshAuthenticationConfig
   */
  public function setSshAuthenticationConfig(SshAuthenticationConfig $sshAuthenticationConfig)
  {
    $this->sshAuthenticationConfig = $sshAuthenticationConfig;
  }
  /**
   * @return SshAuthenticationConfig
   */
  public function getSshAuthenticationConfig()
  {
    return $this->sshAuthenticationConfig;
  }
  /**
   * @param string
   */
  public function setTokenStatus($tokenStatus)
  {
    $this->tokenStatus = $tokenStatus;
  }
  /**
   * @return string
   */
  public function getTokenStatus()
  {
    return $this->tokenStatus;
  }
  /**
   * @param string
   */
  public function setUrl($url)
  {
    $this->url = $url;
  }
  /**
   * @return string
   */
  public function getUrl()
  {
    return $this->url;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GitRemoteSettings::class, 'Google_Service_Dataform_GitRemoteSettings');
