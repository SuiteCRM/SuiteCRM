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

namespace Google\Service\DeveloperConnect;

class GitHubEnterpriseConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $appId;
  /**
   * @var string
   */
  public $appInstallationId;
  /**
   * @var string
   */
  public $appSlug;
  /**
   * @var string
   */
  public $hostUri;
  /**
   * @var string
   */
  public $installationUri;
  /**
   * @var string
   */
  public $privateKeySecretVersion;
  /**
   * @var string
   */
  public $serverVersion;
  protected $serviceDirectoryConfigType = ServiceDirectoryConfig::class;
  protected $serviceDirectoryConfigDataType = '';
  /**
   * @var string
   */
  public $sslCaCertificate;
  /**
   * @var string
   */
  public $webhookSecretSecretVersion;

  /**
   * @param string
   */
  public function setAppId($appId)
  {
    $this->appId = $appId;
  }
  /**
   * @return string
   */
  public function getAppId()
  {
    return $this->appId;
  }
  /**
   * @param string
   */
  public function setAppInstallationId($appInstallationId)
  {
    $this->appInstallationId = $appInstallationId;
  }
  /**
   * @return string
   */
  public function getAppInstallationId()
  {
    return $this->appInstallationId;
  }
  /**
   * @param string
   */
  public function setAppSlug($appSlug)
  {
    $this->appSlug = $appSlug;
  }
  /**
   * @return string
   */
  public function getAppSlug()
  {
    return $this->appSlug;
  }
  /**
   * @param string
   */
  public function setHostUri($hostUri)
  {
    $this->hostUri = $hostUri;
  }
  /**
   * @return string
   */
  public function getHostUri()
  {
    return $this->hostUri;
  }
  /**
   * @param string
   */
  public function setInstallationUri($installationUri)
  {
    $this->installationUri = $installationUri;
  }
  /**
   * @return string
   */
  public function getInstallationUri()
  {
    return $this->installationUri;
  }
  /**
   * @param string
   */
  public function setPrivateKeySecretVersion($privateKeySecretVersion)
  {
    $this->privateKeySecretVersion = $privateKeySecretVersion;
  }
  /**
   * @return string
   */
  public function getPrivateKeySecretVersion()
  {
    return $this->privateKeySecretVersion;
  }
  /**
   * @param string
   */
  public function setServerVersion($serverVersion)
  {
    $this->serverVersion = $serverVersion;
  }
  /**
   * @return string
   */
  public function getServerVersion()
  {
    return $this->serverVersion;
  }
  /**
   * @param ServiceDirectoryConfig
   */
  public function setServiceDirectoryConfig(ServiceDirectoryConfig $serviceDirectoryConfig)
  {
    $this->serviceDirectoryConfig = $serviceDirectoryConfig;
  }
  /**
   * @return ServiceDirectoryConfig
   */
  public function getServiceDirectoryConfig()
  {
    return $this->serviceDirectoryConfig;
  }
  /**
   * @param string
   */
  public function setSslCaCertificate($sslCaCertificate)
  {
    $this->sslCaCertificate = $sslCaCertificate;
  }
  /**
   * @return string
   */
  public function getSslCaCertificate()
  {
    return $this->sslCaCertificate;
  }
  /**
   * @param string
   */
  public function setWebhookSecretSecretVersion($webhookSecretSecretVersion)
  {
    $this->webhookSecretSecretVersion = $webhookSecretSecretVersion;
  }
  /**
   * @return string
   */
  public function getWebhookSecretSecretVersion()
  {
    return $this->webhookSecretSecretVersion;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GitHubEnterpriseConfig::class, 'Google_Service_DeveloperConnect_GitHubEnterpriseConfig');
