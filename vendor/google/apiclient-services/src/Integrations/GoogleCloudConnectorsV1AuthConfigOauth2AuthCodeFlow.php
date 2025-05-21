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

namespace Google\Service\Integrations;

class GoogleCloudConnectorsV1AuthConfigOauth2AuthCodeFlow extends \Google\Collection
{
  protected $collection_key = 'scopes';
  /**
   * @var string
   */
  public $authCode;
  /**
   * @var string
   */
  public $authUri;
  /**
   * @var string
   */
  public $clientId;
  protected $clientSecretType = GoogleCloudConnectorsV1Secret::class;
  protected $clientSecretDataType = '';
  /**
   * @var bool
   */
  public $enablePkce;
  /**
   * @var string
   */
  public $pkceVerifier;
  /**
   * @var string
   */
  public $redirectUri;
  /**
   * @var string[]
   */
  public $scopes;

  /**
   * @param string
   */
  public function setAuthCode($authCode)
  {
    $this->authCode = $authCode;
  }
  /**
   * @return string
   */
  public function getAuthCode()
  {
    return $this->authCode;
  }
  /**
   * @param string
   */
  public function setAuthUri($authUri)
  {
    $this->authUri = $authUri;
  }
  /**
   * @return string
   */
  public function getAuthUri()
  {
    return $this->authUri;
  }
  /**
   * @param string
   */
  public function setClientId($clientId)
  {
    $this->clientId = $clientId;
  }
  /**
   * @return string
   */
  public function getClientId()
  {
    return $this->clientId;
  }
  /**
   * @param GoogleCloudConnectorsV1Secret
   */
  public function setClientSecret(GoogleCloudConnectorsV1Secret $clientSecret)
  {
    $this->clientSecret = $clientSecret;
  }
  /**
   * @return GoogleCloudConnectorsV1Secret
   */
  public function getClientSecret()
  {
    return $this->clientSecret;
  }
  /**
   * @param bool
   */
  public function setEnablePkce($enablePkce)
  {
    $this->enablePkce = $enablePkce;
  }
  /**
   * @return bool
   */
  public function getEnablePkce()
  {
    return $this->enablePkce;
  }
  /**
   * @param string
   */
  public function setPkceVerifier($pkceVerifier)
  {
    $this->pkceVerifier = $pkceVerifier;
  }
  /**
   * @return string
   */
  public function getPkceVerifier()
  {
    return $this->pkceVerifier;
  }
  /**
   * @param string
   */
  public function setRedirectUri($redirectUri)
  {
    $this->redirectUri = $redirectUri;
  }
  /**
   * @return string
   */
  public function getRedirectUri()
  {
    return $this->redirectUri;
  }
  /**
   * @param string[]
   */
  public function setScopes($scopes)
  {
    $this->scopes = $scopes;
  }
  /**
   * @return string[]
   */
  public function getScopes()
  {
    return $this->scopes;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudConnectorsV1AuthConfigOauth2AuthCodeFlow::class, 'Google_Service_Integrations_GoogleCloudConnectorsV1AuthConfigOauth2AuthCodeFlow');
