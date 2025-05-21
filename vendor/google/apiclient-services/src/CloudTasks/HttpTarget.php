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

namespace Google\Service\CloudTasks;

class HttpTarget extends \Google\Collection
{
  protected $collection_key = 'headerOverrides';
  protected $headerOverridesType = HeaderOverride::class;
  protected $headerOverridesDataType = 'array';
  /**
   * @var string
   */
  public $httpMethod;
  protected $oauthTokenType = OAuthToken::class;
  protected $oauthTokenDataType = '';
  protected $oidcTokenType = OidcToken::class;
  protected $oidcTokenDataType = '';
  protected $uriOverrideType = UriOverride::class;
  protected $uriOverrideDataType = '';

  /**
   * @param HeaderOverride[]
   */
  public function setHeaderOverrides($headerOverrides)
  {
    $this->headerOverrides = $headerOverrides;
  }
  /**
   * @return HeaderOverride[]
   */
  public function getHeaderOverrides()
  {
    return $this->headerOverrides;
  }
  /**
   * @param string
   */
  public function setHttpMethod($httpMethod)
  {
    $this->httpMethod = $httpMethod;
  }
  /**
   * @return string
   */
  public function getHttpMethod()
  {
    return $this->httpMethod;
  }
  /**
   * @param OAuthToken
   */
  public function setOauthToken(OAuthToken $oauthToken)
  {
    $this->oauthToken = $oauthToken;
  }
  /**
   * @return OAuthToken
   */
  public function getOauthToken()
  {
    return $this->oauthToken;
  }
  /**
   * @param OidcToken
   */
  public function setOidcToken(OidcToken $oidcToken)
  {
    $this->oidcToken = $oidcToken;
  }
  /**
   * @return OidcToken
   */
  public function getOidcToken()
  {
    return $this->oidcToken;
  }
  /**
   * @param UriOverride
   */
  public function setUriOverride(UriOverride $uriOverride)
  {
    $this->uriOverride = $uriOverride;
  }
  /**
   * @return UriOverride
   */
  public function getUriOverride()
  {
    return $this->uriOverride;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(HttpTarget::class, 'Google_Service_CloudTasks_HttpTarget');
