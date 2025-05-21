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

namespace Google\Service\Dialogflow;

class GoogleCloudDialogflowCxV3WebhookGenericWebService extends \Google\Collection
{
  protected $collection_key = 'allowedCaCerts';
  /**
   * @var string[]
   */
  public $allowedCaCerts;
  /**
   * @var string
   */
  public $httpMethod;
  protected $oauthConfigType = GoogleCloudDialogflowCxV3WebhookGenericWebServiceOAuthConfig::class;
  protected $oauthConfigDataType = '';
  /**
   * @var string[]
   */
  public $parameterMapping;
  /**
   * @var string
   */
  public $password;
  /**
   * @var string
   */
  public $requestBody;
  /**
   * @var string[]
   */
  public $requestHeaders;
  /**
   * @var string
   */
  public $serviceAgentAuth;
  /**
   * @var string
   */
  public $uri;
  /**
   * @var string
   */
  public $username;
  /**
   * @var string
   */
  public $webhookType;

  /**
   * @param string[]
   */
  public function setAllowedCaCerts($allowedCaCerts)
  {
    $this->allowedCaCerts = $allowedCaCerts;
  }
  /**
   * @return string[]
   */
  public function getAllowedCaCerts()
  {
    return $this->allowedCaCerts;
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
   * @param GoogleCloudDialogflowCxV3WebhookGenericWebServiceOAuthConfig
   */
  public function setOauthConfig(GoogleCloudDialogflowCxV3WebhookGenericWebServiceOAuthConfig $oauthConfig)
  {
    $this->oauthConfig = $oauthConfig;
  }
  /**
   * @return GoogleCloudDialogflowCxV3WebhookGenericWebServiceOAuthConfig
   */
  public function getOauthConfig()
  {
    return $this->oauthConfig;
  }
  /**
   * @param string[]
   */
  public function setParameterMapping($parameterMapping)
  {
    $this->parameterMapping = $parameterMapping;
  }
  /**
   * @return string[]
   */
  public function getParameterMapping()
  {
    return $this->parameterMapping;
  }
  /**
   * @param string
   */
  public function setPassword($password)
  {
    $this->password = $password;
  }
  /**
   * @return string
   */
  public function getPassword()
  {
    return $this->password;
  }
  /**
   * @param string
   */
  public function setRequestBody($requestBody)
  {
    $this->requestBody = $requestBody;
  }
  /**
   * @return string
   */
  public function getRequestBody()
  {
    return $this->requestBody;
  }
  /**
   * @param string[]
   */
  public function setRequestHeaders($requestHeaders)
  {
    $this->requestHeaders = $requestHeaders;
  }
  /**
   * @return string[]
   */
  public function getRequestHeaders()
  {
    return $this->requestHeaders;
  }
  /**
   * @param string
   */
  public function setServiceAgentAuth($serviceAgentAuth)
  {
    $this->serviceAgentAuth = $serviceAgentAuth;
  }
  /**
   * @return string
   */
  public function getServiceAgentAuth()
  {
    return $this->serviceAgentAuth;
  }
  /**
   * @param string
   */
  public function setUri($uri)
  {
    $this->uri = $uri;
  }
  /**
   * @return string
   */
  public function getUri()
  {
    return $this->uri;
  }
  /**
   * @param string
   */
  public function setUsername($username)
  {
    $this->username = $username;
  }
  /**
   * @return string
   */
  public function getUsername()
  {
    return $this->username;
  }
  /**
   * @param string
   */
  public function setWebhookType($webhookType)
  {
    $this->webhookType = $webhookType;
  }
  /**
   * @return string
   */
  public function getWebhookType()
  {
    return $this->webhookType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDialogflowCxV3WebhookGenericWebService::class, 'Google_Service_Dialogflow_GoogleCloudDialogflowCxV3WebhookGenericWebService');
