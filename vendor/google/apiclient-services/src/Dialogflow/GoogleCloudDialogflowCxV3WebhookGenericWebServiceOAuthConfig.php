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

class GoogleCloudDialogflowCxV3WebhookGenericWebServiceOAuthConfig extends \Google\Collection
{
  protected $collection_key = 'scopes';
  /**
   * @var string
   */
  public $clientId;
  /**
   * @var string
   */
  public $clientSecret;
  /**
   * @var string[]
   */
  public $scopes;
  /**
   * @var string
   */
  public $tokenEndpoint;

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
   * @param string
   */
  public function setClientSecret($clientSecret)
  {
    $this->clientSecret = $clientSecret;
  }
  /**
   * @return string
   */
  public function getClientSecret()
  {
    return $this->clientSecret;
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
  /**
   * @param string
   */
  public function setTokenEndpoint($tokenEndpoint)
  {
    $this->tokenEndpoint = $tokenEndpoint;
  }
  /**
   * @return string
   */
  public function getTokenEndpoint()
  {
    return $this->tokenEndpoint;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDialogflowCxV3WebhookGenericWebServiceOAuthConfig::class, 'Google_Service_Dialogflow_GoogleCloudDialogflowCxV3WebhookGenericWebServiceOAuthConfig');
