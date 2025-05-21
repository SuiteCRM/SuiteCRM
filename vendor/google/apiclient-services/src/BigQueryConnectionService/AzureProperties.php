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

namespace Google\Service\BigQueryConnectionService;

class AzureProperties extends \Google\Model
{
  /**
   * @var string
   */
  public $application;
  /**
   * @var string
   */
  public $clientId;
  /**
   * @var string
   */
  public $customerTenantId;
  /**
   * @var string
   */
  public $federatedApplicationClientId;
  /**
   * @var string
   */
  public $identity;
  /**
   * @var string
   */
  public $objectId;
  /**
   * @var string
   */
  public $redirectUri;

  /**
   * @param string
   */
  public function setApplication($application)
  {
    $this->application = $application;
  }
  /**
   * @return string
   */
  public function getApplication()
  {
    return $this->application;
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
   * @param string
   */
  public function setCustomerTenantId($customerTenantId)
  {
    $this->customerTenantId = $customerTenantId;
  }
  /**
   * @return string
   */
  public function getCustomerTenantId()
  {
    return $this->customerTenantId;
  }
  /**
   * @param string
   */
  public function setFederatedApplicationClientId($federatedApplicationClientId)
  {
    $this->federatedApplicationClientId = $federatedApplicationClientId;
  }
  /**
   * @return string
   */
  public function getFederatedApplicationClientId()
  {
    return $this->federatedApplicationClientId;
  }
  /**
   * @param string
   */
  public function setIdentity($identity)
  {
    $this->identity = $identity;
  }
  /**
   * @return string
   */
  public function getIdentity()
  {
    return $this->identity;
  }
  /**
   * @param string
   */
  public function setObjectId($objectId)
  {
    $this->objectId = $objectId;
  }
  /**
   * @return string
   */
  public function getObjectId()
  {
    return $this->objectId;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AzureProperties::class, 'Google_Service_BigQueryConnectionService_AzureProperties');
