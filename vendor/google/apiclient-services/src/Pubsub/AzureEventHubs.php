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

namespace Google\Service\Pubsub;

class AzureEventHubs extends \Google\Model
{
  /**
   * @var string
   */
  public $clientId;
  /**
   * @var string
   */
  public $eventHub;
  /**
   * @var string
   */
  public $gcpServiceAccount;
  /**
   * @var string
   */
  public $namespace;
  /**
   * @var string
   */
  public $resourceGroup;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $subscriptionId;
  /**
   * @var string
   */
  public $tenantId;

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
  public function setEventHub($eventHub)
  {
    $this->eventHub = $eventHub;
  }
  /**
   * @return string
   */
  public function getEventHub()
  {
    return $this->eventHub;
  }
  /**
   * @param string
   */
  public function setGcpServiceAccount($gcpServiceAccount)
  {
    $this->gcpServiceAccount = $gcpServiceAccount;
  }
  /**
   * @return string
   */
  public function getGcpServiceAccount()
  {
    return $this->gcpServiceAccount;
  }
  /**
   * @param string
   */
  public function setNamespace($namespace)
  {
    $this->namespace = $namespace;
  }
  /**
   * @return string
   */
  public function getNamespace()
  {
    return $this->namespace;
  }
  /**
   * @param string
   */
  public function setResourceGroup($resourceGroup)
  {
    $this->resourceGroup = $resourceGroup;
  }
  /**
   * @return string
   */
  public function getResourceGroup()
  {
    return $this->resourceGroup;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param string
   */
  public function setSubscriptionId($subscriptionId)
  {
    $this->subscriptionId = $subscriptionId;
  }
  /**
   * @return string
   */
  public function getSubscriptionId()
  {
    return $this->subscriptionId;
  }
  /**
   * @param string
   */
  public function setTenantId($tenantId)
  {
    $this->tenantId = $tenantId;
  }
  /**
   * @return string
   */
  public function getTenantId()
  {
    return $this->tenantId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AzureEventHubs::class, 'Google_Service_Pubsub_AzureEventHubs');
