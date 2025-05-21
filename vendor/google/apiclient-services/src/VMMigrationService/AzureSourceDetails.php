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

namespace Google\Service\VMMigrationService;

class AzureSourceDetails extends \Google\Model
{
  /**
   * @var string
   */
  public $azureLocation;
  protected $clientSecretCredsType = ClientSecretCredentials::class;
  protected $clientSecretCredsDataType = '';
  protected $errorType = Status::class;
  protected $errorDataType = '';
  /**
   * @var string[]
   */
  public $migrationResourcesUserTags;
  /**
   * @var string
   */
  public $resourceGroupId;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $subscriptionId;

  /**
   * @param string
   */
  public function setAzureLocation($azureLocation)
  {
    $this->azureLocation = $azureLocation;
  }
  /**
   * @return string
   */
  public function getAzureLocation()
  {
    return $this->azureLocation;
  }
  /**
   * @param ClientSecretCredentials
   */
  public function setClientSecretCreds(ClientSecretCredentials $clientSecretCreds)
  {
    $this->clientSecretCreds = $clientSecretCreds;
  }
  /**
   * @return ClientSecretCredentials
   */
  public function getClientSecretCreds()
  {
    return $this->clientSecretCreds;
  }
  /**
   * @param Status
   */
  public function setError(Status $error)
  {
    $this->error = $error;
  }
  /**
   * @return Status
   */
  public function getError()
  {
    return $this->error;
  }
  /**
   * @param string[]
   */
  public function setMigrationResourcesUserTags($migrationResourcesUserTags)
  {
    $this->migrationResourcesUserTags = $migrationResourcesUserTags;
  }
  /**
   * @return string[]
   */
  public function getMigrationResourcesUserTags()
  {
    return $this->migrationResourcesUserTags;
  }
  /**
   * @param string
   */
  public function setResourceGroupId($resourceGroupId)
  {
    $this->resourceGroupId = $resourceGroupId;
  }
  /**
   * @return string
   */
  public function getResourceGroupId()
  {
    return $this->resourceGroupId;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AzureSourceDetails::class, 'Google_Service_VMMigrationService_AzureSourceDetails');
