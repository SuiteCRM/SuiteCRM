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

class GoogleCloudConnectorsV1ConnectorVersionInfraConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $connectionRatelimitWindowSeconds;
  /**
   * @var string
   */
  public $deploymentModel;
  /**
   * @var string
   */
  public $deploymentModelMigrationState;
  protected $hpaConfigType = GoogleCloudConnectorsV1HPAConfig::class;
  protected $hpaConfigDataType = '';
  /**
   * @var string
   */
  public $internalclientRatelimitThreshold;
  /**
   * @var int
   */
  public $maxInstanceRequestConcurrency;
  /**
   * @var string
   */
  public $ratelimitThreshold;
  protected $resourceLimitsType = GoogleCloudConnectorsV1ResourceLimits::class;
  protected $resourceLimitsDataType = '';
  protected $resourceRequestsType = GoogleCloudConnectorsV1ResourceRequests::class;
  protected $resourceRequestsDataType = '';
  /**
   * @var string
   */
  public $sharedDeployment;
  /**
   * @var string
   */
  public $tlsMigrationState;

  /**
   * @param string
   */
  public function setConnectionRatelimitWindowSeconds($connectionRatelimitWindowSeconds)
  {
    $this->connectionRatelimitWindowSeconds = $connectionRatelimitWindowSeconds;
  }
  /**
   * @return string
   */
  public function getConnectionRatelimitWindowSeconds()
  {
    return $this->connectionRatelimitWindowSeconds;
  }
  /**
   * @param string
   */
  public function setDeploymentModel($deploymentModel)
  {
    $this->deploymentModel = $deploymentModel;
  }
  /**
   * @return string
   */
  public function getDeploymentModel()
  {
    return $this->deploymentModel;
  }
  /**
   * @param string
   */
  public function setDeploymentModelMigrationState($deploymentModelMigrationState)
  {
    $this->deploymentModelMigrationState = $deploymentModelMigrationState;
  }
  /**
   * @return string
   */
  public function getDeploymentModelMigrationState()
  {
    return $this->deploymentModelMigrationState;
  }
  /**
   * @param GoogleCloudConnectorsV1HPAConfig
   */
  public function setHpaConfig(GoogleCloudConnectorsV1HPAConfig $hpaConfig)
  {
    $this->hpaConfig = $hpaConfig;
  }
  /**
   * @return GoogleCloudConnectorsV1HPAConfig
   */
  public function getHpaConfig()
  {
    return $this->hpaConfig;
  }
  /**
   * @param string
   */
  public function setInternalclientRatelimitThreshold($internalclientRatelimitThreshold)
  {
    $this->internalclientRatelimitThreshold = $internalclientRatelimitThreshold;
  }
  /**
   * @return string
   */
  public function getInternalclientRatelimitThreshold()
  {
    return $this->internalclientRatelimitThreshold;
  }
  /**
   * @param int
   */
  public function setMaxInstanceRequestConcurrency($maxInstanceRequestConcurrency)
  {
    $this->maxInstanceRequestConcurrency = $maxInstanceRequestConcurrency;
  }
  /**
   * @return int
   */
  public function getMaxInstanceRequestConcurrency()
  {
    return $this->maxInstanceRequestConcurrency;
  }
  /**
   * @param string
   */
  public function setRatelimitThreshold($ratelimitThreshold)
  {
    $this->ratelimitThreshold = $ratelimitThreshold;
  }
  /**
   * @return string
   */
  public function getRatelimitThreshold()
  {
    return $this->ratelimitThreshold;
  }
  /**
   * @param GoogleCloudConnectorsV1ResourceLimits
   */
  public function setResourceLimits(GoogleCloudConnectorsV1ResourceLimits $resourceLimits)
  {
    $this->resourceLimits = $resourceLimits;
  }
  /**
   * @return GoogleCloudConnectorsV1ResourceLimits
   */
  public function getResourceLimits()
  {
    return $this->resourceLimits;
  }
  /**
   * @param GoogleCloudConnectorsV1ResourceRequests
   */
  public function setResourceRequests(GoogleCloudConnectorsV1ResourceRequests $resourceRequests)
  {
    $this->resourceRequests = $resourceRequests;
  }
  /**
   * @return GoogleCloudConnectorsV1ResourceRequests
   */
  public function getResourceRequests()
  {
    return $this->resourceRequests;
  }
  /**
   * @param string
   */
  public function setSharedDeployment($sharedDeployment)
  {
    $this->sharedDeployment = $sharedDeployment;
  }
  /**
   * @return string
   */
  public function getSharedDeployment()
  {
    return $this->sharedDeployment;
  }
  /**
   * @param string
   */
  public function setTlsMigrationState($tlsMigrationState)
  {
    $this->tlsMigrationState = $tlsMigrationState;
  }
  /**
   * @return string
   */
  public function getTlsMigrationState()
  {
    return $this->tlsMigrationState;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudConnectorsV1ConnectorVersionInfraConfig::class, 'Google_Service_Integrations_GoogleCloudConnectorsV1ConnectorVersionInfraConfig');
