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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1Endpoint extends \Google\Collection
{
  protected $collection_key = 'deployedModels';
  protected $clientConnectionConfigType = GoogleCloudAiplatformV1ClientConnectionConfig::class;
  protected $clientConnectionConfigDataType = '';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $dedicatedEndpointDns;
  /**
   * @var bool
   */
  public $dedicatedEndpointEnabled;
  protected $deployedModelsType = GoogleCloudAiplatformV1DeployedModel::class;
  protected $deployedModelsDataType = 'array';
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var bool
   */
  public $enablePrivateServiceConnect;
  protected $encryptionSpecType = GoogleCloudAiplatformV1EncryptionSpec::class;
  protected $encryptionSpecDataType = '';
  /**
   * @var string
   */
  public $etag;
  protected $genAiAdvancedFeaturesConfigType = GoogleCloudAiplatformV1GenAiAdvancedFeaturesConfig::class;
  protected $genAiAdvancedFeaturesConfigDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $modelDeploymentMonitoringJob;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $network;
  protected $predictRequestResponseLoggingConfigType = GoogleCloudAiplatformV1PredictRequestResponseLoggingConfig::class;
  protected $predictRequestResponseLoggingConfigDataType = '';
  protected $privateServiceConnectConfigType = GoogleCloudAiplatformV1PrivateServiceConnectConfig::class;
  protected $privateServiceConnectConfigDataType = '';
  /**
   * @var bool
   */
  public $satisfiesPzi;
  /**
   * @var bool
   */
  public $satisfiesPzs;
  /**
   * @var int[]
   */
  public $trafficSplit;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param GoogleCloudAiplatformV1ClientConnectionConfig
   */
  public function setClientConnectionConfig(GoogleCloudAiplatformV1ClientConnectionConfig $clientConnectionConfig)
  {
    $this->clientConnectionConfig = $clientConnectionConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1ClientConnectionConfig
   */
  public function getClientConnectionConfig()
  {
    return $this->clientConnectionConfig;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setDedicatedEndpointDns($dedicatedEndpointDns)
  {
    $this->dedicatedEndpointDns = $dedicatedEndpointDns;
  }
  /**
   * @return string
   */
  public function getDedicatedEndpointDns()
  {
    return $this->dedicatedEndpointDns;
  }
  /**
   * @param bool
   */
  public function setDedicatedEndpointEnabled($dedicatedEndpointEnabled)
  {
    $this->dedicatedEndpointEnabled = $dedicatedEndpointEnabled;
  }
  /**
   * @return bool
   */
  public function getDedicatedEndpointEnabled()
  {
    return $this->dedicatedEndpointEnabled;
  }
  /**
   * @param GoogleCloudAiplatformV1DeployedModel[]
   */
  public function setDeployedModels($deployedModels)
  {
    $this->deployedModels = $deployedModels;
  }
  /**
   * @return GoogleCloudAiplatformV1DeployedModel[]
   */
  public function getDeployedModels()
  {
    return $this->deployedModels;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param bool
   */
  public function setEnablePrivateServiceConnect($enablePrivateServiceConnect)
  {
    $this->enablePrivateServiceConnect = $enablePrivateServiceConnect;
  }
  /**
   * @return bool
   */
  public function getEnablePrivateServiceConnect()
  {
    return $this->enablePrivateServiceConnect;
  }
  /**
   * @param GoogleCloudAiplatformV1EncryptionSpec
   */
  public function setEncryptionSpec(GoogleCloudAiplatformV1EncryptionSpec $encryptionSpec)
  {
    $this->encryptionSpec = $encryptionSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1EncryptionSpec
   */
  public function getEncryptionSpec()
  {
    return $this->encryptionSpec;
  }
  /**
   * @param string
   */
  public function setEtag($etag)
  {
    $this->etag = $etag;
  }
  /**
   * @return string
   */
  public function getEtag()
  {
    return $this->etag;
  }
  /**
   * @param GoogleCloudAiplatformV1GenAiAdvancedFeaturesConfig
   */
  public function setGenAiAdvancedFeaturesConfig(GoogleCloudAiplatformV1GenAiAdvancedFeaturesConfig $genAiAdvancedFeaturesConfig)
  {
    $this->genAiAdvancedFeaturesConfig = $genAiAdvancedFeaturesConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1GenAiAdvancedFeaturesConfig
   */
  public function getGenAiAdvancedFeaturesConfig()
  {
    return $this->genAiAdvancedFeaturesConfig;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
  }
  /**
   * @param string
   */
  public function setModelDeploymentMonitoringJob($modelDeploymentMonitoringJob)
  {
    $this->modelDeploymentMonitoringJob = $modelDeploymentMonitoringJob;
  }
  /**
   * @return string
   */
  public function getModelDeploymentMonitoringJob()
  {
    return $this->modelDeploymentMonitoringJob;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setNetwork($network)
  {
    $this->network = $network;
  }
  /**
   * @return string
   */
  public function getNetwork()
  {
    return $this->network;
  }
  /**
   * @param GoogleCloudAiplatformV1PredictRequestResponseLoggingConfig
   */
  public function setPredictRequestResponseLoggingConfig(GoogleCloudAiplatformV1PredictRequestResponseLoggingConfig $predictRequestResponseLoggingConfig)
  {
    $this->predictRequestResponseLoggingConfig = $predictRequestResponseLoggingConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1PredictRequestResponseLoggingConfig
   */
  public function getPredictRequestResponseLoggingConfig()
  {
    return $this->predictRequestResponseLoggingConfig;
  }
  /**
   * @param GoogleCloudAiplatformV1PrivateServiceConnectConfig
   */
  public function setPrivateServiceConnectConfig(GoogleCloudAiplatformV1PrivateServiceConnectConfig $privateServiceConnectConfig)
  {
    $this->privateServiceConnectConfig = $privateServiceConnectConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1PrivateServiceConnectConfig
   */
  public function getPrivateServiceConnectConfig()
  {
    return $this->privateServiceConnectConfig;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzi($satisfiesPzi)
  {
    $this->satisfiesPzi = $satisfiesPzi;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzi()
  {
    return $this->satisfiesPzi;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzs($satisfiesPzs)
  {
    $this->satisfiesPzs = $satisfiesPzs;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzs()
  {
    return $this->satisfiesPzs;
  }
  /**
   * @param int[]
   */
  public function setTrafficSplit($trafficSplit)
  {
    $this->trafficSplit = $trafficSplit;
  }
  /**
   * @return int[]
   */
  public function getTrafficSplit()
  {
    return $this->trafficSplit;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1Endpoint::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1Endpoint');
