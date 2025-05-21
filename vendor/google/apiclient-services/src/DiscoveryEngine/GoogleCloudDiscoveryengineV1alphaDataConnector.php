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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1alphaDataConnector extends \Google\Collection
{
  protected $collection_key = 'staticIpAddresses';
  protected $actionConfigType = GoogleCloudDiscoveryengineV1alphaActionConfig::class;
  protected $actionConfigDataType = '';
  /**
   * @var bool
   */
  public $autoRunDisabled;
  /**
   * @var string[]
   */
  public $blockingReasons;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $dataSource;
  protected $destinationConfigsType = GoogleCloudDiscoveryengineV1alphaDestinationConfig::class;
  protected $destinationConfigsDataType = 'array';
  protected $entitiesType = GoogleCloudDiscoveryengineV1alphaDataConnectorSourceEntity::class;
  protected $entitiesDataType = 'array';
  protected $errorsType = GoogleRpcStatus::class;
  protected $errorsDataType = 'array';
  /**
   * @var string
   */
  public $identityRefreshInterval;
  protected $identityScheduleConfigType = GoogleCloudDiscoveryengineV1alphaIdentityScheduleConfig::class;
  protected $identityScheduleConfigDataType = '';
  /**
   * @var string
   */
  public $kmsKeyName;
  /**
   * @var string
   */
  public $lastSyncTime;
  /**
   * @var string
   */
  public $latestPauseTime;
  /**
   * @var string
   */
  public $name;
  protected $nextSyncTimeType = GoogleTypeDateTime::class;
  protected $nextSyncTimeDataType = '';
  /**
   * @var array[]
   */
  public $params;
  /**
   * @var string
   */
  public $privateConnectivityProjectId;
  /**
   * @var string
   */
  public $refreshInterval;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string[]
   */
  public $staticIpAddresses;
  /**
   * @var bool
   */
  public $staticIpEnabled;
  /**
   * @var string
   */
  public $syncMode;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param GoogleCloudDiscoveryengineV1alphaActionConfig
   */
  public function setActionConfig(GoogleCloudDiscoveryengineV1alphaActionConfig $actionConfig)
  {
    $this->actionConfig = $actionConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaActionConfig
   */
  public function getActionConfig()
  {
    return $this->actionConfig;
  }
  /**
   * @param bool
   */
  public function setAutoRunDisabled($autoRunDisabled)
  {
    $this->autoRunDisabled = $autoRunDisabled;
  }
  /**
   * @return bool
   */
  public function getAutoRunDisabled()
  {
    return $this->autoRunDisabled;
  }
  /**
   * @param string[]
   */
  public function setBlockingReasons($blockingReasons)
  {
    $this->blockingReasons = $blockingReasons;
  }
  /**
   * @return string[]
   */
  public function getBlockingReasons()
  {
    return $this->blockingReasons;
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
  public function setDataSource($dataSource)
  {
    $this->dataSource = $dataSource;
  }
  /**
   * @return string
   */
  public function getDataSource()
  {
    return $this->dataSource;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaDestinationConfig[]
   */
  public function setDestinationConfigs($destinationConfigs)
  {
    $this->destinationConfigs = $destinationConfigs;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaDestinationConfig[]
   */
  public function getDestinationConfigs()
  {
    return $this->destinationConfigs;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaDataConnectorSourceEntity[]
   */
  public function setEntities($entities)
  {
    $this->entities = $entities;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaDataConnectorSourceEntity[]
   */
  public function getEntities()
  {
    return $this->entities;
  }
  /**
   * @param GoogleRpcStatus[]
   */
  public function setErrors($errors)
  {
    $this->errors = $errors;
  }
  /**
   * @return GoogleRpcStatus[]
   */
  public function getErrors()
  {
    return $this->errors;
  }
  /**
   * @param string
   */
  public function setIdentityRefreshInterval($identityRefreshInterval)
  {
    $this->identityRefreshInterval = $identityRefreshInterval;
  }
  /**
   * @return string
   */
  public function getIdentityRefreshInterval()
  {
    return $this->identityRefreshInterval;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaIdentityScheduleConfig
   */
  public function setIdentityScheduleConfig(GoogleCloudDiscoveryengineV1alphaIdentityScheduleConfig $identityScheduleConfig)
  {
    $this->identityScheduleConfig = $identityScheduleConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaIdentityScheduleConfig
   */
  public function getIdentityScheduleConfig()
  {
    return $this->identityScheduleConfig;
  }
  /**
   * @param string
   */
  public function setKmsKeyName($kmsKeyName)
  {
    $this->kmsKeyName = $kmsKeyName;
  }
  /**
   * @return string
   */
  public function getKmsKeyName()
  {
    return $this->kmsKeyName;
  }
  /**
   * @param string
   */
  public function setLastSyncTime($lastSyncTime)
  {
    $this->lastSyncTime = $lastSyncTime;
  }
  /**
   * @return string
   */
  public function getLastSyncTime()
  {
    return $this->lastSyncTime;
  }
  /**
   * @param string
   */
  public function setLatestPauseTime($latestPauseTime)
  {
    $this->latestPauseTime = $latestPauseTime;
  }
  /**
   * @return string
   */
  public function getLatestPauseTime()
  {
    return $this->latestPauseTime;
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
   * @param GoogleTypeDateTime
   */
  public function setNextSyncTime(GoogleTypeDateTime $nextSyncTime)
  {
    $this->nextSyncTime = $nextSyncTime;
  }
  /**
   * @return GoogleTypeDateTime
   */
  public function getNextSyncTime()
  {
    return $this->nextSyncTime;
  }
  /**
   * @param array[]
   */
  public function setParams($params)
  {
    $this->params = $params;
  }
  /**
   * @return array[]
   */
  public function getParams()
  {
    return $this->params;
  }
  /**
   * @param string
   */
  public function setPrivateConnectivityProjectId($privateConnectivityProjectId)
  {
    $this->privateConnectivityProjectId = $privateConnectivityProjectId;
  }
  /**
   * @return string
   */
  public function getPrivateConnectivityProjectId()
  {
    return $this->privateConnectivityProjectId;
  }
  /**
   * @param string
   */
  public function setRefreshInterval($refreshInterval)
  {
    $this->refreshInterval = $refreshInterval;
  }
  /**
   * @return string
   */
  public function getRefreshInterval()
  {
    return $this->refreshInterval;
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
   * @param string[]
   */
  public function setStaticIpAddresses($staticIpAddresses)
  {
    $this->staticIpAddresses = $staticIpAddresses;
  }
  /**
   * @return string[]
   */
  public function getStaticIpAddresses()
  {
    return $this->staticIpAddresses;
  }
  /**
   * @param bool
   */
  public function setStaticIpEnabled($staticIpEnabled)
  {
    $this->staticIpEnabled = $staticIpEnabled;
  }
  /**
   * @return bool
   */
  public function getStaticIpEnabled()
  {
    return $this->staticIpEnabled;
  }
  /**
   * @param string
   */
  public function setSyncMode($syncMode)
  {
    $this->syncMode = $syncMode;
  }
  /**
   * @return string
   */
  public function getSyncMode()
  {
    return $this->syncMode;
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
class_alias(GoogleCloudDiscoveryengineV1alphaDataConnector::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1alphaDataConnector');
