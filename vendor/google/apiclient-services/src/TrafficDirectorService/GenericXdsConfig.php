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

namespace Google\Service\TrafficDirectorService;

class GenericXdsConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $clientStatus;
  /**
   * @var string
   */
  public $configStatus;
  protected $errorStateType = UpdateFailureState::class;
  protected $errorStateDataType = '';
  /**
   * @var bool
   */
  public $isStaticResource;
  /**
   * @var string
   */
  public $lastUpdated;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $typeUrl;
  /**
   * @var string
   */
  public $versionInfo;
  /**
   * @var array[]
   */
  public $xdsConfig;

  /**
   * @param string
   */
  public function setClientStatus($clientStatus)
  {
    $this->clientStatus = $clientStatus;
  }
  /**
   * @return string
   */
  public function getClientStatus()
  {
    return $this->clientStatus;
  }
  /**
   * @param string
   */
  public function setConfigStatus($configStatus)
  {
    $this->configStatus = $configStatus;
  }
  /**
   * @return string
   */
  public function getConfigStatus()
  {
    return $this->configStatus;
  }
  /**
   * @param UpdateFailureState
   */
  public function setErrorState(UpdateFailureState $errorState)
  {
    $this->errorState = $errorState;
  }
  /**
   * @return UpdateFailureState
   */
  public function getErrorState()
  {
    return $this->errorState;
  }
  /**
   * @param bool
   */
  public function setIsStaticResource($isStaticResource)
  {
    $this->isStaticResource = $isStaticResource;
  }
  /**
   * @return bool
   */
  public function getIsStaticResource()
  {
    return $this->isStaticResource;
  }
  /**
   * @param string
   */
  public function setLastUpdated($lastUpdated)
  {
    $this->lastUpdated = $lastUpdated;
  }
  /**
   * @return string
   */
  public function getLastUpdated()
  {
    return $this->lastUpdated;
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
  public function setTypeUrl($typeUrl)
  {
    $this->typeUrl = $typeUrl;
  }
  /**
   * @return string
   */
  public function getTypeUrl()
  {
    return $this->typeUrl;
  }
  /**
   * @param string
   */
  public function setVersionInfo($versionInfo)
  {
    $this->versionInfo = $versionInfo;
  }
  /**
   * @return string
   */
  public function getVersionInfo()
  {
    return $this->versionInfo;
  }
  /**
   * @param array[]
   */
  public function setXdsConfig($xdsConfig)
  {
    $this->xdsConfig = $xdsConfig;
  }
  /**
   * @return array[]
   */
  public function getXdsConfig()
  {
    return $this->xdsConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GenericXdsConfig::class, 'Google_Service_TrafficDirectorService_GenericXdsConfig');
