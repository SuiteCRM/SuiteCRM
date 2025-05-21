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

class GoogleCloudAiplatformV1DeployedIndex extends \Google\Collection
{
  protected $collection_key = 'reservedIpRanges';
  protected $automaticResourcesType = GoogleCloudAiplatformV1AutomaticResources::class;
  protected $automaticResourcesDataType = '';
  /**
   * @var string
   */
  public $createTime;
  protected $dedicatedResourcesType = GoogleCloudAiplatformV1DedicatedResources::class;
  protected $dedicatedResourcesDataType = '';
  protected $deployedIndexAuthConfigType = GoogleCloudAiplatformV1DeployedIndexAuthConfig::class;
  protected $deployedIndexAuthConfigDataType = '';
  /**
   * @var string
   */
  public $deploymentGroup;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var bool
   */
  public $enableAccessLogging;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $index;
  /**
   * @var string
   */
  public $indexSyncTime;
  protected $privateEndpointsType = GoogleCloudAiplatformV1IndexPrivateEndpoints::class;
  protected $privateEndpointsDataType = '';
  protected $pscAutomationConfigsType = GoogleCloudAiplatformV1PSCAutomationConfig::class;
  protected $pscAutomationConfigsDataType = 'array';
  /**
   * @var string[]
   */
  public $reservedIpRanges;

  /**
   * @param GoogleCloudAiplatformV1AutomaticResources
   */
  public function setAutomaticResources(GoogleCloudAiplatformV1AutomaticResources $automaticResources)
  {
    $this->automaticResources = $automaticResources;
  }
  /**
   * @return GoogleCloudAiplatformV1AutomaticResources
   */
  public function getAutomaticResources()
  {
    return $this->automaticResources;
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
   * @param GoogleCloudAiplatformV1DedicatedResources
   */
  public function setDedicatedResources(GoogleCloudAiplatformV1DedicatedResources $dedicatedResources)
  {
    $this->dedicatedResources = $dedicatedResources;
  }
  /**
   * @return GoogleCloudAiplatformV1DedicatedResources
   */
  public function getDedicatedResources()
  {
    return $this->dedicatedResources;
  }
  /**
   * @param GoogleCloudAiplatformV1DeployedIndexAuthConfig
   */
  public function setDeployedIndexAuthConfig(GoogleCloudAiplatformV1DeployedIndexAuthConfig $deployedIndexAuthConfig)
  {
    $this->deployedIndexAuthConfig = $deployedIndexAuthConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1DeployedIndexAuthConfig
   */
  public function getDeployedIndexAuthConfig()
  {
    return $this->deployedIndexAuthConfig;
  }
  /**
   * @param string
   */
  public function setDeploymentGroup($deploymentGroup)
  {
    $this->deploymentGroup = $deploymentGroup;
  }
  /**
   * @return string
   */
  public function getDeploymentGroup()
  {
    return $this->deploymentGroup;
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
  public function setEnableAccessLogging($enableAccessLogging)
  {
    $this->enableAccessLogging = $enableAccessLogging;
  }
  /**
   * @return bool
   */
  public function getEnableAccessLogging()
  {
    return $this->enableAccessLogging;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
  }
  /**
   * @param string
   */
  public function setIndex($index)
  {
    $this->index = $index;
  }
  /**
   * @return string
   */
  public function getIndex()
  {
    return $this->index;
  }
  /**
   * @param string
   */
  public function setIndexSyncTime($indexSyncTime)
  {
    $this->indexSyncTime = $indexSyncTime;
  }
  /**
   * @return string
   */
  public function getIndexSyncTime()
  {
    return $this->indexSyncTime;
  }
  /**
   * @param GoogleCloudAiplatformV1IndexPrivateEndpoints
   */
  public function setPrivateEndpoints(GoogleCloudAiplatformV1IndexPrivateEndpoints $privateEndpoints)
  {
    $this->privateEndpoints = $privateEndpoints;
  }
  /**
   * @return GoogleCloudAiplatformV1IndexPrivateEndpoints
   */
  public function getPrivateEndpoints()
  {
    return $this->privateEndpoints;
  }
  /**
   * @param GoogleCloudAiplatformV1PSCAutomationConfig[]
   */
  public function setPscAutomationConfigs($pscAutomationConfigs)
  {
    $this->pscAutomationConfigs = $pscAutomationConfigs;
  }
  /**
   * @return GoogleCloudAiplatformV1PSCAutomationConfig[]
   */
  public function getPscAutomationConfigs()
  {
    return $this->pscAutomationConfigs;
  }
  /**
   * @param string[]
   */
  public function setReservedIpRanges($reservedIpRanges)
  {
    $this->reservedIpRanges = $reservedIpRanges;
  }
  /**
   * @return string[]
   */
  public function getReservedIpRanges()
  {
    return $this->reservedIpRanges;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1DeployedIndex::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1DeployedIndex');
