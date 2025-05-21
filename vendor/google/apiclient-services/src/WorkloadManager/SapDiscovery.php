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

namespace Google\Service\WorkloadManager;

class SapDiscovery extends \Google\Model
{
  protected $applicationLayerType = SapDiscoveryComponent::class;
  protected $applicationLayerDataType = '';
  protected $databaseLayerType = SapDiscoveryComponent::class;
  protected $databaseLayerDataType = '';
  protected $metadataType = SapDiscoveryMetadata::class;
  protected $metadataDataType = '';
  /**
   * @var string
   */
  public $projectNumber;
  /**
   * @var string
   */
  public $systemId;
  /**
   * @var string
   */
  public $updateTime;
  /**
   * @var bool
   */
  public $useDrReconciliation;
  protected $workloadPropertiesType = SapDiscoveryWorkloadProperties::class;
  protected $workloadPropertiesDataType = '';

  /**
   * @param SapDiscoveryComponent
   */
  public function setApplicationLayer(SapDiscoveryComponent $applicationLayer)
  {
    $this->applicationLayer = $applicationLayer;
  }
  /**
   * @return SapDiscoveryComponent
   */
  public function getApplicationLayer()
  {
    return $this->applicationLayer;
  }
  /**
   * @param SapDiscoveryComponent
   */
  public function setDatabaseLayer(SapDiscoveryComponent $databaseLayer)
  {
    $this->databaseLayer = $databaseLayer;
  }
  /**
   * @return SapDiscoveryComponent
   */
  public function getDatabaseLayer()
  {
    return $this->databaseLayer;
  }
  /**
   * @param SapDiscoveryMetadata
   */
  public function setMetadata(SapDiscoveryMetadata $metadata)
  {
    $this->metadata = $metadata;
  }
  /**
   * @return SapDiscoveryMetadata
   */
  public function getMetadata()
  {
    return $this->metadata;
  }
  /**
   * @param string
   */
  public function setProjectNumber($projectNumber)
  {
    $this->projectNumber = $projectNumber;
  }
  /**
   * @return string
   */
  public function getProjectNumber()
  {
    return $this->projectNumber;
  }
  /**
   * @param string
   */
  public function setSystemId($systemId)
  {
    $this->systemId = $systemId;
  }
  /**
   * @return string
   */
  public function getSystemId()
  {
    return $this->systemId;
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
  /**
   * @param bool
   */
  public function setUseDrReconciliation($useDrReconciliation)
  {
    $this->useDrReconciliation = $useDrReconciliation;
  }
  /**
   * @return bool
   */
  public function getUseDrReconciliation()
  {
    return $this->useDrReconciliation;
  }
  /**
   * @param SapDiscoveryWorkloadProperties
   */
  public function setWorkloadProperties(SapDiscoveryWorkloadProperties $workloadProperties)
  {
    $this->workloadProperties = $workloadProperties;
  }
  /**
   * @return SapDiscoveryWorkloadProperties
   */
  public function getWorkloadProperties()
  {
    return $this->workloadProperties;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SapDiscovery::class, 'Google_Service_WorkloadManager_SapDiscovery');
