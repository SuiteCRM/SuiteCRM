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

namespace Google\Service\Connectors;

class Instance extends \Google\Collection
{
  protected $collection_key = 'provisionedResources';
  /**
   * @var string
   */
  public $consumerDefinedName;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $instanceType;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string[]
   */
  public $maintenancePolicyNames;
  protected $maintenanceSchedulesType = MaintenanceSchedule::class;
  protected $maintenanceSchedulesDataType = 'map';
  protected $maintenanceSettingsType = MaintenanceSettings::class;
  protected $maintenanceSettingsDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $notificationParametersType = NotificationParameter::class;
  protected $notificationParametersDataType = 'map';
  /**
   * @var string[]
   */
  public $producerMetadata;
  protected $provisionedResourcesType = ProvisionedResource::class;
  protected $provisionedResourcesDataType = 'array';
  /**
   * @var string
   */
  public $slmInstanceTemplate;
  protected $sloMetadataType = SloMetadata::class;
  protected $sloMetadataDataType = '';
  /**
   * @var string[]
   */
  public $softwareVersions;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $tenantProjectId;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string
   */
  public function setConsumerDefinedName($consumerDefinedName)
  {
    $this->consumerDefinedName = $consumerDefinedName;
  }
  /**
   * @return string
   */
  public function getConsumerDefinedName()
  {
    return $this->consumerDefinedName;
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
  public function setInstanceType($instanceType)
  {
    $this->instanceType = $instanceType;
  }
  /**
   * @return string
   */
  public function getInstanceType()
  {
    return $this->instanceType;
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
   * @param string[]
   */
  public function setMaintenancePolicyNames($maintenancePolicyNames)
  {
    $this->maintenancePolicyNames = $maintenancePolicyNames;
  }
  /**
   * @return string[]
   */
  public function getMaintenancePolicyNames()
  {
    return $this->maintenancePolicyNames;
  }
  /**
   * @param MaintenanceSchedule[]
   */
  public function setMaintenanceSchedules($maintenanceSchedules)
  {
    $this->maintenanceSchedules = $maintenanceSchedules;
  }
  /**
   * @return MaintenanceSchedule[]
   */
  public function getMaintenanceSchedules()
  {
    return $this->maintenanceSchedules;
  }
  /**
   * @param MaintenanceSettings
   */
  public function setMaintenanceSettings(MaintenanceSettings $maintenanceSettings)
  {
    $this->maintenanceSettings = $maintenanceSettings;
  }
  /**
   * @return MaintenanceSettings
   */
  public function getMaintenanceSettings()
  {
    return $this->maintenanceSettings;
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
   * @param NotificationParameter[]
   */
  public function setNotificationParameters($notificationParameters)
  {
    $this->notificationParameters = $notificationParameters;
  }
  /**
   * @return NotificationParameter[]
   */
  public function getNotificationParameters()
  {
    return $this->notificationParameters;
  }
  /**
   * @param string[]
   */
  public function setProducerMetadata($producerMetadata)
  {
    $this->producerMetadata = $producerMetadata;
  }
  /**
   * @return string[]
   */
  public function getProducerMetadata()
  {
    return $this->producerMetadata;
  }
  /**
   * @param ProvisionedResource[]
   */
  public function setProvisionedResources($provisionedResources)
  {
    $this->provisionedResources = $provisionedResources;
  }
  /**
   * @return ProvisionedResource[]
   */
  public function getProvisionedResources()
  {
    return $this->provisionedResources;
  }
  /**
   * @param string
   */
  public function setSlmInstanceTemplate($slmInstanceTemplate)
  {
    $this->slmInstanceTemplate = $slmInstanceTemplate;
  }
  /**
   * @return string
   */
  public function getSlmInstanceTemplate()
  {
    return $this->slmInstanceTemplate;
  }
  /**
   * @param SloMetadata
   */
  public function setSloMetadata(SloMetadata $sloMetadata)
  {
    $this->sloMetadata = $sloMetadata;
  }
  /**
   * @return SloMetadata
   */
  public function getSloMetadata()
  {
    return $this->sloMetadata;
  }
  /**
   * @param string[]
   */
  public function setSoftwareVersions($softwareVersions)
  {
    $this->softwareVersions = $softwareVersions;
  }
  /**
   * @return string[]
   */
  public function getSoftwareVersions()
  {
    return $this->softwareVersions;
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
  public function setTenantProjectId($tenantProjectId)
  {
    $this->tenantProjectId = $tenantProjectId;
  }
  /**
   * @return string
   */
  public function getTenantProjectId()
  {
    return $this->tenantProjectId;
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
class_alias(Instance::class, 'Google_Service_Connectors_Instance');
