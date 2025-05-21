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

namespace Google\Service\CloudAlloyDBAdmin;

class StorageDatabasecenterPartnerapiV1mainDatabaseResourceMetadata extends \Google\Collection
{
  protected $collection_key = 'entitlements';
  protected $availabilityConfigurationType = StorageDatabasecenterPartnerapiV1mainAvailabilityConfiguration::class;
  protected $availabilityConfigurationDataType = '';
  protected $backupConfigurationType = StorageDatabasecenterPartnerapiV1mainBackupConfiguration::class;
  protected $backupConfigurationDataType = '';
  protected $backupRunType = StorageDatabasecenterPartnerapiV1mainBackupRun::class;
  protected $backupRunDataType = '';
  /**
   * @var string
   */
  public $creationTime;
  /**
   * @var string
   */
  public $currentState;
  protected $customMetadataType = StorageDatabasecenterPartnerapiV1mainCustomMetadataData::class;
  protected $customMetadataDataType = '';
  /**
   * @var string
   */
  public $edition;
  protected $entitlementsType = StorageDatabasecenterPartnerapiV1mainEntitlement::class;
  protected $entitlementsDataType = 'array';
  /**
   * @var string
   */
  public $expectedState;
  protected $idType = StorageDatabasecenterPartnerapiV1mainDatabaseResourceId::class;
  protected $idDataType = '';
  /**
   * @var string
   */
  public $instanceType;
  /**
   * @var string
   */
  public $location;
  protected $machineConfigurationType = StorageDatabasecenterPartnerapiV1mainMachineConfiguration::class;
  protected $machineConfigurationDataType = '';
  protected $primaryResourceIdType = StorageDatabasecenterPartnerapiV1mainDatabaseResourceId::class;
  protected $primaryResourceIdDataType = '';
  /**
   * @var string
   */
  public $primaryResourceLocation;
  protected $productType = StorageDatabasecenterProtoCommonProduct::class;
  protected $productDataType = '';
  /**
   * @var string
   */
  public $resourceContainer;
  /**
   * @var string
   */
  public $resourceName;
  /**
   * @var string
   */
  public $suspensionReason;
  protected $tagsSetType = StorageDatabasecenterPartnerapiV1mainTags::class;
  protected $tagsSetDataType = '';
  /**
   * @var string
   */
  public $updationTime;
  protected $userLabelSetType = StorageDatabasecenterPartnerapiV1mainUserLabels::class;
  protected $userLabelSetDataType = '';

  /**
   * @param StorageDatabasecenterPartnerapiV1mainAvailabilityConfiguration
   */
  public function setAvailabilityConfiguration(StorageDatabasecenterPartnerapiV1mainAvailabilityConfiguration $availabilityConfiguration)
  {
    $this->availabilityConfiguration = $availabilityConfiguration;
  }
  /**
   * @return StorageDatabasecenterPartnerapiV1mainAvailabilityConfiguration
   */
  public function getAvailabilityConfiguration()
  {
    return $this->availabilityConfiguration;
  }
  /**
   * @param StorageDatabasecenterPartnerapiV1mainBackupConfiguration
   */
  public function setBackupConfiguration(StorageDatabasecenterPartnerapiV1mainBackupConfiguration $backupConfiguration)
  {
    $this->backupConfiguration = $backupConfiguration;
  }
  /**
   * @return StorageDatabasecenterPartnerapiV1mainBackupConfiguration
   */
  public function getBackupConfiguration()
  {
    return $this->backupConfiguration;
  }
  /**
   * @param StorageDatabasecenterPartnerapiV1mainBackupRun
   */
  public function setBackupRun(StorageDatabasecenterPartnerapiV1mainBackupRun $backupRun)
  {
    $this->backupRun = $backupRun;
  }
  /**
   * @return StorageDatabasecenterPartnerapiV1mainBackupRun
   */
  public function getBackupRun()
  {
    return $this->backupRun;
  }
  /**
   * @param string
   */
  public function setCreationTime($creationTime)
  {
    $this->creationTime = $creationTime;
  }
  /**
   * @return string
   */
  public function getCreationTime()
  {
    return $this->creationTime;
  }
  /**
   * @param string
   */
  public function setCurrentState($currentState)
  {
    $this->currentState = $currentState;
  }
  /**
   * @return string
   */
  public function getCurrentState()
  {
    return $this->currentState;
  }
  /**
   * @param StorageDatabasecenterPartnerapiV1mainCustomMetadataData
   */
  public function setCustomMetadata(StorageDatabasecenterPartnerapiV1mainCustomMetadataData $customMetadata)
  {
    $this->customMetadata = $customMetadata;
  }
  /**
   * @return StorageDatabasecenterPartnerapiV1mainCustomMetadataData
   */
  public function getCustomMetadata()
  {
    return $this->customMetadata;
  }
  /**
   * @param string
   */
  public function setEdition($edition)
  {
    $this->edition = $edition;
  }
  /**
   * @return string
   */
  public function getEdition()
  {
    return $this->edition;
  }
  /**
   * @param StorageDatabasecenterPartnerapiV1mainEntitlement[]
   */
  public function setEntitlements($entitlements)
  {
    $this->entitlements = $entitlements;
  }
  /**
   * @return StorageDatabasecenterPartnerapiV1mainEntitlement[]
   */
  public function getEntitlements()
  {
    return $this->entitlements;
  }
  /**
   * @param string
   */
  public function setExpectedState($expectedState)
  {
    $this->expectedState = $expectedState;
  }
  /**
   * @return string
   */
  public function getExpectedState()
  {
    return $this->expectedState;
  }
  /**
   * @param StorageDatabasecenterPartnerapiV1mainDatabaseResourceId
   */
  public function setId(StorageDatabasecenterPartnerapiV1mainDatabaseResourceId $id)
  {
    $this->id = $id;
  }
  /**
   * @return StorageDatabasecenterPartnerapiV1mainDatabaseResourceId
   */
  public function getId()
  {
    return $this->id;
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
   * @param string
   */
  public function setLocation($location)
  {
    $this->location = $location;
  }
  /**
   * @return string
   */
  public function getLocation()
  {
    return $this->location;
  }
  /**
   * @param StorageDatabasecenterPartnerapiV1mainMachineConfiguration
   */
  public function setMachineConfiguration(StorageDatabasecenterPartnerapiV1mainMachineConfiguration $machineConfiguration)
  {
    $this->machineConfiguration = $machineConfiguration;
  }
  /**
   * @return StorageDatabasecenterPartnerapiV1mainMachineConfiguration
   */
  public function getMachineConfiguration()
  {
    return $this->machineConfiguration;
  }
  /**
   * @param StorageDatabasecenterPartnerapiV1mainDatabaseResourceId
   */
  public function setPrimaryResourceId(StorageDatabasecenterPartnerapiV1mainDatabaseResourceId $primaryResourceId)
  {
    $this->primaryResourceId = $primaryResourceId;
  }
  /**
   * @return StorageDatabasecenterPartnerapiV1mainDatabaseResourceId
   */
  public function getPrimaryResourceId()
  {
    return $this->primaryResourceId;
  }
  /**
   * @param string
   */
  public function setPrimaryResourceLocation($primaryResourceLocation)
  {
    $this->primaryResourceLocation = $primaryResourceLocation;
  }
  /**
   * @return string
   */
  public function getPrimaryResourceLocation()
  {
    return $this->primaryResourceLocation;
  }
  /**
   * @param StorageDatabasecenterProtoCommonProduct
   */
  public function setProduct(StorageDatabasecenterProtoCommonProduct $product)
  {
    $this->product = $product;
  }
  /**
   * @return StorageDatabasecenterProtoCommonProduct
   */
  public function getProduct()
  {
    return $this->product;
  }
  /**
   * @param string
   */
  public function setResourceContainer($resourceContainer)
  {
    $this->resourceContainer = $resourceContainer;
  }
  /**
   * @return string
   */
  public function getResourceContainer()
  {
    return $this->resourceContainer;
  }
  /**
   * @param string
   */
  public function setResourceName($resourceName)
  {
    $this->resourceName = $resourceName;
  }
  /**
   * @return string
   */
  public function getResourceName()
  {
    return $this->resourceName;
  }
  /**
   * @param string
   */
  public function setSuspensionReason($suspensionReason)
  {
    $this->suspensionReason = $suspensionReason;
  }
  /**
   * @return string
   */
  public function getSuspensionReason()
  {
    return $this->suspensionReason;
  }
  /**
   * @param StorageDatabasecenterPartnerapiV1mainTags
   */
  public function setTagsSet(StorageDatabasecenterPartnerapiV1mainTags $tagsSet)
  {
    $this->tagsSet = $tagsSet;
  }
  /**
   * @return StorageDatabasecenterPartnerapiV1mainTags
   */
  public function getTagsSet()
  {
    return $this->tagsSet;
  }
  /**
   * @param string
   */
  public function setUpdationTime($updationTime)
  {
    $this->updationTime = $updationTime;
  }
  /**
   * @return string
   */
  public function getUpdationTime()
  {
    return $this->updationTime;
  }
  /**
   * @param StorageDatabasecenterPartnerapiV1mainUserLabels
   */
  public function setUserLabelSet(StorageDatabasecenterPartnerapiV1mainUserLabels $userLabelSet)
  {
    $this->userLabelSet = $userLabelSet;
  }
  /**
   * @return StorageDatabasecenterPartnerapiV1mainUserLabels
   */
  public function getUserLabelSet()
  {
    return $this->userLabelSet;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(StorageDatabasecenterPartnerapiV1mainDatabaseResourceMetadata::class, 'Google_Service_CloudAlloyDBAdmin_StorageDatabasecenterPartnerapiV1mainDatabaseResourceMetadata');
