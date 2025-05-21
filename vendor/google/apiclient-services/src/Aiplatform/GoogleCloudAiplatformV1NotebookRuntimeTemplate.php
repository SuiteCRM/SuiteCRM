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

class GoogleCloudAiplatformV1NotebookRuntimeTemplate extends \Google\Collection
{
  protected $collection_key = 'networkTags';
  /**
   * @var string
   */
  public $createTime;
  protected $dataPersistentDiskSpecType = GoogleCloudAiplatformV1PersistentDiskSpec::class;
  protected $dataPersistentDiskSpecDataType = '';
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $displayName;
  protected $encryptionSpecType = GoogleCloudAiplatformV1EncryptionSpec::class;
  protected $encryptionSpecDataType = '';
  /**
   * @var string
   */
  public $etag;
  protected $eucConfigType = GoogleCloudAiplatformV1NotebookEucConfig::class;
  protected $eucConfigDataType = '';
  protected $idleShutdownConfigType = GoogleCloudAiplatformV1NotebookIdleShutdownConfig::class;
  protected $idleShutdownConfigDataType = '';
  /**
   * @var bool
   */
  public $isDefault;
  /**
   * @var string[]
   */
  public $labels;
  protected $machineSpecType = GoogleCloudAiplatformV1MachineSpec::class;
  protected $machineSpecDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $networkSpecType = GoogleCloudAiplatformV1NetworkSpec::class;
  protected $networkSpecDataType = '';
  /**
   * @var string[]
   */
  public $networkTags;
  /**
   * @var string
   */
  public $notebookRuntimeType;
  /**
   * @var string
   */
  public $serviceAccount;
  protected $shieldedVmConfigType = GoogleCloudAiplatformV1ShieldedVmConfig::class;
  protected $shieldedVmConfigDataType = '';
  protected $softwareConfigType = GoogleCloudAiplatformV1NotebookSoftwareConfig::class;
  protected $softwareConfigDataType = '';
  /**
   * @var string
   */
  public $updateTime;

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
   * @param GoogleCloudAiplatformV1PersistentDiskSpec
   */
  public function setDataPersistentDiskSpec(GoogleCloudAiplatformV1PersistentDiskSpec $dataPersistentDiskSpec)
  {
    $this->dataPersistentDiskSpec = $dataPersistentDiskSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1PersistentDiskSpec
   */
  public function getDataPersistentDiskSpec()
  {
    return $this->dataPersistentDiskSpec;
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
   * @param GoogleCloudAiplatformV1NotebookEucConfig
   */
  public function setEucConfig(GoogleCloudAiplatformV1NotebookEucConfig $eucConfig)
  {
    $this->eucConfig = $eucConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1NotebookEucConfig
   */
  public function getEucConfig()
  {
    return $this->eucConfig;
  }
  /**
   * @param GoogleCloudAiplatformV1NotebookIdleShutdownConfig
   */
  public function setIdleShutdownConfig(GoogleCloudAiplatformV1NotebookIdleShutdownConfig $idleShutdownConfig)
  {
    $this->idleShutdownConfig = $idleShutdownConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1NotebookIdleShutdownConfig
   */
  public function getIdleShutdownConfig()
  {
    return $this->idleShutdownConfig;
  }
  /**
   * @param bool
   */
  public function setIsDefault($isDefault)
  {
    $this->isDefault = $isDefault;
  }
  /**
   * @return bool
   */
  public function getIsDefault()
  {
    return $this->isDefault;
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
   * @param GoogleCloudAiplatformV1MachineSpec
   */
  public function setMachineSpec(GoogleCloudAiplatformV1MachineSpec $machineSpec)
  {
    $this->machineSpec = $machineSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1MachineSpec
   */
  public function getMachineSpec()
  {
    return $this->machineSpec;
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
   * @param GoogleCloudAiplatformV1NetworkSpec
   */
  public function setNetworkSpec(GoogleCloudAiplatformV1NetworkSpec $networkSpec)
  {
    $this->networkSpec = $networkSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1NetworkSpec
   */
  public function getNetworkSpec()
  {
    return $this->networkSpec;
  }
  /**
   * @param string[]
   */
  public function setNetworkTags($networkTags)
  {
    $this->networkTags = $networkTags;
  }
  /**
   * @return string[]
   */
  public function getNetworkTags()
  {
    return $this->networkTags;
  }
  /**
   * @param string
   */
  public function setNotebookRuntimeType($notebookRuntimeType)
  {
    $this->notebookRuntimeType = $notebookRuntimeType;
  }
  /**
   * @return string
   */
  public function getNotebookRuntimeType()
  {
    return $this->notebookRuntimeType;
  }
  /**
   * @param string
   */
  public function setServiceAccount($serviceAccount)
  {
    $this->serviceAccount = $serviceAccount;
  }
  /**
   * @return string
   */
  public function getServiceAccount()
  {
    return $this->serviceAccount;
  }
  /**
   * @param GoogleCloudAiplatformV1ShieldedVmConfig
   */
  public function setShieldedVmConfig(GoogleCloudAiplatformV1ShieldedVmConfig $shieldedVmConfig)
  {
    $this->shieldedVmConfig = $shieldedVmConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1ShieldedVmConfig
   */
  public function getShieldedVmConfig()
  {
    return $this->shieldedVmConfig;
  }
  /**
   * @param GoogleCloudAiplatformV1NotebookSoftwareConfig
   */
  public function setSoftwareConfig(GoogleCloudAiplatformV1NotebookSoftwareConfig $softwareConfig)
  {
    $this->softwareConfig = $softwareConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1NotebookSoftwareConfig
   */
  public function getSoftwareConfig()
  {
    return $this->softwareConfig;
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
class_alias(GoogleCloudAiplatformV1NotebookRuntimeTemplate::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1NotebookRuntimeTemplate');
