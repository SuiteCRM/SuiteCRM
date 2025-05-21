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

namespace Google\Service\Container;

class NodeConfig extends \Google\Collection
{
  protected $collection_key = 'taints';
  protected $acceleratorsType = AcceleratorConfig::class;
  protected $acceleratorsDataType = 'array';
  protected $advancedMachineFeaturesType = AdvancedMachineFeatures::class;
  protected $advancedMachineFeaturesDataType = '';
  /**
   * @var string
   */
  public $bootDiskKmsKey;
  protected $confidentialNodesType = ConfidentialNodes::class;
  protected $confidentialNodesDataType = '';
  protected $containerdConfigType = ContainerdConfig::class;
  protected $containerdConfigDataType = '';
  /**
   * @var int
   */
  public $diskSizeGb;
  /**
   * @var string
   */
  public $diskType;
  /**
   * @var string
   */
  public $effectiveCgroupMode;
  /**
   * @var bool
   */
  public $enableConfidentialStorage;
  protected $ephemeralStorageLocalSsdConfigType = EphemeralStorageLocalSsdConfig::class;
  protected $ephemeralStorageLocalSsdConfigDataType = '';
  protected $fastSocketType = FastSocket::class;
  protected $fastSocketDataType = '';
  protected $gcfsConfigType = GcfsConfig::class;
  protected $gcfsConfigDataType = '';
  protected $gvnicType = VirtualNIC::class;
  protected $gvnicDataType = '';
  /**
   * @var string
   */
  public $imageType;
  protected $kubeletConfigType = NodeKubeletConfig::class;
  protected $kubeletConfigDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  protected $linuxNodeConfigType = LinuxNodeConfig::class;
  protected $linuxNodeConfigDataType = '';
  protected $localNvmeSsdBlockConfigType = LocalNvmeSsdBlockConfig::class;
  protected $localNvmeSsdBlockConfigDataType = '';
  /**
   * @var int
   */
  public $localSsdCount;
  /**
   * @var string
   */
  public $localSsdEncryptionMode;
  protected $loggingConfigType = NodePoolLoggingConfig::class;
  protected $loggingConfigDataType = '';
  /**
   * @var string
   */
  public $machineType;
  /**
   * @var string
   */
  public $maxRunDuration;
  /**
   * @var string[]
   */
  public $metadata;
  /**
   * @var string
   */
  public $minCpuPlatform;
  /**
   * @var string
   */
  public $nodeGroup;
  /**
   * @var string[]
   */
  public $oauthScopes;
  /**
   * @var bool
   */
  public $preemptible;
  protected $reservationAffinityType = ReservationAffinity::class;
  protected $reservationAffinityDataType = '';
  /**
   * @var string[]
   */
  public $resourceLabels;
  protected $resourceManagerTagsType = ResourceManagerTags::class;
  protected $resourceManagerTagsDataType = '';
  protected $sandboxConfigType = SandboxConfig::class;
  protected $sandboxConfigDataType = '';
  protected $secondaryBootDiskUpdateStrategyType = SecondaryBootDiskUpdateStrategy::class;
  protected $secondaryBootDiskUpdateStrategyDataType = '';
  protected $secondaryBootDisksType = SecondaryBootDisk::class;
  protected $secondaryBootDisksDataType = 'array';
  /**
   * @var string
   */
  public $serviceAccount;
  protected $shieldedInstanceConfigType = ShieldedInstanceConfig::class;
  protected $shieldedInstanceConfigDataType = '';
  protected $soleTenantConfigType = SoleTenantConfig::class;
  protected $soleTenantConfigDataType = '';
  /**
   * @var bool
   */
  public $spot;
  /**
   * @var string[]
   */
  public $storagePools;
  /**
   * @var string[]
   */
  public $tags;
  protected $taintsType = NodeTaint::class;
  protected $taintsDataType = 'array';
  protected $windowsNodeConfigType = WindowsNodeConfig::class;
  protected $windowsNodeConfigDataType = '';
  protected $workloadMetadataConfigType = WorkloadMetadataConfig::class;
  protected $workloadMetadataConfigDataType = '';

  /**
   * @param AcceleratorConfig[]
   */
  public function setAccelerators($accelerators)
  {
    $this->accelerators = $accelerators;
  }
  /**
   * @return AcceleratorConfig[]
   */
  public function getAccelerators()
  {
    return $this->accelerators;
  }
  /**
   * @param AdvancedMachineFeatures
   */
  public function setAdvancedMachineFeatures(AdvancedMachineFeatures $advancedMachineFeatures)
  {
    $this->advancedMachineFeatures = $advancedMachineFeatures;
  }
  /**
   * @return AdvancedMachineFeatures
   */
  public function getAdvancedMachineFeatures()
  {
    return $this->advancedMachineFeatures;
  }
  /**
   * @param string
   */
  public function setBootDiskKmsKey($bootDiskKmsKey)
  {
    $this->bootDiskKmsKey = $bootDiskKmsKey;
  }
  /**
   * @return string
   */
  public function getBootDiskKmsKey()
  {
    return $this->bootDiskKmsKey;
  }
  /**
   * @param ConfidentialNodes
   */
  public function setConfidentialNodes(ConfidentialNodes $confidentialNodes)
  {
    $this->confidentialNodes = $confidentialNodes;
  }
  /**
   * @return ConfidentialNodes
   */
  public function getConfidentialNodes()
  {
    return $this->confidentialNodes;
  }
  /**
   * @param ContainerdConfig
   */
  public function setContainerdConfig(ContainerdConfig $containerdConfig)
  {
    $this->containerdConfig = $containerdConfig;
  }
  /**
   * @return ContainerdConfig
   */
  public function getContainerdConfig()
  {
    return $this->containerdConfig;
  }
  /**
   * @param int
   */
  public function setDiskSizeGb($diskSizeGb)
  {
    $this->diskSizeGb = $diskSizeGb;
  }
  /**
   * @return int
   */
  public function getDiskSizeGb()
  {
    return $this->diskSizeGb;
  }
  /**
   * @param string
   */
  public function setDiskType($diskType)
  {
    $this->diskType = $diskType;
  }
  /**
   * @return string
   */
  public function getDiskType()
  {
    return $this->diskType;
  }
  /**
   * @param string
   */
  public function setEffectiveCgroupMode($effectiveCgroupMode)
  {
    $this->effectiveCgroupMode = $effectiveCgroupMode;
  }
  /**
   * @return string
   */
  public function getEffectiveCgroupMode()
  {
    return $this->effectiveCgroupMode;
  }
  /**
   * @param bool
   */
  public function setEnableConfidentialStorage($enableConfidentialStorage)
  {
    $this->enableConfidentialStorage = $enableConfidentialStorage;
  }
  /**
   * @return bool
   */
  public function getEnableConfidentialStorage()
  {
    return $this->enableConfidentialStorage;
  }
  /**
   * @param EphemeralStorageLocalSsdConfig
   */
  public function setEphemeralStorageLocalSsdConfig(EphemeralStorageLocalSsdConfig $ephemeralStorageLocalSsdConfig)
  {
    $this->ephemeralStorageLocalSsdConfig = $ephemeralStorageLocalSsdConfig;
  }
  /**
   * @return EphemeralStorageLocalSsdConfig
   */
  public function getEphemeralStorageLocalSsdConfig()
  {
    return $this->ephemeralStorageLocalSsdConfig;
  }
  /**
   * @param FastSocket
   */
  public function setFastSocket(FastSocket $fastSocket)
  {
    $this->fastSocket = $fastSocket;
  }
  /**
   * @return FastSocket
   */
  public function getFastSocket()
  {
    return $this->fastSocket;
  }
  /**
   * @param GcfsConfig
   */
  public function setGcfsConfig(GcfsConfig $gcfsConfig)
  {
    $this->gcfsConfig = $gcfsConfig;
  }
  /**
   * @return GcfsConfig
   */
  public function getGcfsConfig()
  {
    return $this->gcfsConfig;
  }
  /**
   * @param VirtualNIC
   */
  public function setGvnic(VirtualNIC $gvnic)
  {
    $this->gvnic = $gvnic;
  }
  /**
   * @return VirtualNIC
   */
  public function getGvnic()
  {
    return $this->gvnic;
  }
  /**
   * @param string
   */
  public function setImageType($imageType)
  {
    $this->imageType = $imageType;
  }
  /**
   * @return string
   */
  public function getImageType()
  {
    return $this->imageType;
  }
  /**
   * @param NodeKubeletConfig
   */
  public function setKubeletConfig(NodeKubeletConfig $kubeletConfig)
  {
    $this->kubeletConfig = $kubeletConfig;
  }
  /**
   * @return NodeKubeletConfig
   */
  public function getKubeletConfig()
  {
    return $this->kubeletConfig;
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
   * @param LinuxNodeConfig
   */
  public function setLinuxNodeConfig(LinuxNodeConfig $linuxNodeConfig)
  {
    $this->linuxNodeConfig = $linuxNodeConfig;
  }
  /**
   * @return LinuxNodeConfig
   */
  public function getLinuxNodeConfig()
  {
    return $this->linuxNodeConfig;
  }
  /**
   * @param LocalNvmeSsdBlockConfig
   */
  public function setLocalNvmeSsdBlockConfig(LocalNvmeSsdBlockConfig $localNvmeSsdBlockConfig)
  {
    $this->localNvmeSsdBlockConfig = $localNvmeSsdBlockConfig;
  }
  /**
   * @return LocalNvmeSsdBlockConfig
   */
  public function getLocalNvmeSsdBlockConfig()
  {
    return $this->localNvmeSsdBlockConfig;
  }
  /**
   * @param int
   */
  public function setLocalSsdCount($localSsdCount)
  {
    $this->localSsdCount = $localSsdCount;
  }
  /**
   * @return int
   */
  public function getLocalSsdCount()
  {
    return $this->localSsdCount;
  }
  /**
   * @param string
   */
  public function setLocalSsdEncryptionMode($localSsdEncryptionMode)
  {
    $this->localSsdEncryptionMode = $localSsdEncryptionMode;
  }
  /**
   * @return string
   */
  public function getLocalSsdEncryptionMode()
  {
    return $this->localSsdEncryptionMode;
  }
  /**
   * @param NodePoolLoggingConfig
   */
  public function setLoggingConfig(NodePoolLoggingConfig $loggingConfig)
  {
    $this->loggingConfig = $loggingConfig;
  }
  /**
   * @return NodePoolLoggingConfig
   */
  public function getLoggingConfig()
  {
    return $this->loggingConfig;
  }
  /**
   * @param string
   */
  public function setMachineType($machineType)
  {
    $this->machineType = $machineType;
  }
  /**
   * @return string
   */
  public function getMachineType()
  {
    return $this->machineType;
  }
  /**
   * @param string
   */
  public function setMaxRunDuration($maxRunDuration)
  {
    $this->maxRunDuration = $maxRunDuration;
  }
  /**
   * @return string
   */
  public function getMaxRunDuration()
  {
    return $this->maxRunDuration;
  }
  /**
   * @param string[]
   */
  public function setMetadata($metadata)
  {
    $this->metadata = $metadata;
  }
  /**
   * @return string[]
   */
  public function getMetadata()
  {
    return $this->metadata;
  }
  /**
   * @param string
   */
  public function setMinCpuPlatform($minCpuPlatform)
  {
    $this->minCpuPlatform = $minCpuPlatform;
  }
  /**
   * @return string
   */
  public function getMinCpuPlatform()
  {
    return $this->minCpuPlatform;
  }
  /**
   * @param string
   */
  public function setNodeGroup($nodeGroup)
  {
    $this->nodeGroup = $nodeGroup;
  }
  /**
   * @return string
   */
  public function getNodeGroup()
  {
    return $this->nodeGroup;
  }
  /**
   * @param string[]
   */
  public function setOauthScopes($oauthScopes)
  {
    $this->oauthScopes = $oauthScopes;
  }
  /**
   * @return string[]
   */
  public function getOauthScopes()
  {
    return $this->oauthScopes;
  }
  /**
   * @param bool
   */
  public function setPreemptible($preemptible)
  {
    $this->preemptible = $preemptible;
  }
  /**
   * @return bool
   */
  public function getPreemptible()
  {
    return $this->preemptible;
  }
  /**
   * @param ReservationAffinity
   */
  public function setReservationAffinity(ReservationAffinity $reservationAffinity)
  {
    $this->reservationAffinity = $reservationAffinity;
  }
  /**
   * @return ReservationAffinity
   */
  public function getReservationAffinity()
  {
    return $this->reservationAffinity;
  }
  /**
   * @param string[]
   */
  public function setResourceLabels($resourceLabels)
  {
    $this->resourceLabels = $resourceLabels;
  }
  /**
   * @return string[]
   */
  public function getResourceLabels()
  {
    return $this->resourceLabels;
  }
  /**
   * @param ResourceManagerTags
   */
  public function setResourceManagerTags(ResourceManagerTags $resourceManagerTags)
  {
    $this->resourceManagerTags = $resourceManagerTags;
  }
  /**
   * @return ResourceManagerTags
   */
  public function getResourceManagerTags()
  {
    return $this->resourceManagerTags;
  }
  /**
   * @param SandboxConfig
   */
  public function setSandboxConfig(SandboxConfig $sandboxConfig)
  {
    $this->sandboxConfig = $sandboxConfig;
  }
  /**
   * @return SandboxConfig
   */
  public function getSandboxConfig()
  {
    return $this->sandboxConfig;
  }
  /**
   * @param SecondaryBootDiskUpdateStrategy
   */
  public function setSecondaryBootDiskUpdateStrategy(SecondaryBootDiskUpdateStrategy $secondaryBootDiskUpdateStrategy)
  {
    $this->secondaryBootDiskUpdateStrategy = $secondaryBootDiskUpdateStrategy;
  }
  /**
   * @return SecondaryBootDiskUpdateStrategy
   */
  public function getSecondaryBootDiskUpdateStrategy()
  {
    return $this->secondaryBootDiskUpdateStrategy;
  }
  /**
   * @param SecondaryBootDisk[]
   */
  public function setSecondaryBootDisks($secondaryBootDisks)
  {
    $this->secondaryBootDisks = $secondaryBootDisks;
  }
  /**
   * @return SecondaryBootDisk[]
   */
  public function getSecondaryBootDisks()
  {
    return $this->secondaryBootDisks;
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
   * @param ShieldedInstanceConfig
   */
  public function setShieldedInstanceConfig(ShieldedInstanceConfig $shieldedInstanceConfig)
  {
    $this->shieldedInstanceConfig = $shieldedInstanceConfig;
  }
  /**
   * @return ShieldedInstanceConfig
   */
  public function getShieldedInstanceConfig()
  {
    return $this->shieldedInstanceConfig;
  }
  /**
   * @param SoleTenantConfig
   */
  public function setSoleTenantConfig(SoleTenantConfig $soleTenantConfig)
  {
    $this->soleTenantConfig = $soleTenantConfig;
  }
  /**
   * @return SoleTenantConfig
   */
  public function getSoleTenantConfig()
  {
    return $this->soleTenantConfig;
  }
  /**
   * @param bool
   */
  public function setSpot($spot)
  {
    $this->spot = $spot;
  }
  /**
   * @return bool
   */
  public function getSpot()
  {
    return $this->spot;
  }
  /**
   * @param string[]
   */
  public function setStoragePools($storagePools)
  {
    $this->storagePools = $storagePools;
  }
  /**
   * @return string[]
   */
  public function getStoragePools()
  {
    return $this->storagePools;
  }
  /**
   * @param string[]
   */
  public function setTags($tags)
  {
    $this->tags = $tags;
  }
  /**
   * @return string[]
   */
  public function getTags()
  {
    return $this->tags;
  }
  /**
   * @param NodeTaint[]
   */
  public function setTaints($taints)
  {
    $this->taints = $taints;
  }
  /**
   * @return NodeTaint[]
   */
  public function getTaints()
  {
    return $this->taints;
  }
  /**
   * @param WindowsNodeConfig
   */
  public function setWindowsNodeConfig(WindowsNodeConfig $windowsNodeConfig)
  {
    $this->windowsNodeConfig = $windowsNodeConfig;
  }
  /**
   * @return WindowsNodeConfig
   */
  public function getWindowsNodeConfig()
  {
    return $this->windowsNodeConfig;
  }
  /**
   * @param WorkloadMetadataConfig
   */
  public function setWorkloadMetadataConfig(WorkloadMetadataConfig $workloadMetadataConfig)
  {
    $this->workloadMetadataConfig = $workloadMetadataConfig;
  }
  /**
   * @return WorkloadMetadataConfig
   */
  public function getWorkloadMetadataConfig()
  {
    return $this->workloadMetadataConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(NodeConfig::class, 'Google_Service_Container_NodeConfig');
