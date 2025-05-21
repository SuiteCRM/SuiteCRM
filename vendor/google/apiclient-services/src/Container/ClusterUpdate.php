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

class ClusterUpdate extends \Google\Collection
{
  protected $collection_key = 'desiredLocations';
  protected $additionalPodRangesConfigType = AdditionalPodRangesConfig::class;
  protected $additionalPodRangesConfigDataType = '';
  protected $desiredAddonsConfigType = AddonsConfig::class;
  protected $desiredAddonsConfigDataType = '';
  protected $desiredAuthenticatorGroupsConfigType = AuthenticatorGroupsConfig::class;
  protected $desiredAuthenticatorGroupsConfigDataType = '';
  protected $desiredAutopilotWorkloadPolicyConfigType = WorkloadPolicyConfig::class;
  protected $desiredAutopilotWorkloadPolicyConfigDataType = '';
  protected $desiredBinaryAuthorizationType = BinaryAuthorization::class;
  protected $desiredBinaryAuthorizationDataType = '';
  protected $desiredClusterAutoscalingType = ClusterAutoscaling::class;
  protected $desiredClusterAutoscalingDataType = '';
  protected $desiredCompliancePostureConfigType = CompliancePostureConfig::class;
  protected $desiredCompliancePostureConfigDataType = '';
  protected $desiredContainerdConfigType = ContainerdConfig::class;
  protected $desiredContainerdConfigDataType = '';
  protected $desiredControlPlaneEndpointsConfigType = ControlPlaneEndpointsConfig::class;
  protected $desiredControlPlaneEndpointsConfigDataType = '';
  protected $desiredCostManagementConfigType = CostManagementConfig::class;
  protected $desiredCostManagementConfigDataType = '';
  protected $desiredDatabaseEncryptionType = DatabaseEncryption::class;
  protected $desiredDatabaseEncryptionDataType = '';
  /**
   * @var string
   */
  public $desiredDatapathProvider;
  /**
   * @var bool
   */
  public $desiredDefaultEnablePrivateNodes;
  protected $desiredDefaultSnatStatusType = DefaultSnatStatus::class;
  protected $desiredDefaultSnatStatusDataType = '';
  /**
   * @var bool
   */
  public $desiredDisableL4LbFirewallReconciliation;
  protected $desiredDnsConfigType = DNSConfig::class;
  protected $desiredDnsConfigDataType = '';
  /**
   * @var bool
   */
  public $desiredEnableCiliumClusterwideNetworkPolicy;
  /**
   * @var bool
   */
  public $desiredEnableFqdnNetworkPolicy;
  /**
   * @var bool
   */
  public $desiredEnableMultiNetworking;
  /**
   * @var bool
   */
  public $desiredEnablePrivateEndpoint;
  protected $desiredEnterpriseConfigType = DesiredEnterpriseConfig::class;
  protected $desiredEnterpriseConfigDataType = '';
  protected $desiredFleetType = Fleet::class;
  protected $desiredFleetDataType = '';
  protected $desiredGatewayApiConfigType = GatewayAPIConfig::class;
  protected $desiredGatewayApiConfigDataType = '';
  protected $desiredGcfsConfigType = GcfsConfig::class;
  protected $desiredGcfsConfigDataType = '';
  protected $desiredIdentityServiceConfigType = IdentityServiceConfig::class;
  protected $desiredIdentityServiceConfigDataType = '';
  /**
   * @var string
   */
  public $desiredImageType;
  /**
   * @var string
   */
  public $desiredInTransitEncryptionConfig;
  protected $desiredIntraNodeVisibilityConfigType = IntraNodeVisibilityConfig::class;
  protected $desiredIntraNodeVisibilityConfigDataType = '';
  protected $desiredK8sBetaApisType = K8sBetaAPIConfig::class;
  protected $desiredK8sBetaApisDataType = '';
  protected $desiredL4ilbSubsettingConfigType = ILBSubsettingConfig::class;
  protected $desiredL4ilbSubsettingConfigDataType = '';
  /**
   * @var string[]
   */
  public $desiredLocations;
  protected $desiredLoggingConfigType = LoggingConfig::class;
  protected $desiredLoggingConfigDataType = '';
  /**
   * @var string
   */
  public $desiredLoggingService;
  protected $desiredMasterAuthorizedNetworksConfigType = MasterAuthorizedNetworksConfig::class;
  protected $desiredMasterAuthorizedNetworksConfigDataType = '';
  /**
   * @var string
   */
  public $desiredMasterVersion;
  protected $desiredMeshCertificatesType = MeshCertificates::class;
  protected $desiredMeshCertificatesDataType = '';
  protected $desiredMonitoringConfigType = MonitoringConfig::class;
  protected $desiredMonitoringConfigDataType = '';
  /**
   * @var string
   */
  public $desiredMonitoringService;
  protected $desiredNetworkPerformanceConfigType = ClusterNetworkPerformanceConfig::class;
  protected $desiredNetworkPerformanceConfigDataType = '';
  protected $desiredNodeKubeletConfigType = NodeKubeletConfig::class;
  protected $desiredNodeKubeletConfigDataType = '';
  protected $desiredNodePoolAutoConfigKubeletConfigType = NodeKubeletConfig::class;
  protected $desiredNodePoolAutoConfigKubeletConfigDataType = '';
  protected $desiredNodePoolAutoConfigLinuxNodeConfigType = LinuxNodeConfig::class;
  protected $desiredNodePoolAutoConfigLinuxNodeConfigDataType = '';
  protected $desiredNodePoolAutoConfigNetworkTagsType = NetworkTags::class;
  protected $desiredNodePoolAutoConfigNetworkTagsDataType = '';
  protected $desiredNodePoolAutoConfigResourceManagerTagsType = ResourceManagerTags::class;
  protected $desiredNodePoolAutoConfigResourceManagerTagsDataType = '';
  protected $desiredNodePoolAutoscalingType = NodePoolAutoscaling::class;
  protected $desiredNodePoolAutoscalingDataType = '';
  /**
   * @var string
   */
  public $desiredNodePoolId;
  protected $desiredNodePoolLoggingConfigType = NodePoolLoggingConfig::class;
  protected $desiredNodePoolLoggingConfigDataType = '';
  /**
   * @var string
   */
  public $desiredNodeVersion;
  protected $desiredNotificationConfigType = NotificationConfig::class;
  protected $desiredNotificationConfigDataType = '';
  protected $desiredParentProductConfigType = ParentProductConfig::class;
  protected $desiredParentProductConfigDataType = '';
  protected $desiredPrivateClusterConfigType = PrivateClusterConfig::class;
  protected $desiredPrivateClusterConfigDataType = '';
  /**
   * @var string
   */
  public $desiredPrivateIpv6GoogleAccess;
  protected $desiredRbacBindingConfigType = RBACBindingConfig::class;
  protected $desiredRbacBindingConfigDataType = '';
  protected $desiredReleaseChannelType = ReleaseChannel::class;
  protected $desiredReleaseChannelDataType = '';
  protected $desiredResourceUsageExportConfigType = ResourceUsageExportConfig::class;
  protected $desiredResourceUsageExportConfigDataType = '';
  protected $desiredSecretManagerConfigType = SecretManagerConfig::class;
  protected $desiredSecretManagerConfigDataType = '';
  protected $desiredSecurityPostureConfigType = SecurityPostureConfig::class;
  protected $desiredSecurityPostureConfigDataType = '';
  protected $desiredServiceExternalIpsConfigType = ServiceExternalIPsConfig::class;
  protected $desiredServiceExternalIpsConfigDataType = '';
  protected $desiredShieldedNodesType = ShieldedNodes::class;
  protected $desiredShieldedNodesDataType = '';
  /**
   * @var string
   */
  public $desiredStackType;
  protected $desiredVerticalPodAutoscalingType = VerticalPodAutoscaling::class;
  protected $desiredVerticalPodAutoscalingDataType = '';
  protected $desiredWorkloadIdentityConfigType = WorkloadIdentityConfig::class;
  protected $desiredWorkloadIdentityConfigDataType = '';
  protected $enableK8sBetaApisType = K8sBetaAPIConfig::class;
  protected $enableK8sBetaApisDataType = '';
  /**
   * @var string
   */
  public $etag;
  protected $removedAdditionalPodRangesConfigType = AdditionalPodRangesConfig::class;
  protected $removedAdditionalPodRangesConfigDataType = '';
  protected $userManagedKeysConfigType = UserManagedKeysConfig::class;
  protected $userManagedKeysConfigDataType = '';

  /**
   * @param AdditionalPodRangesConfig
   */
  public function setAdditionalPodRangesConfig(AdditionalPodRangesConfig $additionalPodRangesConfig)
  {
    $this->additionalPodRangesConfig = $additionalPodRangesConfig;
  }
  /**
   * @return AdditionalPodRangesConfig
   */
  public function getAdditionalPodRangesConfig()
  {
    return $this->additionalPodRangesConfig;
  }
  /**
   * @param AddonsConfig
   */
  public function setDesiredAddonsConfig(AddonsConfig $desiredAddonsConfig)
  {
    $this->desiredAddonsConfig = $desiredAddonsConfig;
  }
  /**
   * @return AddonsConfig
   */
  public function getDesiredAddonsConfig()
  {
    return $this->desiredAddonsConfig;
  }
  /**
   * @param AuthenticatorGroupsConfig
   */
  public function setDesiredAuthenticatorGroupsConfig(AuthenticatorGroupsConfig $desiredAuthenticatorGroupsConfig)
  {
    $this->desiredAuthenticatorGroupsConfig = $desiredAuthenticatorGroupsConfig;
  }
  /**
   * @return AuthenticatorGroupsConfig
   */
  public function getDesiredAuthenticatorGroupsConfig()
  {
    return $this->desiredAuthenticatorGroupsConfig;
  }
  /**
   * @param WorkloadPolicyConfig
   */
  public function setDesiredAutopilotWorkloadPolicyConfig(WorkloadPolicyConfig $desiredAutopilotWorkloadPolicyConfig)
  {
    $this->desiredAutopilotWorkloadPolicyConfig = $desiredAutopilotWorkloadPolicyConfig;
  }
  /**
   * @return WorkloadPolicyConfig
   */
  public function getDesiredAutopilotWorkloadPolicyConfig()
  {
    return $this->desiredAutopilotWorkloadPolicyConfig;
  }
  /**
   * @param BinaryAuthorization
   */
  public function setDesiredBinaryAuthorization(BinaryAuthorization $desiredBinaryAuthorization)
  {
    $this->desiredBinaryAuthorization = $desiredBinaryAuthorization;
  }
  /**
   * @return BinaryAuthorization
   */
  public function getDesiredBinaryAuthorization()
  {
    return $this->desiredBinaryAuthorization;
  }
  /**
   * @param ClusterAutoscaling
   */
  public function setDesiredClusterAutoscaling(ClusterAutoscaling $desiredClusterAutoscaling)
  {
    $this->desiredClusterAutoscaling = $desiredClusterAutoscaling;
  }
  /**
   * @return ClusterAutoscaling
   */
  public function getDesiredClusterAutoscaling()
  {
    return $this->desiredClusterAutoscaling;
  }
  /**
   * @param CompliancePostureConfig
   */
  public function setDesiredCompliancePostureConfig(CompliancePostureConfig $desiredCompliancePostureConfig)
  {
    $this->desiredCompliancePostureConfig = $desiredCompliancePostureConfig;
  }
  /**
   * @return CompliancePostureConfig
   */
  public function getDesiredCompliancePostureConfig()
  {
    return $this->desiredCompliancePostureConfig;
  }
  /**
   * @param ContainerdConfig
   */
  public function setDesiredContainerdConfig(ContainerdConfig $desiredContainerdConfig)
  {
    $this->desiredContainerdConfig = $desiredContainerdConfig;
  }
  /**
   * @return ContainerdConfig
   */
  public function getDesiredContainerdConfig()
  {
    return $this->desiredContainerdConfig;
  }
  /**
   * @param ControlPlaneEndpointsConfig
   */
  public function setDesiredControlPlaneEndpointsConfig(ControlPlaneEndpointsConfig $desiredControlPlaneEndpointsConfig)
  {
    $this->desiredControlPlaneEndpointsConfig = $desiredControlPlaneEndpointsConfig;
  }
  /**
   * @return ControlPlaneEndpointsConfig
   */
  public function getDesiredControlPlaneEndpointsConfig()
  {
    return $this->desiredControlPlaneEndpointsConfig;
  }
  /**
   * @param CostManagementConfig
   */
  public function setDesiredCostManagementConfig(CostManagementConfig $desiredCostManagementConfig)
  {
    $this->desiredCostManagementConfig = $desiredCostManagementConfig;
  }
  /**
   * @return CostManagementConfig
   */
  public function getDesiredCostManagementConfig()
  {
    return $this->desiredCostManagementConfig;
  }
  /**
   * @param DatabaseEncryption
   */
  public function setDesiredDatabaseEncryption(DatabaseEncryption $desiredDatabaseEncryption)
  {
    $this->desiredDatabaseEncryption = $desiredDatabaseEncryption;
  }
  /**
   * @return DatabaseEncryption
   */
  public function getDesiredDatabaseEncryption()
  {
    return $this->desiredDatabaseEncryption;
  }
  /**
   * @param string
   */
  public function setDesiredDatapathProvider($desiredDatapathProvider)
  {
    $this->desiredDatapathProvider = $desiredDatapathProvider;
  }
  /**
   * @return string
   */
  public function getDesiredDatapathProvider()
  {
    return $this->desiredDatapathProvider;
  }
  /**
   * @param bool
   */
  public function setDesiredDefaultEnablePrivateNodes($desiredDefaultEnablePrivateNodes)
  {
    $this->desiredDefaultEnablePrivateNodes = $desiredDefaultEnablePrivateNodes;
  }
  /**
   * @return bool
   */
  public function getDesiredDefaultEnablePrivateNodes()
  {
    return $this->desiredDefaultEnablePrivateNodes;
  }
  /**
   * @param DefaultSnatStatus
   */
  public function setDesiredDefaultSnatStatus(DefaultSnatStatus $desiredDefaultSnatStatus)
  {
    $this->desiredDefaultSnatStatus = $desiredDefaultSnatStatus;
  }
  /**
   * @return DefaultSnatStatus
   */
  public function getDesiredDefaultSnatStatus()
  {
    return $this->desiredDefaultSnatStatus;
  }
  /**
   * @param bool
   */
  public function setDesiredDisableL4LbFirewallReconciliation($desiredDisableL4LbFirewallReconciliation)
  {
    $this->desiredDisableL4LbFirewallReconciliation = $desiredDisableL4LbFirewallReconciliation;
  }
  /**
   * @return bool
   */
  public function getDesiredDisableL4LbFirewallReconciliation()
  {
    return $this->desiredDisableL4LbFirewallReconciliation;
  }
  /**
   * @param DNSConfig
   */
  public function setDesiredDnsConfig(DNSConfig $desiredDnsConfig)
  {
    $this->desiredDnsConfig = $desiredDnsConfig;
  }
  /**
   * @return DNSConfig
   */
  public function getDesiredDnsConfig()
  {
    return $this->desiredDnsConfig;
  }
  /**
   * @param bool
   */
  public function setDesiredEnableCiliumClusterwideNetworkPolicy($desiredEnableCiliumClusterwideNetworkPolicy)
  {
    $this->desiredEnableCiliumClusterwideNetworkPolicy = $desiredEnableCiliumClusterwideNetworkPolicy;
  }
  /**
   * @return bool
   */
  public function getDesiredEnableCiliumClusterwideNetworkPolicy()
  {
    return $this->desiredEnableCiliumClusterwideNetworkPolicy;
  }
  /**
   * @param bool
   */
  public function setDesiredEnableFqdnNetworkPolicy($desiredEnableFqdnNetworkPolicy)
  {
    $this->desiredEnableFqdnNetworkPolicy = $desiredEnableFqdnNetworkPolicy;
  }
  /**
   * @return bool
   */
  public function getDesiredEnableFqdnNetworkPolicy()
  {
    return $this->desiredEnableFqdnNetworkPolicy;
  }
  /**
   * @param bool
   */
  public function setDesiredEnableMultiNetworking($desiredEnableMultiNetworking)
  {
    $this->desiredEnableMultiNetworking = $desiredEnableMultiNetworking;
  }
  /**
   * @return bool
   */
  public function getDesiredEnableMultiNetworking()
  {
    return $this->desiredEnableMultiNetworking;
  }
  /**
   * @param bool
   */
  public function setDesiredEnablePrivateEndpoint($desiredEnablePrivateEndpoint)
  {
    $this->desiredEnablePrivateEndpoint = $desiredEnablePrivateEndpoint;
  }
  /**
   * @return bool
   */
  public function getDesiredEnablePrivateEndpoint()
  {
    return $this->desiredEnablePrivateEndpoint;
  }
  /**
   * @param DesiredEnterpriseConfig
   */
  public function setDesiredEnterpriseConfig(DesiredEnterpriseConfig $desiredEnterpriseConfig)
  {
    $this->desiredEnterpriseConfig = $desiredEnterpriseConfig;
  }
  /**
   * @return DesiredEnterpriseConfig
   */
  public function getDesiredEnterpriseConfig()
  {
    return $this->desiredEnterpriseConfig;
  }
  /**
   * @param Fleet
   */
  public function setDesiredFleet(Fleet $desiredFleet)
  {
    $this->desiredFleet = $desiredFleet;
  }
  /**
   * @return Fleet
   */
  public function getDesiredFleet()
  {
    return $this->desiredFleet;
  }
  /**
   * @param GatewayAPIConfig
   */
  public function setDesiredGatewayApiConfig(GatewayAPIConfig $desiredGatewayApiConfig)
  {
    $this->desiredGatewayApiConfig = $desiredGatewayApiConfig;
  }
  /**
   * @return GatewayAPIConfig
   */
  public function getDesiredGatewayApiConfig()
  {
    return $this->desiredGatewayApiConfig;
  }
  /**
   * @param GcfsConfig
   */
  public function setDesiredGcfsConfig(GcfsConfig $desiredGcfsConfig)
  {
    $this->desiredGcfsConfig = $desiredGcfsConfig;
  }
  /**
   * @return GcfsConfig
   */
  public function getDesiredGcfsConfig()
  {
    return $this->desiredGcfsConfig;
  }
  /**
   * @param IdentityServiceConfig
   */
  public function setDesiredIdentityServiceConfig(IdentityServiceConfig $desiredIdentityServiceConfig)
  {
    $this->desiredIdentityServiceConfig = $desiredIdentityServiceConfig;
  }
  /**
   * @return IdentityServiceConfig
   */
  public function getDesiredIdentityServiceConfig()
  {
    return $this->desiredIdentityServiceConfig;
  }
  /**
   * @param string
   */
  public function setDesiredImageType($desiredImageType)
  {
    $this->desiredImageType = $desiredImageType;
  }
  /**
   * @return string
   */
  public function getDesiredImageType()
  {
    return $this->desiredImageType;
  }
  /**
   * @param string
   */
  public function setDesiredInTransitEncryptionConfig($desiredInTransitEncryptionConfig)
  {
    $this->desiredInTransitEncryptionConfig = $desiredInTransitEncryptionConfig;
  }
  /**
   * @return string
   */
  public function getDesiredInTransitEncryptionConfig()
  {
    return $this->desiredInTransitEncryptionConfig;
  }
  /**
   * @param IntraNodeVisibilityConfig
   */
  public function setDesiredIntraNodeVisibilityConfig(IntraNodeVisibilityConfig $desiredIntraNodeVisibilityConfig)
  {
    $this->desiredIntraNodeVisibilityConfig = $desiredIntraNodeVisibilityConfig;
  }
  /**
   * @return IntraNodeVisibilityConfig
   */
  public function getDesiredIntraNodeVisibilityConfig()
  {
    return $this->desiredIntraNodeVisibilityConfig;
  }
  /**
   * @param K8sBetaAPIConfig
   */
  public function setDesiredK8sBetaApis(K8sBetaAPIConfig $desiredK8sBetaApis)
  {
    $this->desiredK8sBetaApis = $desiredK8sBetaApis;
  }
  /**
   * @return K8sBetaAPIConfig
   */
  public function getDesiredK8sBetaApis()
  {
    return $this->desiredK8sBetaApis;
  }
  /**
   * @param ILBSubsettingConfig
   */
  public function setDesiredL4ilbSubsettingConfig(ILBSubsettingConfig $desiredL4ilbSubsettingConfig)
  {
    $this->desiredL4ilbSubsettingConfig = $desiredL4ilbSubsettingConfig;
  }
  /**
   * @return ILBSubsettingConfig
   */
  public function getDesiredL4ilbSubsettingConfig()
  {
    return $this->desiredL4ilbSubsettingConfig;
  }
  /**
   * @param string[]
   */
  public function setDesiredLocations($desiredLocations)
  {
    $this->desiredLocations = $desiredLocations;
  }
  /**
   * @return string[]
   */
  public function getDesiredLocations()
  {
    return $this->desiredLocations;
  }
  /**
   * @param LoggingConfig
   */
  public function setDesiredLoggingConfig(LoggingConfig $desiredLoggingConfig)
  {
    $this->desiredLoggingConfig = $desiredLoggingConfig;
  }
  /**
   * @return LoggingConfig
   */
  public function getDesiredLoggingConfig()
  {
    return $this->desiredLoggingConfig;
  }
  /**
   * @param string
   */
  public function setDesiredLoggingService($desiredLoggingService)
  {
    $this->desiredLoggingService = $desiredLoggingService;
  }
  /**
   * @return string
   */
  public function getDesiredLoggingService()
  {
    return $this->desiredLoggingService;
  }
  /**
   * @param MasterAuthorizedNetworksConfig
   */
  public function setDesiredMasterAuthorizedNetworksConfig(MasterAuthorizedNetworksConfig $desiredMasterAuthorizedNetworksConfig)
  {
    $this->desiredMasterAuthorizedNetworksConfig = $desiredMasterAuthorizedNetworksConfig;
  }
  /**
   * @return MasterAuthorizedNetworksConfig
   */
  public function getDesiredMasterAuthorizedNetworksConfig()
  {
    return $this->desiredMasterAuthorizedNetworksConfig;
  }
  /**
   * @param string
   */
  public function setDesiredMasterVersion($desiredMasterVersion)
  {
    $this->desiredMasterVersion = $desiredMasterVersion;
  }
  /**
   * @return string
   */
  public function getDesiredMasterVersion()
  {
    return $this->desiredMasterVersion;
  }
  /**
   * @param MeshCertificates
   */
  public function setDesiredMeshCertificates(MeshCertificates $desiredMeshCertificates)
  {
    $this->desiredMeshCertificates = $desiredMeshCertificates;
  }
  /**
   * @return MeshCertificates
   */
  public function getDesiredMeshCertificates()
  {
    return $this->desiredMeshCertificates;
  }
  /**
   * @param MonitoringConfig
   */
  public function setDesiredMonitoringConfig(MonitoringConfig $desiredMonitoringConfig)
  {
    $this->desiredMonitoringConfig = $desiredMonitoringConfig;
  }
  /**
   * @return MonitoringConfig
   */
  public function getDesiredMonitoringConfig()
  {
    return $this->desiredMonitoringConfig;
  }
  /**
   * @param string
   */
  public function setDesiredMonitoringService($desiredMonitoringService)
  {
    $this->desiredMonitoringService = $desiredMonitoringService;
  }
  /**
   * @return string
   */
  public function getDesiredMonitoringService()
  {
    return $this->desiredMonitoringService;
  }
  /**
   * @param ClusterNetworkPerformanceConfig
   */
  public function setDesiredNetworkPerformanceConfig(ClusterNetworkPerformanceConfig $desiredNetworkPerformanceConfig)
  {
    $this->desiredNetworkPerformanceConfig = $desiredNetworkPerformanceConfig;
  }
  /**
   * @return ClusterNetworkPerformanceConfig
   */
  public function getDesiredNetworkPerformanceConfig()
  {
    return $this->desiredNetworkPerformanceConfig;
  }
  /**
   * @param NodeKubeletConfig
   */
  public function setDesiredNodeKubeletConfig(NodeKubeletConfig $desiredNodeKubeletConfig)
  {
    $this->desiredNodeKubeletConfig = $desiredNodeKubeletConfig;
  }
  /**
   * @return NodeKubeletConfig
   */
  public function getDesiredNodeKubeletConfig()
  {
    return $this->desiredNodeKubeletConfig;
  }
  /**
   * @param NodeKubeletConfig
   */
  public function setDesiredNodePoolAutoConfigKubeletConfig(NodeKubeletConfig $desiredNodePoolAutoConfigKubeletConfig)
  {
    $this->desiredNodePoolAutoConfigKubeletConfig = $desiredNodePoolAutoConfigKubeletConfig;
  }
  /**
   * @return NodeKubeletConfig
   */
  public function getDesiredNodePoolAutoConfigKubeletConfig()
  {
    return $this->desiredNodePoolAutoConfigKubeletConfig;
  }
  /**
   * @param LinuxNodeConfig
   */
  public function setDesiredNodePoolAutoConfigLinuxNodeConfig(LinuxNodeConfig $desiredNodePoolAutoConfigLinuxNodeConfig)
  {
    $this->desiredNodePoolAutoConfigLinuxNodeConfig = $desiredNodePoolAutoConfigLinuxNodeConfig;
  }
  /**
   * @return LinuxNodeConfig
   */
  public function getDesiredNodePoolAutoConfigLinuxNodeConfig()
  {
    return $this->desiredNodePoolAutoConfigLinuxNodeConfig;
  }
  /**
   * @param NetworkTags
   */
  public function setDesiredNodePoolAutoConfigNetworkTags(NetworkTags $desiredNodePoolAutoConfigNetworkTags)
  {
    $this->desiredNodePoolAutoConfigNetworkTags = $desiredNodePoolAutoConfigNetworkTags;
  }
  /**
   * @return NetworkTags
   */
  public function getDesiredNodePoolAutoConfigNetworkTags()
  {
    return $this->desiredNodePoolAutoConfigNetworkTags;
  }
  /**
   * @param ResourceManagerTags
   */
  public function setDesiredNodePoolAutoConfigResourceManagerTags(ResourceManagerTags $desiredNodePoolAutoConfigResourceManagerTags)
  {
    $this->desiredNodePoolAutoConfigResourceManagerTags = $desiredNodePoolAutoConfigResourceManagerTags;
  }
  /**
   * @return ResourceManagerTags
   */
  public function getDesiredNodePoolAutoConfigResourceManagerTags()
  {
    return $this->desiredNodePoolAutoConfigResourceManagerTags;
  }
  /**
   * @param NodePoolAutoscaling
   */
  public function setDesiredNodePoolAutoscaling(NodePoolAutoscaling $desiredNodePoolAutoscaling)
  {
    $this->desiredNodePoolAutoscaling = $desiredNodePoolAutoscaling;
  }
  /**
   * @return NodePoolAutoscaling
   */
  public function getDesiredNodePoolAutoscaling()
  {
    return $this->desiredNodePoolAutoscaling;
  }
  /**
   * @param string
   */
  public function setDesiredNodePoolId($desiredNodePoolId)
  {
    $this->desiredNodePoolId = $desiredNodePoolId;
  }
  /**
   * @return string
   */
  public function getDesiredNodePoolId()
  {
    return $this->desiredNodePoolId;
  }
  /**
   * @param NodePoolLoggingConfig
   */
  public function setDesiredNodePoolLoggingConfig(NodePoolLoggingConfig $desiredNodePoolLoggingConfig)
  {
    $this->desiredNodePoolLoggingConfig = $desiredNodePoolLoggingConfig;
  }
  /**
   * @return NodePoolLoggingConfig
   */
  public function getDesiredNodePoolLoggingConfig()
  {
    return $this->desiredNodePoolLoggingConfig;
  }
  /**
   * @param string
   */
  public function setDesiredNodeVersion($desiredNodeVersion)
  {
    $this->desiredNodeVersion = $desiredNodeVersion;
  }
  /**
   * @return string
   */
  public function getDesiredNodeVersion()
  {
    return $this->desiredNodeVersion;
  }
  /**
   * @param NotificationConfig
   */
  public function setDesiredNotificationConfig(NotificationConfig $desiredNotificationConfig)
  {
    $this->desiredNotificationConfig = $desiredNotificationConfig;
  }
  /**
   * @return NotificationConfig
   */
  public function getDesiredNotificationConfig()
  {
    return $this->desiredNotificationConfig;
  }
  /**
   * @param ParentProductConfig
   */
  public function setDesiredParentProductConfig(ParentProductConfig $desiredParentProductConfig)
  {
    $this->desiredParentProductConfig = $desiredParentProductConfig;
  }
  /**
   * @return ParentProductConfig
   */
  public function getDesiredParentProductConfig()
  {
    return $this->desiredParentProductConfig;
  }
  /**
   * @param PrivateClusterConfig
   */
  public function setDesiredPrivateClusterConfig(PrivateClusterConfig $desiredPrivateClusterConfig)
  {
    $this->desiredPrivateClusterConfig = $desiredPrivateClusterConfig;
  }
  /**
   * @return PrivateClusterConfig
   */
  public function getDesiredPrivateClusterConfig()
  {
    return $this->desiredPrivateClusterConfig;
  }
  /**
   * @param string
   */
  public function setDesiredPrivateIpv6GoogleAccess($desiredPrivateIpv6GoogleAccess)
  {
    $this->desiredPrivateIpv6GoogleAccess = $desiredPrivateIpv6GoogleAccess;
  }
  /**
   * @return string
   */
  public function getDesiredPrivateIpv6GoogleAccess()
  {
    return $this->desiredPrivateIpv6GoogleAccess;
  }
  /**
   * @param RBACBindingConfig
   */
  public function setDesiredRbacBindingConfig(RBACBindingConfig $desiredRbacBindingConfig)
  {
    $this->desiredRbacBindingConfig = $desiredRbacBindingConfig;
  }
  /**
   * @return RBACBindingConfig
   */
  public function getDesiredRbacBindingConfig()
  {
    return $this->desiredRbacBindingConfig;
  }
  /**
   * @param ReleaseChannel
   */
  public function setDesiredReleaseChannel(ReleaseChannel $desiredReleaseChannel)
  {
    $this->desiredReleaseChannel = $desiredReleaseChannel;
  }
  /**
   * @return ReleaseChannel
   */
  public function getDesiredReleaseChannel()
  {
    return $this->desiredReleaseChannel;
  }
  /**
   * @param ResourceUsageExportConfig
   */
  public function setDesiredResourceUsageExportConfig(ResourceUsageExportConfig $desiredResourceUsageExportConfig)
  {
    $this->desiredResourceUsageExportConfig = $desiredResourceUsageExportConfig;
  }
  /**
   * @return ResourceUsageExportConfig
   */
  public function getDesiredResourceUsageExportConfig()
  {
    return $this->desiredResourceUsageExportConfig;
  }
  /**
   * @param SecretManagerConfig
   */
  public function setDesiredSecretManagerConfig(SecretManagerConfig $desiredSecretManagerConfig)
  {
    $this->desiredSecretManagerConfig = $desiredSecretManagerConfig;
  }
  /**
   * @return SecretManagerConfig
   */
  public function getDesiredSecretManagerConfig()
  {
    return $this->desiredSecretManagerConfig;
  }
  /**
   * @param SecurityPostureConfig
   */
  public function setDesiredSecurityPostureConfig(SecurityPostureConfig $desiredSecurityPostureConfig)
  {
    $this->desiredSecurityPostureConfig = $desiredSecurityPostureConfig;
  }
  /**
   * @return SecurityPostureConfig
   */
  public function getDesiredSecurityPostureConfig()
  {
    return $this->desiredSecurityPostureConfig;
  }
  /**
   * @param ServiceExternalIPsConfig
   */
  public function setDesiredServiceExternalIpsConfig(ServiceExternalIPsConfig $desiredServiceExternalIpsConfig)
  {
    $this->desiredServiceExternalIpsConfig = $desiredServiceExternalIpsConfig;
  }
  /**
   * @return ServiceExternalIPsConfig
   */
  public function getDesiredServiceExternalIpsConfig()
  {
    return $this->desiredServiceExternalIpsConfig;
  }
  /**
   * @param ShieldedNodes
   */
  public function setDesiredShieldedNodes(ShieldedNodes $desiredShieldedNodes)
  {
    $this->desiredShieldedNodes = $desiredShieldedNodes;
  }
  /**
   * @return ShieldedNodes
   */
  public function getDesiredShieldedNodes()
  {
    return $this->desiredShieldedNodes;
  }
  /**
   * @param string
   */
  public function setDesiredStackType($desiredStackType)
  {
    $this->desiredStackType = $desiredStackType;
  }
  /**
   * @return string
   */
  public function getDesiredStackType()
  {
    return $this->desiredStackType;
  }
  /**
   * @param VerticalPodAutoscaling
   */
  public function setDesiredVerticalPodAutoscaling(VerticalPodAutoscaling $desiredVerticalPodAutoscaling)
  {
    $this->desiredVerticalPodAutoscaling = $desiredVerticalPodAutoscaling;
  }
  /**
   * @return VerticalPodAutoscaling
   */
  public function getDesiredVerticalPodAutoscaling()
  {
    return $this->desiredVerticalPodAutoscaling;
  }
  /**
   * @param WorkloadIdentityConfig
   */
  public function setDesiredWorkloadIdentityConfig(WorkloadIdentityConfig $desiredWorkloadIdentityConfig)
  {
    $this->desiredWorkloadIdentityConfig = $desiredWorkloadIdentityConfig;
  }
  /**
   * @return WorkloadIdentityConfig
   */
  public function getDesiredWorkloadIdentityConfig()
  {
    return $this->desiredWorkloadIdentityConfig;
  }
  /**
   * @param K8sBetaAPIConfig
   */
  public function setEnableK8sBetaApis(K8sBetaAPIConfig $enableK8sBetaApis)
  {
    $this->enableK8sBetaApis = $enableK8sBetaApis;
  }
  /**
   * @return K8sBetaAPIConfig
   */
  public function getEnableK8sBetaApis()
  {
    return $this->enableK8sBetaApis;
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
   * @param AdditionalPodRangesConfig
   */
  public function setRemovedAdditionalPodRangesConfig(AdditionalPodRangesConfig $removedAdditionalPodRangesConfig)
  {
    $this->removedAdditionalPodRangesConfig = $removedAdditionalPodRangesConfig;
  }
  /**
   * @return AdditionalPodRangesConfig
   */
  public function getRemovedAdditionalPodRangesConfig()
  {
    return $this->removedAdditionalPodRangesConfig;
  }
  /**
   * @param UserManagedKeysConfig
   */
  public function setUserManagedKeysConfig(UserManagedKeysConfig $userManagedKeysConfig)
  {
    $this->userManagedKeysConfig = $userManagedKeysConfig;
  }
  /**
   * @return UserManagedKeysConfig
   */
  public function getUserManagedKeysConfig()
  {
    return $this->userManagedKeysConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ClusterUpdate::class, 'Google_Service_Container_ClusterUpdate');
