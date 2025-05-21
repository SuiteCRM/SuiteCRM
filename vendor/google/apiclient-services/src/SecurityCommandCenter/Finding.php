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

namespace Google\Service\SecurityCommandCenter;

class Finding extends \Google\Collection
{
  protected $collection_key = 'processes';
  protected $accessType = Access::class;
  protected $accessDataType = '';
  protected $applicationType = Application::class;
  protected $applicationDataType = '';
  protected $attackExposureType = AttackExposure::class;
  protected $attackExposureDataType = '';
  protected $backupDisasterRecoveryType = BackupDisasterRecovery::class;
  protected $backupDisasterRecoveryDataType = '';
  /**
   * @var string
   */
  public $canonicalName;
  /**
   * @var string
   */
  public $category;
  protected $cloudArmorType = CloudArmor::class;
  protected $cloudArmorDataType = '';
  protected $cloudDlpDataProfileType = CloudDlpDataProfile::class;
  protected $cloudDlpDataProfileDataType = '';
  protected $cloudDlpInspectionType = CloudDlpInspection::class;
  protected $cloudDlpInspectionDataType = '';
  protected $compliancesType = Compliance::class;
  protected $compliancesDataType = 'array';
  protected $connectionsType = Connection::class;
  protected $connectionsDataType = 'array';
  protected $contactsType = ContactDetails::class;
  protected $contactsDataType = 'map';
  protected $containersType = Container::class;
  protected $containersDataType = 'array';
  /**
   * @var string
   */
  public $createTime;
  protected $dataAccessEventsType = DataAccessEvent::class;
  protected $dataAccessEventsDataType = 'array';
  protected $dataFlowEventsType = DataFlowEvent::class;
  protected $dataFlowEventsDataType = 'array';
  protected $dataRetentionDeletionEventsType = DataRetentionDeletionEvent::class;
  protected $dataRetentionDeletionEventsDataType = 'array';
  protected $databaseType = Database::class;
  protected $databaseDataType = '';
  /**
   * @var string
   */
  public $description;
  protected $diskType = Disk::class;
  protected $diskDataType = '';
  /**
   * @var string
   */
  public $eventTime;
  protected $exfiltrationType = Exfiltration::class;
  protected $exfiltrationDataType = '';
  protected $externalSystemsType = GoogleCloudSecuritycenterV1ExternalSystem::class;
  protected $externalSystemsDataType = 'map';
  /**
   * @var string
   */
  public $externalUri;
  protected $filesType = SecuritycenterFile::class;
  protected $filesDataType = 'array';
  /**
   * @var string
   */
  public $findingClass;
  protected $groupMembershipsType = GroupMembership::class;
  protected $groupMembershipsDataType = 'array';
  protected $iamBindingsType = IamBinding::class;
  protected $iamBindingsDataType = 'array';
  protected $indicatorType = Indicator::class;
  protected $indicatorDataType = '';
  protected $ipRulesType = IpRules::class;
  protected $ipRulesDataType = '';
  protected $jobType = Job::class;
  protected $jobDataType = '';
  protected $kernelRootkitType = KernelRootkit::class;
  protected $kernelRootkitDataType = '';
  protected $kubernetesType = Kubernetes::class;
  protected $kubernetesDataType = '';
  protected $loadBalancersType = LoadBalancer::class;
  protected $loadBalancersDataType = 'array';
  protected $logEntriesType = LogEntry::class;
  protected $logEntriesDataType = 'array';
  protected $mitreAttackType = MitreAttack::class;
  protected $mitreAttackDataType = '';
  /**
   * @var string
   */
  public $moduleName;
  /**
   * @var string
   */
  public $mute;
  protected $muteInfoType = MuteInfo::class;
  protected $muteInfoDataType = '';
  /**
   * @var string
   */
  public $muteInitiator;
  /**
   * @var string
   */
  public $muteUpdateTime;
  /**
   * @var string
   */
  public $name;
  protected $networksType = Network::class;
  protected $networksDataType = 'array';
  /**
   * @var string
   */
  public $nextSteps;
  protected $notebookType = Notebook::class;
  protected $notebookDataType = '';
  protected $orgPoliciesType = OrgPolicy::class;
  protected $orgPoliciesDataType = 'array';
  /**
   * @var string
   */
  public $parent;
  /**
   * @var string
   */
  public $parentDisplayName;
  protected $processesType = Process::class;
  protected $processesDataType = 'array';
  /**
   * @var string
   */
  public $resourceName;
  protected $securityMarksType = SecurityMarks::class;
  protected $securityMarksDataType = '';
  protected $securityPostureType = SecurityPosture::class;
  protected $securityPostureDataType = '';
  /**
   * @var string
   */
  public $severity;
  /**
   * @var array[]
   */
  public $sourceProperties;
  /**
   * @var string
   */
  public $state;
  protected $toxicCombinationType = ToxicCombination::class;
  protected $toxicCombinationDataType = '';
  protected $vulnerabilityType = Vulnerability::class;
  protected $vulnerabilityDataType = '';

  /**
   * @param Access
   */
  public function setAccess(Access $access)
  {
    $this->access = $access;
  }
  /**
   * @return Access
   */
  public function getAccess()
  {
    return $this->access;
  }
  /**
   * @param Application
   */
  public function setApplication(Application $application)
  {
    $this->application = $application;
  }
  /**
   * @return Application
   */
  public function getApplication()
  {
    return $this->application;
  }
  /**
   * @param AttackExposure
   */
  public function setAttackExposure(AttackExposure $attackExposure)
  {
    $this->attackExposure = $attackExposure;
  }
  /**
   * @return AttackExposure
   */
  public function getAttackExposure()
  {
    return $this->attackExposure;
  }
  /**
   * @param BackupDisasterRecovery
   */
  public function setBackupDisasterRecovery(BackupDisasterRecovery $backupDisasterRecovery)
  {
    $this->backupDisasterRecovery = $backupDisasterRecovery;
  }
  /**
   * @return BackupDisasterRecovery
   */
  public function getBackupDisasterRecovery()
  {
    return $this->backupDisasterRecovery;
  }
  /**
   * @param string
   */
  public function setCanonicalName($canonicalName)
  {
    $this->canonicalName = $canonicalName;
  }
  /**
   * @return string
   */
  public function getCanonicalName()
  {
    return $this->canonicalName;
  }
  /**
   * @param string
   */
  public function setCategory($category)
  {
    $this->category = $category;
  }
  /**
   * @return string
   */
  public function getCategory()
  {
    return $this->category;
  }
  /**
   * @param CloudArmor
   */
  public function setCloudArmor(CloudArmor $cloudArmor)
  {
    $this->cloudArmor = $cloudArmor;
  }
  /**
   * @return CloudArmor
   */
  public function getCloudArmor()
  {
    return $this->cloudArmor;
  }
  /**
   * @param CloudDlpDataProfile
   */
  public function setCloudDlpDataProfile(CloudDlpDataProfile $cloudDlpDataProfile)
  {
    $this->cloudDlpDataProfile = $cloudDlpDataProfile;
  }
  /**
   * @return CloudDlpDataProfile
   */
  public function getCloudDlpDataProfile()
  {
    return $this->cloudDlpDataProfile;
  }
  /**
   * @param CloudDlpInspection
   */
  public function setCloudDlpInspection(CloudDlpInspection $cloudDlpInspection)
  {
    $this->cloudDlpInspection = $cloudDlpInspection;
  }
  /**
   * @return CloudDlpInspection
   */
  public function getCloudDlpInspection()
  {
    return $this->cloudDlpInspection;
  }
  /**
   * @param Compliance[]
   */
  public function setCompliances($compliances)
  {
    $this->compliances = $compliances;
  }
  /**
   * @return Compliance[]
   */
  public function getCompliances()
  {
    return $this->compliances;
  }
  /**
   * @param Connection[]
   */
  public function setConnections($connections)
  {
    $this->connections = $connections;
  }
  /**
   * @return Connection[]
   */
  public function getConnections()
  {
    return $this->connections;
  }
  /**
   * @param ContactDetails[]
   */
  public function setContacts($contacts)
  {
    $this->contacts = $contacts;
  }
  /**
   * @return ContactDetails[]
   */
  public function getContacts()
  {
    return $this->contacts;
  }
  /**
   * @param Container[]
   */
  public function setContainers($containers)
  {
    $this->containers = $containers;
  }
  /**
   * @return Container[]
   */
  public function getContainers()
  {
    return $this->containers;
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
   * @param DataAccessEvent[]
   */
  public function setDataAccessEvents($dataAccessEvents)
  {
    $this->dataAccessEvents = $dataAccessEvents;
  }
  /**
   * @return DataAccessEvent[]
   */
  public function getDataAccessEvents()
  {
    return $this->dataAccessEvents;
  }
  /**
   * @param DataFlowEvent[]
   */
  public function setDataFlowEvents($dataFlowEvents)
  {
    $this->dataFlowEvents = $dataFlowEvents;
  }
  /**
   * @return DataFlowEvent[]
   */
  public function getDataFlowEvents()
  {
    return $this->dataFlowEvents;
  }
  /**
   * @param DataRetentionDeletionEvent[]
   */
  public function setDataRetentionDeletionEvents($dataRetentionDeletionEvents)
  {
    $this->dataRetentionDeletionEvents = $dataRetentionDeletionEvents;
  }
  /**
   * @return DataRetentionDeletionEvent[]
   */
  public function getDataRetentionDeletionEvents()
  {
    return $this->dataRetentionDeletionEvents;
  }
  /**
   * @param Database
   */
  public function setDatabase(Database $database)
  {
    $this->database = $database;
  }
  /**
   * @return Database
   */
  public function getDatabase()
  {
    return $this->database;
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
   * @param Disk
   */
  public function setDisk(Disk $disk)
  {
    $this->disk = $disk;
  }
  /**
   * @return Disk
   */
  public function getDisk()
  {
    return $this->disk;
  }
  /**
   * @param string
   */
  public function setEventTime($eventTime)
  {
    $this->eventTime = $eventTime;
  }
  /**
   * @return string
   */
  public function getEventTime()
  {
    return $this->eventTime;
  }
  /**
   * @param Exfiltration
   */
  public function setExfiltration(Exfiltration $exfiltration)
  {
    $this->exfiltration = $exfiltration;
  }
  /**
   * @return Exfiltration
   */
  public function getExfiltration()
  {
    return $this->exfiltration;
  }
  /**
   * @param GoogleCloudSecuritycenterV1ExternalSystem[]
   */
  public function setExternalSystems($externalSystems)
  {
    $this->externalSystems = $externalSystems;
  }
  /**
   * @return GoogleCloudSecuritycenterV1ExternalSystem[]
   */
  public function getExternalSystems()
  {
    return $this->externalSystems;
  }
  /**
   * @param string
   */
  public function setExternalUri($externalUri)
  {
    $this->externalUri = $externalUri;
  }
  /**
   * @return string
   */
  public function getExternalUri()
  {
    return $this->externalUri;
  }
  /**
   * @param SecuritycenterFile[]
   */
  public function setFiles($files)
  {
    $this->files = $files;
  }
  /**
   * @return SecuritycenterFile[]
   */
  public function getFiles()
  {
    return $this->files;
  }
  /**
   * @param string
   */
  public function setFindingClass($findingClass)
  {
    $this->findingClass = $findingClass;
  }
  /**
   * @return string
   */
  public function getFindingClass()
  {
    return $this->findingClass;
  }
  /**
   * @param GroupMembership[]
   */
  public function setGroupMemberships($groupMemberships)
  {
    $this->groupMemberships = $groupMemberships;
  }
  /**
   * @return GroupMembership[]
   */
  public function getGroupMemberships()
  {
    return $this->groupMemberships;
  }
  /**
   * @param IamBinding[]
   */
  public function setIamBindings($iamBindings)
  {
    $this->iamBindings = $iamBindings;
  }
  /**
   * @return IamBinding[]
   */
  public function getIamBindings()
  {
    return $this->iamBindings;
  }
  /**
   * @param Indicator
   */
  public function setIndicator(Indicator $indicator)
  {
    $this->indicator = $indicator;
  }
  /**
   * @return Indicator
   */
  public function getIndicator()
  {
    return $this->indicator;
  }
  /**
   * @param IpRules
   */
  public function setIpRules(IpRules $ipRules)
  {
    $this->ipRules = $ipRules;
  }
  /**
   * @return IpRules
   */
  public function getIpRules()
  {
    return $this->ipRules;
  }
  /**
   * @param Job
   */
  public function setJob(Job $job)
  {
    $this->job = $job;
  }
  /**
   * @return Job
   */
  public function getJob()
  {
    return $this->job;
  }
  /**
   * @param KernelRootkit
   */
  public function setKernelRootkit(KernelRootkit $kernelRootkit)
  {
    $this->kernelRootkit = $kernelRootkit;
  }
  /**
   * @return KernelRootkit
   */
  public function getKernelRootkit()
  {
    return $this->kernelRootkit;
  }
  /**
   * @param Kubernetes
   */
  public function setKubernetes(Kubernetes $kubernetes)
  {
    $this->kubernetes = $kubernetes;
  }
  /**
   * @return Kubernetes
   */
  public function getKubernetes()
  {
    return $this->kubernetes;
  }
  /**
   * @param LoadBalancer[]
   */
  public function setLoadBalancers($loadBalancers)
  {
    $this->loadBalancers = $loadBalancers;
  }
  /**
   * @return LoadBalancer[]
   */
  public function getLoadBalancers()
  {
    return $this->loadBalancers;
  }
  /**
   * @param LogEntry[]
   */
  public function setLogEntries($logEntries)
  {
    $this->logEntries = $logEntries;
  }
  /**
   * @return LogEntry[]
   */
  public function getLogEntries()
  {
    return $this->logEntries;
  }
  /**
   * @param MitreAttack
   */
  public function setMitreAttack(MitreAttack $mitreAttack)
  {
    $this->mitreAttack = $mitreAttack;
  }
  /**
   * @return MitreAttack
   */
  public function getMitreAttack()
  {
    return $this->mitreAttack;
  }
  /**
   * @param string
   */
  public function setModuleName($moduleName)
  {
    $this->moduleName = $moduleName;
  }
  /**
   * @return string
   */
  public function getModuleName()
  {
    return $this->moduleName;
  }
  /**
   * @param string
   */
  public function setMute($mute)
  {
    $this->mute = $mute;
  }
  /**
   * @return string
   */
  public function getMute()
  {
    return $this->mute;
  }
  /**
   * @param MuteInfo
   */
  public function setMuteInfo(MuteInfo $muteInfo)
  {
    $this->muteInfo = $muteInfo;
  }
  /**
   * @return MuteInfo
   */
  public function getMuteInfo()
  {
    return $this->muteInfo;
  }
  /**
   * @param string
   */
  public function setMuteInitiator($muteInitiator)
  {
    $this->muteInitiator = $muteInitiator;
  }
  /**
   * @return string
   */
  public function getMuteInitiator()
  {
    return $this->muteInitiator;
  }
  /**
   * @param string
   */
  public function setMuteUpdateTime($muteUpdateTime)
  {
    $this->muteUpdateTime = $muteUpdateTime;
  }
  /**
   * @return string
   */
  public function getMuteUpdateTime()
  {
    return $this->muteUpdateTime;
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
   * @param Network[]
   */
  public function setNetworks($networks)
  {
    $this->networks = $networks;
  }
  /**
   * @return Network[]
   */
  public function getNetworks()
  {
    return $this->networks;
  }
  /**
   * @param string
   */
  public function setNextSteps($nextSteps)
  {
    $this->nextSteps = $nextSteps;
  }
  /**
   * @return string
   */
  public function getNextSteps()
  {
    return $this->nextSteps;
  }
  /**
   * @param Notebook
   */
  public function setNotebook(Notebook $notebook)
  {
    $this->notebook = $notebook;
  }
  /**
   * @return Notebook
   */
  public function getNotebook()
  {
    return $this->notebook;
  }
  /**
   * @param OrgPolicy[]
   */
  public function setOrgPolicies($orgPolicies)
  {
    $this->orgPolicies = $orgPolicies;
  }
  /**
   * @return OrgPolicy[]
   */
  public function getOrgPolicies()
  {
    return $this->orgPolicies;
  }
  /**
   * @param string
   */
  public function setParent($parent)
  {
    $this->parent = $parent;
  }
  /**
   * @return string
   */
  public function getParent()
  {
    return $this->parent;
  }
  /**
   * @param string
   */
  public function setParentDisplayName($parentDisplayName)
  {
    $this->parentDisplayName = $parentDisplayName;
  }
  /**
   * @return string
   */
  public function getParentDisplayName()
  {
    return $this->parentDisplayName;
  }
  /**
   * @param Process[]
   */
  public function setProcesses($processes)
  {
    $this->processes = $processes;
  }
  /**
   * @return Process[]
   */
  public function getProcesses()
  {
    return $this->processes;
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
   * @param SecurityMarks
   */
  public function setSecurityMarks(SecurityMarks $securityMarks)
  {
    $this->securityMarks = $securityMarks;
  }
  /**
   * @return SecurityMarks
   */
  public function getSecurityMarks()
  {
    return $this->securityMarks;
  }
  /**
   * @param SecurityPosture
   */
  public function setSecurityPosture(SecurityPosture $securityPosture)
  {
    $this->securityPosture = $securityPosture;
  }
  /**
   * @return SecurityPosture
   */
  public function getSecurityPosture()
  {
    return $this->securityPosture;
  }
  /**
   * @param string
   */
  public function setSeverity($severity)
  {
    $this->severity = $severity;
  }
  /**
   * @return string
   */
  public function getSeverity()
  {
    return $this->severity;
  }
  /**
   * @param array[]
   */
  public function setSourceProperties($sourceProperties)
  {
    $this->sourceProperties = $sourceProperties;
  }
  /**
   * @return array[]
   */
  public function getSourceProperties()
  {
    return $this->sourceProperties;
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
   * @param ToxicCombination
   */
  public function setToxicCombination(ToxicCombination $toxicCombination)
  {
    $this->toxicCombination = $toxicCombination;
  }
  /**
   * @return ToxicCombination
   */
  public function getToxicCombination()
  {
    return $this->toxicCombination;
  }
  /**
   * @param Vulnerability
   */
  public function setVulnerability(Vulnerability $vulnerability)
  {
    $this->vulnerability = $vulnerability;
  }
  /**
   * @return Vulnerability
   */
  public function getVulnerability()
  {
    return $this->vulnerability;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Finding::class, 'Google_Service_SecurityCommandCenter_Finding');
