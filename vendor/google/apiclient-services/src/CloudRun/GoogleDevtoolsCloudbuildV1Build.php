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

namespace Google\Service\CloudRun;

class GoogleDevtoolsCloudbuildV1Build extends \Google\Collection
{
  protected $collection_key = 'warnings';
  protected $approvalType = GoogleDevtoolsCloudbuildV1BuildApproval::class;
  protected $approvalDataType = '';
  protected $artifactsType = GoogleDevtoolsCloudbuildV1Artifacts::class;
  protected $artifactsDataType = '';
  protected $availableSecretsType = GoogleDevtoolsCloudbuildV1Secrets::class;
  protected $availableSecretsDataType = '';
  /**
   * @var string
   */
  public $buildTriggerId;
  /**
   * @var string
   */
  public $createTime;
  protected $dependenciesType = GoogleDevtoolsCloudbuildV1Dependency::class;
  protected $dependenciesDataType = 'array';
  protected $failureInfoType = GoogleDevtoolsCloudbuildV1FailureInfo::class;
  protected $failureInfoDataType = '';
  /**
   * @var string
   */
  public $finishTime;
  protected $gitConfigType = GoogleDevtoolsCloudbuildV1GitConfig::class;
  protected $gitConfigDataType = '';
  /**
   * @var string
   */
  public $id;
  /**
   * @var string[]
   */
  public $images;
  /**
   * @var string
   */
  public $logUrl;
  /**
   * @var string
   */
  public $logsBucket;
  /**
   * @var string
   */
  public $name;
  protected $optionsType = GoogleDevtoolsCloudbuildV1BuildOptions::class;
  protected $optionsDataType = '';
  /**
   * @var string
   */
  public $projectId;
  /**
   * @var string
   */
  public $queueTtl;
  protected $resultsType = GoogleDevtoolsCloudbuildV1Results::class;
  protected $resultsDataType = '';
  protected $secretsType = GoogleDevtoolsCloudbuildV1Secret::class;
  protected $secretsDataType = 'array';
  /**
   * @var string
   */
  public $serviceAccount;
  protected $sourceType = GoogleDevtoolsCloudbuildV1Source::class;
  protected $sourceDataType = '';
  protected $sourceProvenanceType = GoogleDevtoolsCloudbuildV1SourceProvenance::class;
  protected $sourceProvenanceDataType = '';
  /**
   * @var string
   */
  public $startTime;
  /**
   * @var string
   */
  public $status;
  /**
   * @var string
   */
  public $statusDetail;
  protected $stepsType = GoogleDevtoolsCloudbuildV1BuildStep::class;
  protected $stepsDataType = 'array';
  /**
   * @var string[]
   */
  public $substitutions;
  /**
   * @var string[]
   */
  public $tags;
  /**
   * @var string
   */
  public $timeout;
  protected $timingType = GoogleDevtoolsCloudbuildV1TimeSpan::class;
  protected $timingDataType = 'map';
  protected $warningsType = GoogleDevtoolsCloudbuildV1Warning::class;
  protected $warningsDataType = 'array';

  /**
   * @param GoogleDevtoolsCloudbuildV1BuildApproval
   */
  public function setApproval(GoogleDevtoolsCloudbuildV1BuildApproval $approval)
  {
    $this->approval = $approval;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1BuildApproval
   */
  public function getApproval()
  {
    return $this->approval;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1Artifacts
   */
  public function setArtifacts(GoogleDevtoolsCloudbuildV1Artifacts $artifacts)
  {
    $this->artifacts = $artifacts;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1Artifacts
   */
  public function getArtifacts()
  {
    return $this->artifacts;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1Secrets
   */
  public function setAvailableSecrets(GoogleDevtoolsCloudbuildV1Secrets $availableSecrets)
  {
    $this->availableSecrets = $availableSecrets;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1Secrets
   */
  public function getAvailableSecrets()
  {
    return $this->availableSecrets;
  }
  /**
   * @param string
   */
  public function setBuildTriggerId($buildTriggerId)
  {
    $this->buildTriggerId = $buildTriggerId;
  }
  /**
   * @return string
   */
  public function getBuildTriggerId()
  {
    return $this->buildTriggerId;
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
   * @param GoogleDevtoolsCloudbuildV1Dependency[]
   */
  public function setDependencies($dependencies)
  {
    $this->dependencies = $dependencies;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1Dependency[]
   */
  public function getDependencies()
  {
    return $this->dependencies;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1FailureInfo
   */
  public function setFailureInfo(GoogleDevtoolsCloudbuildV1FailureInfo $failureInfo)
  {
    $this->failureInfo = $failureInfo;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1FailureInfo
   */
  public function getFailureInfo()
  {
    return $this->failureInfo;
  }
  /**
   * @param string
   */
  public function setFinishTime($finishTime)
  {
    $this->finishTime = $finishTime;
  }
  /**
   * @return string
   */
  public function getFinishTime()
  {
    return $this->finishTime;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1GitConfig
   */
  public function setGitConfig(GoogleDevtoolsCloudbuildV1GitConfig $gitConfig)
  {
    $this->gitConfig = $gitConfig;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1GitConfig
   */
  public function getGitConfig()
  {
    return $this->gitConfig;
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
   * @param string[]
   */
  public function setImages($images)
  {
    $this->images = $images;
  }
  /**
   * @return string[]
   */
  public function getImages()
  {
    return $this->images;
  }
  /**
   * @param string
   */
  public function setLogUrl($logUrl)
  {
    $this->logUrl = $logUrl;
  }
  /**
   * @return string
   */
  public function getLogUrl()
  {
    return $this->logUrl;
  }
  /**
   * @param string
   */
  public function setLogsBucket($logsBucket)
  {
    $this->logsBucket = $logsBucket;
  }
  /**
   * @return string
   */
  public function getLogsBucket()
  {
    return $this->logsBucket;
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
   * @param GoogleDevtoolsCloudbuildV1BuildOptions
   */
  public function setOptions(GoogleDevtoolsCloudbuildV1BuildOptions $options)
  {
    $this->options = $options;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1BuildOptions
   */
  public function getOptions()
  {
    return $this->options;
  }
  /**
   * @param string
   */
  public function setProjectId($projectId)
  {
    $this->projectId = $projectId;
  }
  /**
   * @return string
   */
  public function getProjectId()
  {
    return $this->projectId;
  }
  /**
   * @param string
   */
  public function setQueueTtl($queueTtl)
  {
    $this->queueTtl = $queueTtl;
  }
  /**
   * @return string
   */
  public function getQueueTtl()
  {
    return $this->queueTtl;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1Results
   */
  public function setResults(GoogleDevtoolsCloudbuildV1Results $results)
  {
    $this->results = $results;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1Results
   */
  public function getResults()
  {
    return $this->results;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1Secret[]
   */
  public function setSecrets($secrets)
  {
    $this->secrets = $secrets;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1Secret[]
   */
  public function getSecrets()
  {
    return $this->secrets;
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
   * @param GoogleDevtoolsCloudbuildV1Source
   */
  public function setSource(GoogleDevtoolsCloudbuildV1Source $source)
  {
    $this->source = $source;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1Source
   */
  public function getSource()
  {
    return $this->source;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1SourceProvenance
   */
  public function setSourceProvenance(GoogleDevtoolsCloudbuildV1SourceProvenance $sourceProvenance)
  {
    $this->sourceProvenance = $sourceProvenance;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1SourceProvenance
   */
  public function getSourceProvenance()
  {
    return $this->sourceProvenance;
  }
  /**
   * @param string
   */
  public function setStartTime($startTime)
  {
    $this->startTime = $startTime;
  }
  /**
   * @return string
   */
  public function getStartTime()
  {
    return $this->startTime;
  }
  /**
   * @param string
   */
  public function setStatus($status)
  {
    $this->status = $status;
  }
  /**
   * @return string
   */
  public function getStatus()
  {
    return $this->status;
  }
  /**
   * @param string
   */
  public function setStatusDetail($statusDetail)
  {
    $this->statusDetail = $statusDetail;
  }
  /**
   * @return string
   */
  public function getStatusDetail()
  {
    return $this->statusDetail;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1BuildStep[]
   */
  public function setSteps($steps)
  {
    $this->steps = $steps;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1BuildStep[]
   */
  public function getSteps()
  {
    return $this->steps;
  }
  /**
   * @param string[]
   */
  public function setSubstitutions($substitutions)
  {
    $this->substitutions = $substitutions;
  }
  /**
   * @return string[]
   */
  public function getSubstitutions()
  {
    return $this->substitutions;
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
   * @param string
   */
  public function setTimeout($timeout)
  {
    $this->timeout = $timeout;
  }
  /**
   * @return string
   */
  public function getTimeout()
  {
    return $this->timeout;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1TimeSpan[]
   */
  public function setTiming($timing)
  {
    $this->timing = $timing;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1TimeSpan[]
   */
  public function getTiming()
  {
    return $this->timing;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1Warning[]
   */
  public function setWarnings($warnings)
  {
    $this->warnings = $warnings;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1Warning[]
   */
  public function getWarnings()
  {
    return $this->warnings;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleDevtoolsCloudbuildV1Build::class, 'Google_Service_CloudRun_GoogleDevtoolsCloudbuildV1Build');
