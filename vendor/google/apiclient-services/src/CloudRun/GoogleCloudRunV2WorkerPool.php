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

class GoogleCloudRunV2WorkerPool extends \Google\Collection
{
  protected $collection_key = 'instanceSplits';
  /**
   * @var string[]
   */
  public $annotations;
  protected $binaryAuthorizationType = GoogleCloudRunV2BinaryAuthorization::class;
  protected $binaryAuthorizationDataType = '';
  /**
   * @var string
   */
  public $client;
  /**
   * @var string
   */
  public $clientVersion;
  protected $conditionsType = GoogleCloudRunV2Condition::class;
  protected $conditionsDataType = 'array';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $creator;
  /**
   * @var string[]
   */
  public $customAudiences;
  /**
   * @var string
   */
  public $deleteTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $etag;
  /**
   * @var string
   */
  public $expireTime;
  /**
   * @var string
   */
  public $generation;
  protected $instanceSplitStatusesType = GoogleCloudRunV2InstanceSplitStatus::class;
  protected $instanceSplitStatusesDataType = 'array';
  protected $instanceSplitsType = GoogleCloudRunV2InstanceSplit::class;
  protected $instanceSplitsDataType = 'array';
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $lastModifier;
  /**
   * @var string
   */
  public $latestCreatedRevision;
  /**
   * @var string
   */
  public $latestReadyRevision;
  /**
   * @var string
   */
  public $launchStage;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $observedGeneration;
  /**
   * @var bool
   */
  public $reconciling;
  /**
   * @var bool
   */
  public $satisfiesPzs;
  protected $scalingType = GoogleCloudRunV2WorkerPoolScaling::class;
  protected $scalingDataType = '';
  protected $templateType = GoogleCloudRunV2WorkerPoolRevisionTemplate::class;
  protected $templateDataType = '';
  protected $terminalConditionType = GoogleCloudRunV2Condition::class;
  protected $terminalConditionDataType = '';
  /**
   * @var string
   */
  public $uid;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string[]
   */
  public function setAnnotations($annotations)
  {
    $this->annotations = $annotations;
  }
  /**
   * @return string[]
   */
  public function getAnnotations()
  {
    return $this->annotations;
  }
  /**
   * @param GoogleCloudRunV2BinaryAuthorization
   */
  public function setBinaryAuthorization(GoogleCloudRunV2BinaryAuthorization $binaryAuthorization)
  {
    $this->binaryAuthorization = $binaryAuthorization;
  }
  /**
   * @return GoogleCloudRunV2BinaryAuthorization
   */
  public function getBinaryAuthorization()
  {
    return $this->binaryAuthorization;
  }
  /**
   * @param string
   */
  public function setClient($client)
  {
    $this->client = $client;
  }
  /**
   * @return string
   */
  public function getClient()
  {
    return $this->client;
  }
  /**
   * @param string
   */
  public function setClientVersion($clientVersion)
  {
    $this->clientVersion = $clientVersion;
  }
  /**
   * @return string
   */
  public function getClientVersion()
  {
    return $this->clientVersion;
  }
  /**
   * @param GoogleCloudRunV2Condition[]
   */
  public function setConditions($conditions)
  {
    $this->conditions = $conditions;
  }
  /**
   * @return GoogleCloudRunV2Condition[]
   */
  public function getConditions()
  {
    return $this->conditions;
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
  public function setCreator($creator)
  {
    $this->creator = $creator;
  }
  /**
   * @return string
   */
  public function getCreator()
  {
    return $this->creator;
  }
  /**
   * @param string[]
   */
  public function setCustomAudiences($customAudiences)
  {
    $this->customAudiences = $customAudiences;
  }
  /**
   * @return string[]
   */
  public function getCustomAudiences()
  {
    return $this->customAudiences;
  }
  /**
   * @param string
   */
  public function setDeleteTime($deleteTime)
  {
    $this->deleteTime = $deleteTime;
  }
  /**
   * @return string
   */
  public function getDeleteTime()
  {
    return $this->deleteTime;
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
   * @param string
   */
  public function setExpireTime($expireTime)
  {
    $this->expireTime = $expireTime;
  }
  /**
   * @return string
   */
  public function getExpireTime()
  {
    return $this->expireTime;
  }
  /**
   * @param string
   */
  public function setGeneration($generation)
  {
    $this->generation = $generation;
  }
  /**
   * @return string
   */
  public function getGeneration()
  {
    return $this->generation;
  }
  /**
   * @param GoogleCloudRunV2InstanceSplitStatus[]
   */
  public function setInstanceSplitStatuses($instanceSplitStatuses)
  {
    $this->instanceSplitStatuses = $instanceSplitStatuses;
  }
  /**
   * @return GoogleCloudRunV2InstanceSplitStatus[]
   */
  public function getInstanceSplitStatuses()
  {
    return $this->instanceSplitStatuses;
  }
  /**
   * @param GoogleCloudRunV2InstanceSplit[]
   */
  public function setInstanceSplits($instanceSplits)
  {
    $this->instanceSplits = $instanceSplits;
  }
  /**
   * @return GoogleCloudRunV2InstanceSplit[]
   */
  public function getInstanceSplits()
  {
    return $this->instanceSplits;
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
   * @param string
   */
  public function setLastModifier($lastModifier)
  {
    $this->lastModifier = $lastModifier;
  }
  /**
   * @return string
   */
  public function getLastModifier()
  {
    return $this->lastModifier;
  }
  /**
   * @param string
   */
  public function setLatestCreatedRevision($latestCreatedRevision)
  {
    $this->latestCreatedRevision = $latestCreatedRevision;
  }
  /**
   * @return string
   */
  public function getLatestCreatedRevision()
  {
    return $this->latestCreatedRevision;
  }
  /**
   * @param string
   */
  public function setLatestReadyRevision($latestReadyRevision)
  {
    $this->latestReadyRevision = $latestReadyRevision;
  }
  /**
   * @return string
   */
  public function getLatestReadyRevision()
  {
    return $this->latestReadyRevision;
  }
  /**
   * @param string
   */
  public function setLaunchStage($launchStage)
  {
    $this->launchStage = $launchStage;
  }
  /**
   * @return string
   */
  public function getLaunchStage()
  {
    return $this->launchStage;
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
  public function setObservedGeneration($observedGeneration)
  {
    $this->observedGeneration = $observedGeneration;
  }
  /**
   * @return string
   */
  public function getObservedGeneration()
  {
    return $this->observedGeneration;
  }
  /**
   * @param bool
   */
  public function setReconciling($reconciling)
  {
    $this->reconciling = $reconciling;
  }
  /**
   * @return bool
   */
  public function getReconciling()
  {
    return $this->reconciling;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzs($satisfiesPzs)
  {
    $this->satisfiesPzs = $satisfiesPzs;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzs()
  {
    return $this->satisfiesPzs;
  }
  /**
   * @param GoogleCloudRunV2WorkerPoolScaling
   */
  public function setScaling(GoogleCloudRunV2WorkerPoolScaling $scaling)
  {
    $this->scaling = $scaling;
  }
  /**
   * @return GoogleCloudRunV2WorkerPoolScaling
   */
  public function getScaling()
  {
    return $this->scaling;
  }
  /**
   * @param GoogleCloudRunV2WorkerPoolRevisionTemplate
   */
  public function setTemplate(GoogleCloudRunV2WorkerPoolRevisionTemplate $template)
  {
    $this->template = $template;
  }
  /**
   * @return GoogleCloudRunV2WorkerPoolRevisionTemplate
   */
  public function getTemplate()
  {
    return $this->template;
  }
  /**
   * @param GoogleCloudRunV2Condition
   */
  public function setTerminalCondition(GoogleCloudRunV2Condition $terminalCondition)
  {
    $this->terminalCondition = $terminalCondition;
  }
  /**
   * @return GoogleCloudRunV2Condition
   */
  public function getTerminalCondition()
  {
    return $this->terminalCondition;
  }
  /**
   * @param string
   */
  public function setUid($uid)
  {
    $this->uid = $uid;
  }
  /**
   * @return string
   */
  public function getUid()
  {
    return $this->uid;
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
class_alias(GoogleCloudRunV2WorkerPool::class, 'Google_Service_CloudRun_GoogleCloudRunV2WorkerPool');
