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

namespace Google\Service\Workflows;

class Workflow extends \Google\Collection
{
  protected $collection_key = 'allKmsKeysVersions';
  /**
   * @var string[]
   */
  public $allKmsKeys;
  /**
   * @var string[]
   */
  public $allKmsKeysVersions;
  /**
   * @var string
   */
  public $callLogLevel;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $cryptoKeyName;
  /**
   * @var string
   */
  public $cryptoKeyVersion;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $executionHistoryLevel;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $revisionCreateTime;
  /**
   * @var string
   */
  public $revisionId;
  /**
   * @var string
   */
  public $serviceAccount;
  /**
   * @var string
   */
  public $sourceContents;
  /**
   * @var string
   */
  public $state;
  protected $stateErrorType = StateError::class;
  protected $stateErrorDataType = '';
  /**
   * @var string[]
   */
  public $tags;
  /**
   * @var string
   */
  public $updateTime;
  /**
   * @var string[]
   */
  public $userEnvVars;

  /**
   * @param string[]
   */
  public function setAllKmsKeys($allKmsKeys)
  {
    $this->allKmsKeys = $allKmsKeys;
  }
  /**
   * @return string[]
   */
  public function getAllKmsKeys()
  {
    return $this->allKmsKeys;
  }
  /**
   * @param string[]
   */
  public function setAllKmsKeysVersions($allKmsKeysVersions)
  {
    $this->allKmsKeysVersions = $allKmsKeysVersions;
  }
  /**
   * @return string[]
   */
  public function getAllKmsKeysVersions()
  {
    return $this->allKmsKeysVersions;
  }
  /**
   * @param string
   */
  public function setCallLogLevel($callLogLevel)
  {
    $this->callLogLevel = $callLogLevel;
  }
  /**
   * @return string
   */
  public function getCallLogLevel()
  {
    return $this->callLogLevel;
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
  public function setCryptoKeyName($cryptoKeyName)
  {
    $this->cryptoKeyName = $cryptoKeyName;
  }
  /**
   * @return string
   */
  public function getCryptoKeyName()
  {
    return $this->cryptoKeyName;
  }
  /**
   * @param string
   */
  public function setCryptoKeyVersion($cryptoKeyVersion)
  {
    $this->cryptoKeyVersion = $cryptoKeyVersion;
  }
  /**
   * @return string
   */
  public function getCryptoKeyVersion()
  {
    return $this->cryptoKeyVersion;
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
  public function setExecutionHistoryLevel($executionHistoryLevel)
  {
    $this->executionHistoryLevel = $executionHistoryLevel;
  }
  /**
   * @return string
   */
  public function getExecutionHistoryLevel()
  {
    return $this->executionHistoryLevel;
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
  public function setRevisionCreateTime($revisionCreateTime)
  {
    $this->revisionCreateTime = $revisionCreateTime;
  }
  /**
   * @return string
   */
  public function getRevisionCreateTime()
  {
    return $this->revisionCreateTime;
  }
  /**
   * @param string
   */
  public function setRevisionId($revisionId)
  {
    $this->revisionId = $revisionId;
  }
  /**
   * @return string
   */
  public function getRevisionId()
  {
    return $this->revisionId;
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
   * @param string
   */
  public function setSourceContents($sourceContents)
  {
    $this->sourceContents = $sourceContents;
  }
  /**
   * @return string
   */
  public function getSourceContents()
  {
    return $this->sourceContents;
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
   * @param StateError
   */
  public function setStateError(StateError $stateError)
  {
    $this->stateError = $stateError;
  }
  /**
   * @return StateError
   */
  public function getStateError()
  {
    return $this->stateError;
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
   * @param string[]
   */
  public function setUserEnvVars($userEnvVars)
  {
    $this->userEnvVars = $userEnvVars;
  }
  /**
   * @return string[]
   */
  public function getUserEnvVars()
  {
    return $this->userEnvVars;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Workflow::class, 'Google_Service_Workflows_Workflow');
