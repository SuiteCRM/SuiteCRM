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

namespace Google\Service\DatabaseMigrationService;

class ConversionWorkspace extends \Google\Model
{
  /**
   * @var string
   */
  public $createTime;
  protected $destinationType = DatabaseEngineInfo::class;
  protected $destinationDataType = '';
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string[]
   */
  public $globalSettings;
  /**
   * @var bool
   */
  public $hasUncommittedChanges;
  /**
   * @var string
   */
  public $latestCommitId;
  /**
   * @var string
   */
  public $latestCommitTime;
  /**
   * @var string
   */
  public $name;
  protected $sourceType = DatabaseEngineInfo::class;
  protected $sourceDataType = '';
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
   * @param DatabaseEngineInfo
   */
  public function setDestination(DatabaseEngineInfo $destination)
  {
    $this->destination = $destination;
  }
  /**
   * @return DatabaseEngineInfo
   */
  public function getDestination()
  {
    return $this->destination;
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
   * @param string[]
   */
  public function setGlobalSettings($globalSettings)
  {
    $this->globalSettings = $globalSettings;
  }
  /**
   * @return string[]
   */
  public function getGlobalSettings()
  {
    return $this->globalSettings;
  }
  /**
   * @param bool
   */
  public function setHasUncommittedChanges($hasUncommittedChanges)
  {
    $this->hasUncommittedChanges = $hasUncommittedChanges;
  }
  /**
   * @return bool
   */
  public function getHasUncommittedChanges()
  {
    return $this->hasUncommittedChanges;
  }
  /**
   * @param string
   */
  public function setLatestCommitId($latestCommitId)
  {
    $this->latestCommitId = $latestCommitId;
  }
  /**
   * @return string
   */
  public function getLatestCommitId()
  {
    return $this->latestCommitId;
  }
  /**
   * @param string
   */
  public function setLatestCommitTime($latestCommitTime)
  {
    $this->latestCommitTime = $latestCommitTime;
  }
  /**
   * @return string
   */
  public function getLatestCommitTime()
  {
    return $this->latestCommitTime;
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
   * @param DatabaseEngineInfo
   */
  public function setSource(DatabaseEngineInfo $source)
  {
    $this->source = $source;
  }
  /**
   * @return DatabaseEngineInfo
   */
  public function getSource()
  {
    return $this->source;
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
class_alias(ConversionWorkspace::class, 'Google_Service_DatabaseMigrationService_ConversionWorkspace');
