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

namespace Google\Service\BigtableAdmin;

class Table extends \Google\Model
{
  protected $automatedBackupPolicyType = AutomatedBackupPolicy::class;
  protected $automatedBackupPolicyDataType = '';
  protected $changeStreamConfigType = ChangeStreamConfig::class;
  protected $changeStreamConfigDataType = '';
  protected $clusterStatesType = ClusterState::class;
  protected $clusterStatesDataType = 'map';
  protected $columnFamiliesType = ColumnFamily::class;
  protected $columnFamiliesDataType = 'map';
  /**
   * @var bool
   */
  public $deletionProtection;
  /**
   * @var string
   */
  public $granularity;
  /**
   * @var string
   */
  public $name;
  protected $restoreInfoType = RestoreInfo::class;
  protected $restoreInfoDataType = '';
  protected $statsType = TableStats::class;
  protected $statsDataType = '';

  /**
   * @param AutomatedBackupPolicy
   */
  public function setAutomatedBackupPolicy(AutomatedBackupPolicy $automatedBackupPolicy)
  {
    $this->automatedBackupPolicy = $automatedBackupPolicy;
  }
  /**
   * @return AutomatedBackupPolicy
   */
  public function getAutomatedBackupPolicy()
  {
    return $this->automatedBackupPolicy;
  }
  /**
   * @param ChangeStreamConfig
   */
  public function setChangeStreamConfig(ChangeStreamConfig $changeStreamConfig)
  {
    $this->changeStreamConfig = $changeStreamConfig;
  }
  /**
   * @return ChangeStreamConfig
   */
  public function getChangeStreamConfig()
  {
    return $this->changeStreamConfig;
  }
  /**
   * @param ClusterState[]
   */
  public function setClusterStates($clusterStates)
  {
    $this->clusterStates = $clusterStates;
  }
  /**
   * @return ClusterState[]
   */
  public function getClusterStates()
  {
    return $this->clusterStates;
  }
  /**
   * @param ColumnFamily[]
   */
  public function setColumnFamilies($columnFamilies)
  {
    $this->columnFamilies = $columnFamilies;
  }
  /**
   * @return ColumnFamily[]
   */
  public function getColumnFamilies()
  {
    return $this->columnFamilies;
  }
  /**
   * @param bool
   */
  public function setDeletionProtection($deletionProtection)
  {
    $this->deletionProtection = $deletionProtection;
  }
  /**
   * @return bool
   */
  public function getDeletionProtection()
  {
    return $this->deletionProtection;
  }
  /**
   * @param string
   */
  public function setGranularity($granularity)
  {
    $this->granularity = $granularity;
  }
  /**
   * @return string
   */
  public function getGranularity()
  {
    return $this->granularity;
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
   * @param RestoreInfo
   */
  public function setRestoreInfo(RestoreInfo $restoreInfo)
  {
    $this->restoreInfo = $restoreInfo;
  }
  /**
   * @return RestoreInfo
   */
  public function getRestoreInfo()
  {
    return $this->restoreInfo;
  }
  /**
   * @param TableStats
   */
  public function setStats(TableStats $stats)
  {
    $this->stats = $stats;
  }
  /**
   * @return TableStats
   */
  public function getStats()
  {
    return $this->stats;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Table::class, 'Google_Service_BigtableAdmin_Table');
