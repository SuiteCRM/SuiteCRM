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

namespace Google\Service\Bigquery;

class TableReplicationInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $replicatedSourceLastRefreshTime;
  protected $replicationErrorType = ErrorProto::class;
  protected $replicationErrorDataType = '';
  /**
   * @var string
   */
  public $replicationIntervalMs;
  /**
   * @var string
   */
  public $replicationStatus;
  protected $sourceTableType = TableReference::class;
  protected $sourceTableDataType = '';

  /**
   * @param string
   */
  public function setReplicatedSourceLastRefreshTime($replicatedSourceLastRefreshTime)
  {
    $this->replicatedSourceLastRefreshTime = $replicatedSourceLastRefreshTime;
  }
  /**
   * @return string
   */
  public function getReplicatedSourceLastRefreshTime()
  {
    return $this->replicatedSourceLastRefreshTime;
  }
  /**
   * @param ErrorProto
   */
  public function setReplicationError(ErrorProto $replicationError)
  {
    $this->replicationError = $replicationError;
  }
  /**
   * @return ErrorProto
   */
  public function getReplicationError()
  {
    return $this->replicationError;
  }
  /**
   * @param string
   */
  public function setReplicationIntervalMs($replicationIntervalMs)
  {
    $this->replicationIntervalMs = $replicationIntervalMs;
  }
  /**
   * @return string
   */
  public function getReplicationIntervalMs()
  {
    return $this->replicationIntervalMs;
  }
  /**
   * @param string
   */
  public function setReplicationStatus($replicationStatus)
  {
    $this->replicationStatus = $replicationStatus;
  }
  /**
   * @return string
   */
  public function getReplicationStatus()
  {
    return $this->replicationStatus;
  }
  /**
   * @param TableReference
   */
  public function setSourceTable(TableReference $sourceTable)
  {
    $this->sourceTable = $sourceTable;
  }
  /**
   * @return TableReference
   */
  public function getSourceTable()
  {
    return $this->sourceTable;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TableReplicationInfo::class, 'Google_Service_Bigquery_TableReplicationInfo');
