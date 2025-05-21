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

class QueryResponse extends \Google\Collection
{
  protected $collection_key = 'rows';
  /**
   * @var bool
   */
  public $cacheHit;
  /**
   * @var string
   */
  public $creationTime;
  protected $dmlStatsType = DmlStatistics::class;
  protected $dmlStatsDataType = '';
  /**
   * @var string
   */
  public $endTime;
  protected $errorsType = ErrorProto::class;
  protected $errorsDataType = 'array';
  /**
   * @var bool
   */
  public $jobComplete;
  protected $jobCreationReasonType = JobCreationReason::class;
  protected $jobCreationReasonDataType = '';
  protected $jobReferenceType = JobReference::class;
  protected $jobReferenceDataType = '';
  /**
   * @var string
   */
  public $kind;
  /**
   * @var string
   */
  public $location;
  /**
   * @var string
   */
  public $numDmlAffectedRows;
  /**
   * @var string
   */
  public $pageToken;
  /**
   * @var string
   */
  public $queryId;
  protected $rowsType = TableRow::class;
  protected $rowsDataType = 'array';
  protected $schemaType = TableSchema::class;
  protected $schemaDataType = '';
  protected $sessionInfoType = SessionInfo::class;
  protected $sessionInfoDataType = '';
  /**
   * @var string
   */
  public $startTime;
  /**
   * @var string
   */
  public $totalBytesBilled;
  /**
   * @var string
   */
  public $totalBytesProcessed;
  /**
   * @var string
   */
  public $totalRows;
  /**
   * @var string
   */
  public $totalSlotMs;

  /**
   * @param bool
   */
  public function setCacheHit($cacheHit)
  {
    $this->cacheHit = $cacheHit;
  }
  /**
   * @return bool
   */
  public function getCacheHit()
  {
    return $this->cacheHit;
  }
  /**
   * @param string
   */
  public function setCreationTime($creationTime)
  {
    $this->creationTime = $creationTime;
  }
  /**
   * @return string
   */
  public function getCreationTime()
  {
    return $this->creationTime;
  }
  /**
   * @param DmlStatistics
   */
  public function setDmlStats(DmlStatistics $dmlStats)
  {
    $this->dmlStats = $dmlStats;
  }
  /**
   * @return DmlStatistics
   */
  public function getDmlStats()
  {
    return $this->dmlStats;
  }
  /**
   * @param string
   */
  public function setEndTime($endTime)
  {
    $this->endTime = $endTime;
  }
  /**
   * @return string
   */
  public function getEndTime()
  {
    return $this->endTime;
  }
  /**
   * @param ErrorProto[]
   */
  public function setErrors($errors)
  {
    $this->errors = $errors;
  }
  /**
   * @return ErrorProto[]
   */
  public function getErrors()
  {
    return $this->errors;
  }
  /**
   * @param bool
   */
  public function setJobComplete($jobComplete)
  {
    $this->jobComplete = $jobComplete;
  }
  /**
   * @return bool
   */
  public function getJobComplete()
  {
    return $this->jobComplete;
  }
  /**
   * @param JobCreationReason
   */
  public function setJobCreationReason(JobCreationReason $jobCreationReason)
  {
    $this->jobCreationReason = $jobCreationReason;
  }
  /**
   * @return JobCreationReason
   */
  public function getJobCreationReason()
  {
    return $this->jobCreationReason;
  }
  /**
   * @param JobReference
   */
  public function setJobReference(JobReference $jobReference)
  {
    $this->jobReference = $jobReference;
  }
  /**
   * @return JobReference
   */
  public function getJobReference()
  {
    return $this->jobReference;
  }
  /**
   * @param string
   */
  public function setKind($kind)
  {
    $this->kind = $kind;
  }
  /**
   * @return string
   */
  public function getKind()
  {
    return $this->kind;
  }
  /**
   * @param string
   */
  public function setLocation($location)
  {
    $this->location = $location;
  }
  /**
   * @return string
   */
  public function getLocation()
  {
    return $this->location;
  }
  /**
   * @param string
   */
  public function setNumDmlAffectedRows($numDmlAffectedRows)
  {
    $this->numDmlAffectedRows = $numDmlAffectedRows;
  }
  /**
   * @return string
   */
  public function getNumDmlAffectedRows()
  {
    return $this->numDmlAffectedRows;
  }
  /**
   * @param string
   */
  public function setPageToken($pageToken)
  {
    $this->pageToken = $pageToken;
  }
  /**
   * @return string
   */
  public function getPageToken()
  {
    return $this->pageToken;
  }
  /**
   * @param string
   */
  public function setQueryId($queryId)
  {
    $this->queryId = $queryId;
  }
  /**
   * @return string
   */
  public function getQueryId()
  {
    return $this->queryId;
  }
  /**
   * @param TableRow[]
   */
  public function setRows($rows)
  {
    $this->rows = $rows;
  }
  /**
   * @return TableRow[]
   */
  public function getRows()
  {
    return $this->rows;
  }
  /**
   * @param TableSchema
   */
  public function setSchema(TableSchema $schema)
  {
    $this->schema = $schema;
  }
  /**
   * @return TableSchema
   */
  public function getSchema()
  {
    return $this->schema;
  }
  /**
   * @param SessionInfo
   */
  public function setSessionInfo(SessionInfo $sessionInfo)
  {
    $this->sessionInfo = $sessionInfo;
  }
  /**
   * @return SessionInfo
   */
  public function getSessionInfo()
  {
    return $this->sessionInfo;
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
  public function setTotalBytesBilled($totalBytesBilled)
  {
    $this->totalBytesBilled = $totalBytesBilled;
  }
  /**
   * @return string
   */
  public function getTotalBytesBilled()
  {
    return $this->totalBytesBilled;
  }
  /**
   * @param string
   */
  public function setTotalBytesProcessed($totalBytesProcessed)
  {
    $this->totalBytesProcessed = $totalBytesProcessed;
  }
  /**
   * @return string
   */
  public function getTotalBytesProcessed()
  {
    return $this->totalBytesProcessed;
  }
  /**
   * @param string
   */
  public function setTotalRows($totalRows)
  {
    $this->totalRows = $totalRows;
  }
  /**
   * @return string
   */
  public function getTotalRows()
  {
    return $this->totalRows;
  }
  /**
   * @param string
   */
  public function setTotalSlotMs($totalSlotMs)
  {
    $this->totalSlotMs = $totalSlotMs;
  }
  /**
   * @return string
   */
  public function getTotalSlotMs()
  {
    return $this->totalSlotMs;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(QueryResponse::class, 'Google_Service_Bigquery_QueryResponse');
