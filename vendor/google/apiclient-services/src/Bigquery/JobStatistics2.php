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

class JobStatistics2 extends \Google\Collection
{
  protected $collection_key = 'undeclaredQueryParameters';
  protected $biEngineStatisticsType = BiEngineStatistics::class;
  protected $biEngineStatisticsDataType = '';
  /**
   * @var int
   */
  public $billingTier;
  /**
   * @var bool
   */
  public $cacheHit;
  protected $dclTargetDatasetType = DatasetReference::class;
  protected $dclTargetDatasetDataType = '';
  protected $dclTargetTableType = TableReference::class;
  protected $dclTargetTableDataType = '';
  protected $dclTargetViewType = TableReference::class;
  protected $dclTargetViewDataType = '';
  /**
   * @var string
   */
  public $ddlAffectedRowAccessPolicyCount;
  protected $ddlDestinationTableType = TableReference::class;
  protected $ddlDestinationTableDataType = '';
  /**
   * @var string
   */
  public $ddlOperationPerformed;
  protected $ddlTargetDatasetType = DatasetReference::class;
  protected $ddlTargetDatasetDataType = '';
  protected $ddlTargetRoutineType = RoutineReference::class;
  protected $ddlTargetRoutineDataType = '';
  protected $ddlTargetRowAccessPolicyType = RowAccessPolicyReference::class;
  protected $ddlTargetRowAccessPolicyDataType = '';
  protected $ddlTargetTableType = TableReference::class;
  protected $ddlTargetTableDataType = '';
  protected $dmlStatsType = DmlStatistics::class;
  protected $dmlStatsDataType = '';
  /**
   * @var string
   */
  public $estimatedBytesProcessed;
  protected $exportDataStatisticsType = ExportDataStatistics::class;
  protected $exportDataStatisticsDataType = '';
  protected $externalServiceCostsType = ExternalServiceCost::class;
  protected $externalServiceCostsDataType = 'array';
  protected $loadQueryStatisticsType = LoadQueryStatistics::class;
  protected $loadQueryStatisticsDataType = '';
  protected $materializedViewStatisticsType = MaterializedViewStatistics::class;
  protected $materializedViewStatisticsDataType = '';
  protected $metadataCacheStatisticsType = MetadataCacheStatistics::class;
  protected $metadataCacheStatisticsDataType = '';
  protected $mlStatisticsType = MlStatistics::class;
  protected $mlStatisticsDataType = '';
  protected $modelTrainingType = BigQueryModelTraining::class;
  protected $modelTrainingDataType = '';
  /**
   * @var int
   */
  public $modelTrainingCurrentIteration;
  /**
   * @var string
   */
  public $modelTrainingExpectedTotalIteration;
  /**
   * @var string
   */
  public $numDmlAffectedRows;
  protected $performanceInsightsType = PerformanceInsights::class;
  protected $performanceInsightsDataType = '';
  protected $queryInfoType = QueryInfo::class;
  protected $queryInfoDataType = '';
  protected $queryPlanType = ExplainQueryStage::class;
  protected $queryPlanDataType = 'array';
  protected $referencedRoutinesType = RoutineReference::class;
  protected $referencedRoutinesDataType = 'array';
  protected $referencedTablesType = TableReference::class;
  protected $referencedTablesDataType = 'array';
  protected $reservationUsageType = JobStatistics2ReservationUsage::class;
  protected $reservationUsageDataType = 'array';
  protected $schemaType = TableSchema::class;
  protected $schemaDataType = '';
  protected $searchStatisticsType = SearchStatistics::class;
  protected $searchStatisticsDataType = '';
  protected $sparkStatisticsType = SparkStatistics::class;
  protected $sparkStatisticsDataType = '';
  /**
   * @var string
   */
  public $statementType;
  protected $timelineType = QueryTimelineSample::class;
  protected $timelineDataType = 'array';
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
  public $totalBytesProcessedAccuracy;
  /**
   * @var string
   */
  public $totalPartitionsProcessed;
  /**
   * @var string
   */
  public $totalSlotMs;
  /**
   * @var string
   */
  public $transferredBytes;
  protected $undeclaredQueryParametersType = QueryParameter::class;
  protected $undeclaredQueryParametersDataType = 'array';
  protected $vectorSearchStatisticsType = VectorSearchStatistics::class;
  protected $vectorSearchStatisticsDataType = '';

  /**
   * @param BiEngineStatistics
   */
  public function setBiEngineStatistics(BiEngineStatistics $biEngineStatistics)
  {
    $this->biEngineStatistics = $biEngineStatistics;
  }
  /**
   * @return BiEngineStatistics
   */
  public function getBiEngineStatistics()
  {
    return $this->biEngineStatistics;
  }
  /**
   * @param int
   */
  public function setBillingTier($billingTier)
  {
    $this->billingTier = $billingTier;
  }
  /**
   * @return int
   */
  public function getBillingTier()
  {
    return $this->billingTier;
  }
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
   * @param DatasetReference
   */
  public function setDclTargetDataset(DatasetReference $dclTargetDataset)
  {
    $this->dclTargetDataset = $dclTargetDataset;
  }
  /**
   * @return DatasetReference
   */
  public function getDclTargetDataset()
  {
    return $this->dclTargetDataset;
  }
  /**
   * @param TableReference
   */
  public function setDclTargetTable(TableReference $dclTargetTable)
  {
    $this->dclTargetTable = $dclTargetTable;
  }
  /**
   * @return TableReference
   */
  public function getDclTargetTable()
  {
    return $this->dclTargetTable;
  }
  /**
   * @param TableReference
   */
  public function setDclTargetView(TableReference $dclTargetView)
  {
    $this->dclTargetView = $dclTargetView;
  }
  /**
   * @return TableReference
   */
  public function getDclTargetView()
  {
    return $this->dclTargetView;
  }
  /**
   * @param string
   */
  public function setDdlAffectedRowAccessPolicyCount($ddlAffectedRowAccessPolicyCount)
  {
    $this->ddlAffectedRowAccessPolicyCount = $ddlAffectedRowAccessPolicyCount;
  }
  /**
   * @return string
   */
  public function getDdlAffectedRowAccessPolicyCount()
  {
    return $this->ddlAffectedRowAccessPolicyCount;
  }
  /**
   * @param TableReference
   */
  public function setDdlDestinationTable(TableReference $ddlDestinationTable)
  {
    $this->ddlDestinationTable = $ddlDestinationTable;
  }
  /**
   * @return TableReference
   */
  public function getDdlDestinationTable()
  {
    return $this->ddlDestinationTable;
  }
  /**
   * @param string
   */
  public function setDdlOperationPerformed($ddlOperationPerformed)
  {
    $this->ddlOperationPerformed = $ddlOperationPerformed;
  }
  /**
   * @return string
   */
  public function getDdlOperationPerformed()
  {
    return $this->ddlOperationPerformed;
  }
  /**
   * @param DatasetReference
   */
  public function setDdlTargetDataset(DatasetReference $ddlTargetDataset)
  {
    $this->ddlTargetDataset = $ddlTargetDataset;
  }
  /**
   * @return DatasetReference
   */
  public function getDdlTargetDataset()
  {
    return $this->ddlTargetDataset;
  }
  /**
   * @param RoutineReference
   */
  public function setDdlTargetRoutine(RoutineReference $ddlTargetRoutine)
  {
    $this->ddlTargetRoutine = $ddlTargetRoutine;
  }
  /**
   * @return RoutineReference
   */
  public function getDdlTargetRoutine()
  {
    return $this->ddlTargetRoutine;
  }
  /**
   * @param RowAccessPolicyReference
   */
  public function setDdlTargetRowAccessPolicy(RowAccessPolicyReference $ddlTargetRowAccessPolicy)
  {
    $this->ddlTargetRowAccessPolicy = $ddlTargetRowAccessPolicy;
  }
  /**
   * @return RowAccessPolicyReference
   */
  public function getDdlTargetRowAccessPolicy()
  {
    return $this->ddlTargetRowAccessPolicy;
  }
  /**
   * @param TableReference
   */
  public function setDdlTargetTable(TableReference $ddlTargetTable)
  {
    $this->ddlTargetTable = $ddlTargetTable;
  }
  /**
   * @return TableReference
   */
  public function getDdlTargetTable()
  {
    return $this->ddlTargetTable;
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
  public function setEstimatedBytesProcessed($estimatedBytesProcessed)
  {
    $this->estimatedBytesProcessed = $estimatedBytesProcessed;
  }
  /**
   * @return string
   */
  public function getEstimatedBytesProcessed()
  {
    return $this->estimatedBytesProcessed;
  }
  /**
   * @param ExportDataStatistics
   */
  public function setExportDataStatistics(ExportDataStatistics $exportDataStatistics)
  {
    $this->exportDataStatistics = $exportDataStatistics;
  }
  /**
   * @return ExportDataStatistics
   */
  public function getExportDataStatistics()
  {
    return $this->exportDataStatistics;
  }
  /**
   * @param ExternalServiceCost[]
   */
  public function setExternalServiceCosts($externalServiceCosts)
  {
    $this->externalServiceCosts = $externalServiceCosts;
  }
  /**
   * @return ExternalServiceCost[]
   */
  public function getExternalServiceCosts()
  {
    return $this->externalServiceCosts;
  }
  /**
   * @param LoadQueryStatistics
   */
  public function setLoadQueryStatistics(LoadQueryStatistics $loadQueryStatistics)
  {
    $this->loadQueryStatistics = $loadQueryStatistics;
  }
  /**
   * @return LoadQueryStatistics
   */
  public function getLoadQueryStatistics()
  {
    return $this->loadQueryStatistics;
  }
  /**
   * @param MaterializedViewStatistics
   */
  public function setMaterializedViewStatistics(MaterializedViewStatistics $materializedViewStatistics)
  {
    $this->materializedViewStatistics = $materializedViewStatistics;
  }
  /**
   * @return MaterializedViewStatistics
   */
  public function getMaterializedViewStatistics()
  {
    return $this->materializedViewStatistics;
  }
  /**
   * @param MetadataCacheStatistics
   */
  public function setMetadataCacheStatistics(MetadataCacheStatistics $metadataCacheStatistics)
  {
    $this->metadataCacheStatistics = $metadataCacheStatistics;
  }
  /**
   * @return MetadataCacheStatistics
   */
  public function getMetadataCacheStatistics()
  {
    return $this->metadataCacheStatistics;
  }
  /**
   * @param MlStatistics
   */
  public function setMlStatistics(MlStatistics $mlStatistics)
  {
    $this->mlStatistics = $mlStatistics;
  }
  /**
   * @return MlStatistics
   */
  public function getMlStatistics()
  {
    return $this->mlStatistics;
  }
  /**
   * @param BigQueryModelTraining
   */
  public function setModelTraining(BigQueryModelTraining $modelTraining)
  {
    $this->modelTraining = $modelTraining;
  }
  /**
   * @return BigQueryModelTraining
   */
  public function getModelTraining()
  {
    return $this->modelTraining;
  }
  /**
   * @param int
   */
  public function setModelTrainingCurrentIteration($modelTrainingCurrentIteration)
  {
    $this->modelTrainingCurrentIteration = $modelTrainingCurrentIteration;
  }
  /**
   * @return int
   */
  public function getModelTrainingCurrentIteration()
  {
    return $this->modelTrainingCurrentIteration;
  }
  /**
   * @param string
   */
  public function setModelTrainingExpectedTotalIteration($modelTrainingExpectedTotalIteration)
  {
    $this->modelTrainingExpectedTotalIteration = $modelTrainingExpectedTotalIteration;
  }
  /**
   * @return string
   */
  public function getModelTrainingExpectedTotalIteration()
  {
    return $this->modelTrainingExpectedTotalIteration;
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
   * @param PerformanceInsights
   */
  public function setPerformanceInsights(PerformanceInsights $performanceInsights)
  {
    $this->performanceInsights = $performanceInsights;
  }
  /**
   * @return PerformanceInsights
   */
  public function getPerformanceInsights()
  {
    return $this->performanceInsights;
  }
  /**
   * @param QueryInfo
   */
  public function setQueryInfo(QueryInfo $queryInfo)
  {
    $this->queryInfo = $queryInfo;
  }
  /**
   * @return QueryInfo
   */
  public function getQueryInfo()
  {
    return $this->queryInfo;
  }
  /**
   * @param ExplainQueryStage[]
   */
  public function setQueryPlan($queryPlan)
  {
    $this->queryPlan = $queryPlan;
  }
  /**
   * @return ExplainQueryStage[]
   */
  public function getQueryPlan()
  {
    return $this->queryPlan;
  }
  /**
   * @param RoutineReference[]
   */
  public function setReferencedRoutines($referencedRoutines)
  {
    $this->referencedRoutines = $referencedRoutines;
  }
  /**
   * @return RoutineReference[]
   */
  public function getReferencedRoutines()
  {
    return $this->referencedRoutines;
  }
  /**
   * @param TableReference[]
   */
  public function setReferencedTables($referencedTables)
  {
    $this->referencedTables = $referencedTables;
  }
  /**
   * @return TableReference[]
   */
  public function getReferencedTables()
  {
    return $this->referencedTables;
  }
  /**
   * @param JobStatistics2ReservationUsage[]
   */
  public function setReservationUsage($reservationUsage)
  {
    $this->reservationUsage = $reservationUsage;
  }
  /**
   * @return JobStatistics2ReservationUsage[]
   */
  public function getReservationUsage()
  {
    return $this->reservationUsage;
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
   * @param SearchStatistics
   */
  public function setSearchStatistics(SearchStatistics $searchStatistics)
  {
    $this->searchStatistics = $searchStatistics;
  }
  /**
   * @return SearchStatistics
   */
  public function getSearchStatistics()
  {
    return $this->searchStatistics;
  }
  /**
   * @param SparkStatistics
   */
  public function setSparkStatistics(SparkStatistics $sparkStatistics)
  {
    $this->sparkStatistics = $sparkStatistics;
  }
  /**
   * @return SparkStatistics
   */
  public function getSparkStatistics()
  {
    return $this->sparkStatistics;
  }
  /**
   * @param string
   */
  public function setStatementType($statementType)
  {
    $this->statementType = $statementType;
  }
  /**
   * @return string
   */
  public function getStatementType()
  {
    return $this->statementType;
  }
  /**
   * @param QueryTimelineSample[]
   */
  public function setTimeline($timeline)
  {
    $this->timeline = $timeline;
  }
  /**
   * @return QueryTimelineSample[]
   */
  public function getTimeline()
  {
    return $this->timeline;
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
  public function setTotalBytesProcessedAccuracy($totalBytesProcessedAccuracy)
  {
    $this->totalBytesProcessedAccuracy = $totalBytesProcessedAccuracy;
  }
  /**
   * @return string
   */
  public function getTotalBytesProcessedAccuracy()
  {
    return $this->totalBytesProcessedAccuracy;
  }
  /**
   * @param string
   */
  public function setTotalPartitionsProcessed($totalPartitionsProcessed)
  {
    $this->totalPartitionsProcessed = $totalPartitionsProcessed;
  }
  /**
   * @return string
   */
  public function getTotalPartitionsProcessed()
  {
    return $this->totalPartitionsProcessed;
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
  /**
   * @param string
   */
  public function setTransferredBytes($transferredBytes)
  {
    $this->transferredBytes = $transferredBytes;
  }
  /**
   * @return string
   */
  public function getTransferredBytes()
  {
    return $this->transferredBytes;
  }
  /**
   * @param QueryParameter[]
   */
  public function setUndeclaredQueryParameters($undeclaredQueryParameters)
  {
    $this->undeclaredQueryParameters = $undeclaredQueryParameters;
  }
  /**
   * @return QueryParameter[]
   */
  public function getUndeclaredQueryParameters()
  {
    return $this->undeclaredQueryParameters;
  }
  /**
   * @param VectorSearchStatistics
   */
  public function setVectorSearchStatistics(VectorSearchStatistics $vectorSearchStatistics)
  {
    $this->vectorSearchStatistics = $vectorSearchStatistics;
  }
  /**
   * @return VectorSearchStatistics
   */
  public function getVectorSearchStatistics()
  {
    return $this->vectorSearchStatistics;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(JobStatistics2::class, 'Google_Service_Bigquery_JobStatistics2');
