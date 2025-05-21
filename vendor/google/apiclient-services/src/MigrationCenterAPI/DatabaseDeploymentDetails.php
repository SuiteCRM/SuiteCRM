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

namespace Google\Service\MigrationCenterAPI;

class DatabaseDeploymentDetails extends \Google\Model
{
  protected $aggregatedStatsType = DatabaseDeploymentDetailsAggregatedStats::class;
  protected $aggregatedStatsDataType = '';
  protected $awsRdsType = AwsRds::class;
  protected $awsRdsDataType = '';
  /**
   * @var string
   */
  public $edition;
  /**
   * @var string
   */
  public $generatedId;
  /**
   * @var string
   */
  public $manualUniqueId;
  protected $mysqlType = MysqlDatabaseDeployment::class;
  protected $mysqlDataType = '';
  protected $postgresqlType = PostgreSqlDatabaseDeployment::class;
  protected $postgresqlDataType = '';
  protected $sqlServerType = SqlServerDatabaseDeployment::class;
  protected $sqlServerDataType = '';
  protected $topologyType = DatabaseDeploymentTopology::class;
  protected $topologyDataType = '';
  /**
   * @var string
   */
  public $version;

  /**
   * @param DatabaseDeploymentDetailsAggregatedStats
   */
  public function setAggregatedStats(DatabaseDeploymentDetailsAggregatedStats $aggregatedStats)
  {
    $this->aggregatedStats = $aggregatedStats;
  }
  /**
   * @return DatabaseDeploymentDetailsAggregatedStats
   */
  public function getAggregatedStats()
  {
    return $this->aggregatedStats;
  }
  /**
   * @param AwsRds
   */
  public function setAwsRds(AwsRds $awsRds)
  {
    $this->awsRds = $awsRds;
  }
  /**
   * @return AwsRds
   */
  public function getAwsRds()
  {
    return $this->awsRds;
  }
  /**
   * @param string
   */
  public function setEdition($edition)
  {
    $this->edition = $edition;
  }
  /**
   * @return string
   */
  public function getEdition()
  {
    return $this->edition;
  }
  /**
   * @param string
   */
  public function setGeneratedId($generatedId)
  {
    $this->generatedId = $generatedId;
  }
  /**
   * @return string
   */
  public function getGeneratedId()
  {
    return $this->generatedId;
  }
  /**
   * @param string
   */
  public function setManualUniqueId($manualUniqueId)
  {
    $this->manualUniqueId = $manualUniqueId;
  }
  /**
   * @return string
   */
  public function getManualUniqueId()
  {
    return $this->manualUniqueId;
  }
  /**
   * @param MysqlDatabaseDeployment
   */
  public function setMysql(MysqlDatabaseDeployment $mysql)
  {
    $this->mysql = $mysql;
  }
  /**
   * @return MysqlDatabaseDeployment
   */
  public function getMysql()
  {
    return $this->mysql;
  }
  /**
   * @param PostgreSqlDatabaseDeployment
   */
  public function setPostgresql(PostgreSqlDatabaseDeployment $postgresql)
  {
    $this->postgresql = $postgresql;
  }
  /**
   * @return PostgreSqlDatabaseDeployment
   */
  public function getPostgresql()
  {
    return $this->postgresql;
  }
  /**
   * @param SqlServerDatabaseDeployment
   */
  public function setSqlServer(SqlServerDatabaseDeployment $sqlServer)
  {
    $this->sqlServer = $sqlServer;
  }
  /**
   * @return SqlServerDatabaseDeployment
   */
  public function getSqlServer()
  {
    return $this->sqlServer;
  }
  /**
   * @param DatabaseDeploymentTopology
   */
  public function setTopology(DatabaseDeploymentTopology $topology)
  {
    $this->topology = $topology;
  }
  /**
   * @return DatabaseDeploymentTopology
   */
  public function getTopology()
  {
    return $this->topology;
  }
  /**
   * @param string
   */
  public function setVersion($version)
  {
    $this->version = $version;
  }
  /**
   * @return string
   */
  public function getVersion()
  {
    return $this->version;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DatabaseDeploymentDetails::class, 'Google_Service_MigrationCenterAPI_DatabaseDeploymentDetails');
