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

class AssetFrame extends \Google\Collection
{
  protected $collection_key = 'performanceSamples';
  /**
   * @var string[]
   */
  public $attributes;
  /**
   * @var string
   */
  public $collectionType;
  protected $databaseDeploymentDetailsType = DatabaseDeploymentDetails::class;
  protected $databaseDeploymentDetailsDataType = '';
  protected $databaseDetailsType = DatabaseDetails::class;
  protected $databaseDetailsDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  protected $machineDetailsType = MachineDetails::class;
  protected $machineDetailsDataType = '';
  protected $performanceSamplesType = PerformanceSample::class;
  protected $performanceSamplesDataType = 'array';
  /**
   * @var string
   */
  public $reportTime;
  /**
   * @var string
   */
  public $traceToken;

  /**
   * @param string[]
   */
  public function setAttributes($attributes)
  {
    $this->attributes = $attributes;
  }
  /**
   * @return string[]
   */
  public function getAttributes()
  {
    return $this->attributes;
  }
  /**
   * @param string
   */
  public function setCollectionType($collectionType)
  {
    $this->collectionType = $collectionType;
  }
  /**
   * @return string
   */
  public function getCollectionType()
  {
    return $this->collectionType;
  }
  /**
   * @param DatabaseDeploymentDetails
   */
  public function setDatabaseDeploymentDetails(DatabaseDeploymentDetails $databaseDeploymentDetails)
  {
    $this->databaseDeploymentDetails = $databaseDeploymentDetails;
  }
  /**
   * @return DatabaseDeploymentDetails
   */
  public function getDatabaseDeploymentDetails()
  {
    return $this->databaseDeploymentDetails;
  }
  /**
   * @param DatabaseDetails
   */
  public function setDatabaseDetails(DatabaseDetails $databaseDetails)
  {
    $this->databaseDetails = $databaseDetails;
  }
  /**
   * @return DatabaseDetails
   */
  public function getDatabaseDetails()
  {
    return $this->databaseDetails;
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
   * @param MachineDetails
   */
  public function setMachineDetails(MachineDetails $machineDetails)
  {
    $this->machineDetails = $machineDetails;
  }
  /**
   * @return MachineDetails
   */
  public function getMachineDetails()
  {
    return $this->machineDetails;
  }
  /**
   * @param PerformanceSample[]
   */
  public function setPerformanceSamples($performanceSamples)
  {
    $this->performanceSamples = $performanceSamples;
  }
  /**
   * @return PerformanceSample[]
   */
  public function getPerformanceSamples()
  {
    return $this->performanceSamples;
  }
  /**
   * @param string
   */
  public function setReportTime($reportTime)
  {
    $this->reportTime = $reportTime;
  }
  /**
   * @return string
   */
  public function getReportTime()
  {
    return $this->reportTime;
  }
  /**
   * @param string
   */
  public function setTraceToken($traceToken)
  {
    $this->traceToken = $traceToken;
  }
  /**
   * @return string
   */
  public function getTraceToken()
  {
    return $this->traceToken;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AssetFrame::class, 'Google_Service_MigrationCenterAPI_AssetFrame');
