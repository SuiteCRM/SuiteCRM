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

namespace Google\Service\CloudAlloyDBAdmin;

class ClusterUpgradeDetails extends \Google\Collection
{
  protected $collection_key = 'stageInfo';
  /**
   * @var string
   */
  public $clusterType;
  /**
   * @var string
   */
  public $databaseVersion;
  protected $instanceUpgradeDetailsType = InstanceUpgradeDetails::class;
  protected $instanceUpgradeDetailsDataType = 'array';
  /**
   * @var string
   */
  public $name;
  protected $stageInfoType = StageInfo::class;
  protected $stageInfoDataType = 'array';
  /**
   * @var string
   */
  public $upgradeStatus;

  /**
   * @param string
   */
  public function setClusterType($clusterType)
  {
    $this->clusterType = $clusterType;
  }
  /**
   * @return string
   */
  public function getClusterType()
  {
    return $this->clusterType;
  }
  /**
   * @param string
   */
  public function setDatabaseVersion($databaseVersion)
  {
    $this->databaseVersion = $databaseVersion;
  }
  /**
   * @return string
   */
  public function getDatabaseVersion()
  {
    return $this->databaseVersion;
  }
  /**
   * @param InstanceUpgradeDetails[]
   */
  public function setInstanceUpgradeDetails($instanceUpgradeDetails)
  {
    $this->instanceUpgradeDetails = $instanceUpgradeDetails;
  }
  /**
   * @return InstanceUpgradeDetails[]
   */
  public function getInstanceUpgradeDetails()
  {
    return $this->instanceUpgradeDetails;
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
   * @param StageInfo[]
   */
  public function setStageInfo($stageInfo)
  {
    $this->stageInfo = $stageInfo;
  }
  /**
   * @return StageInfo[]
   */
  public function getStageInfo()
  {
    return $this->stageInfo;
  }
  /**
   * @param string
   */
  public function setUpgradeStatus($upgradeStatus)
  {
    $this->upgradeStatus = $upgradeStatus;
  }
  /**
   * @return string
   */
  public function getUpgradeStatus()
  {
    return $this->upgradeStatus;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ClusterUpgradeDetails::class, 'Google_Service_CloudAlloyDBAdmin_ClusterUpgradeDetails');
