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

class RestoreClusterRequest extends \Google\Model
{
  protected $backupSourceType = BackupSource::class;
  protected $backupSourceDataType = '';
  protected $clusterType = Cluster::class;
  protected $clusterDataType = '';
  /**
   * @var string
   */
  public $clusterId;
  protected $continuousBackupSourceType = ContinuousBackupSource::class;
  protected $continuousBackupSourceDataType = '';
  /**
   * @var string
   */
  public $requestId;
  /**
   * @var bool
   */
  public $validateOnly;

  /**
   * @param BackupSource
   */
  public function setBackupSource(BackupSource $backupSource)
  {
    $this->backupSource = $backupSource;
  }
  /**
   * @return BackupSource
   */
  public function getBackupSource()
  {
    return $this->backupSource;
  }
  /**
   * @param Cluster
   */
  public function setCluster(Cluster $cluster)
  {
    $this->cluster = $cluster;
  }
  /**
   * @return Cluster
   */
  public function getCluster()
  {
    return $this->cluster;
  }
  /**
   * @param string
   */
  public function setClusterId($clusterId)
  {
    $this->clusterId = $clusterId;
  }
  /**
   * @return string
   */
  public function getClusterId()
  {
    return $this->clusterId;
  }
  /**
   * @param ContinuousBackupSource
   */
  public function setContinuousBackupSource(ContinuousBackupSource $continuousBackupSource)
  {
    $this->continuousBackupSource = $continuousBackupSource;
  }
  /**
   * @return ContinuousBackupSource
   */
  public function getContinuousBackupSource()
  {
    return $this->continuousBackupSource;
  }
  /**
   * @param string
   */
  public function setRequestId($requestId)
  {
    $this->requestId = $requestId;
  }
  /**
   * @return string
   */
  public function getRequestId()
  {
    return $this->requestId;
  }
  /**
   * @param bool
   */
  public function setValidateOnly($validateOnly)
  {
    $this->validateOnly = $validateOnly;
  }
  /**
   * @return bool
   */
  public function getValidateOnly()
  {
    return $this->validateOnly;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RestoreClusterRequest::class, 'Google_Service_CloudAlloyDBAdmin_RestoreClusterRequest');
